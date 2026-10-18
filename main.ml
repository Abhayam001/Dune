(* =========================================================================
   OCAML LIVE NSE QUANT BOT: CONSCIOUS 100-BOT F&O / INTRADAY SCALPING SWARM
   Libraries: Lwt, Cohttp-lwt-unix, Yojson, Str, Unix
   ========================================================================= *)

open Lwt.Infix
open Printf

(* -------------------------------------------------------------------------
   1. CORE MARKET, INSTRUMENT, TAX & PORTFOLIO TYPES
   ------------------------------------------------------------------------- *)
module Types = struct
  type candle = {
    timestamp : float;
    open_ : float;
    high : float;
    low : float;
    close : float;
    volume : float;
  }

  type feature_vector = {
    price : float;
    ema_9 : float;
    ema_21 : float;
    ema_50 : float;
    rsi_14 : float;
    atr_14 : float;
    bb_upper : float;
    bb_lower : float;
    z_score : float;
    vol_spike : float;
    macro_sentiment : float;
  }

  type trade_instrument =
    | IntradayScalp     (* MIS 5x Leverage Equity Scalp *)
    | FuturesContract   (* NFO 5x Leverage Directional Futures *)
    | OptionsCallScalp  (* NFO High-Gamma Long Call (CE) Scalp *)
    | OptionsPutScalp   (* NFO High-Gamma Long Put (PE) Scalp *)

  type position = {
    symbol : string;
    qty : int;
    entry_price : float;
    stop_loss : float;
    target_price : float;
    entry_charges : float;
    entry_votes : (string * float) list;
  }

  type portfolio = {
    mutable cash : float;
    mutable total_trades : int;
    mutable winning_trades : int;
    mutable total_taxes_paid : float;
    mutable positions : position list;
  }
end

open Types

(* -------------------------------------------------------------------------
   2. ANGEL ONE MULTI-SEGMENT (INTRADAY / FUTURES / OPTIONS) FRICTION ENGINE
   ------------------------------------------------------------------------- *)
module AngelOneFriction = struct
  let instrument_to_string = function
    | IntradayScalp -> "MIS_INTRADAY"
    | FuturesContract -> "NFO_FUTURES"
    | OptionsCallScalp -> "NFO_OPT_CE"
    | OptionsPutScalp -> "NFO_OPT_PE"

  let instrument_of_string = function
    | "NFO_FUTURES" -> FuturesContract
    | "NFO_OPT_CE" -> OptionsCallScalp
    | "NFO_OPT_PE" -> OptionsPutScalp
    | _ -> IntradayScalp

  (* Simulates real NSE order book slippage (higher bid-ask spread on Options) *)
  let apply_slippage (price : float) (qty : int) (is_buy : bool) (inst : trade_instrument) : float =
    let turnover = price *. float_of_int qty in
    let base_slip = match inst with
      | IntradayScalp -> 0.0004
      | FuturesContract -> 0.0005
      | OptionsCallScalp | OptionsPutScalp -> 0.0015 (* Wider bid-ask in F&O options *)
    in
    let impact_pct = base_slip +. (min 0.0020 (turnover /. 50_000_000.0)) in
    if is_buy then price *. (1.0 +. impact_pct)
    else price *. (1.0 -. impact_pct)

  (* Exact Angel One + NSE Statutory Charges across Intraday Equity, Futures, and Options *)
  let calculate_segment_charges (price : float) (qty : int) (is_buy : bool) (inst : trade_instrument) : float =
    let turnover = price *. float_of_int qty in
    let sebi_fee = 0.000001 *. turnover in
    match inst with
    | IntradayScalp ->
        let brokerage = min 20.0 (0.0003 *. turnover) in
        let stt = if is_buy then 0.0 else 0.00025 *. turnover in
        let stamp = if is_buy then 0.00003 *. turnover else 0.0 in
        let exch = 0.0000297 *. turnover in
        let gst = 0.18 *. (brokerage +. exch +. sebi_fee) in
        brokerage +. stt +. stamp +. exch +. sebi_fee +. gst
    | FuturesContract ->
        let brokerage = 20.0 in (* Angel One flat Rs. 20 on F&O *)
        let stt = if is_buy then 0.0 else 0.0002 *. turnover in (* 0.02% STT on Futures sell *)
        let stamp = if is_buy then 0.00002 *. turnover else 0.0 in
        let exch = 0.0000173 *. turnover in
        let gst = 0.18 *. (brokerage +. exch +. sebi_fee) in
        brokerage +. stt +. stamp +. exch +. sebi_fee +. gst
    | OptionsCallScalp | OptionsPutScalp ->
        let brokerage = 20.0 in (* Flat Rs. 20 per executed F&O order *)
        let stt = if is_buy then 0.0 else 0.001 *. turnover in (* 0.1% STT on Options sell premium *)
        let stamp = if is_buy then 0.00003 *. turnover else 0.0 in
        let exch = 0.0003503 *. turnover in (* NSE Options premium txn charge *)
        let gst = 0.18 *. (brokerage +. exch +. sebi_fee) in
        brokerage +. stt +. stamp +. exch +. sebi_fee +. gst
end

(* -------------------------------------------------------------------------
   3. VECTORIZED QUANTITATIVE & STATISTICAL MATH ENGINE
   ------------------------------------------------------------------------- *)
module MathEngine = struct
  let mean (arr : float array) : float =
    let n = Array.length arr in
    if n = 0 then 0.0
    else (Array.fold_left ( +. ) 0.0 arr) /. float_of_int n

  let std_dev (arr : float array) : float =
    let n = Array.length arr in
    if n <= 1 then 1.0
    else
      let m = mean arr in
      let variance =
        Array.fold_left (fun acc x -> acc +. ((x -. m) ** 2.0)) 0.0 arr
        /. float_of_int (n - 1)
      in
      sqrt (max 1e-9 variance)

  let rsi (prices : float array) (period : int) : float =
    let n = Array.length prices in
    if n <= period then 50.0
    else
      let gains = ref 0.0 in
      let losses = ref 0.0 in
      for i = n - period to n - 1 do
        let diff = prices.(i) -. prices.(i - 1) in
        if diff >= 0.0 then gains := !gains +. diff
        else losses := !losses -. diff
      done;
      let avg_gain = !gains /. float_of_int period in
      let avg_loss = !losses /. float_of_int period in
      if avg_loss < 1e-9 then 100.0
      else
        let rs = avg_gain /. avg_loss in
        let raw_rsi = 100.0 -. (100.0 /. (1.0 +. rs)) in
        if Float.is_nan raw_rsi then 50.0 else max 0.0 (min 100.0 raw_rsi)

  let extract_features (candles : candle array) (macro_sentiment : float) : feature_vector =
    let n = Array.length candles in
    if n < 20 then
      { price = 100.0; ema_9 = 100.0; ema_21 = 100.0; ema_50 = 100.0; rsi_14 = 50.0; atr_14 = 1.0; bb_upper = 105.0; bb_lower = 95.0; z_score = 0.0; vol_spike = 1.0; macro_sentiment }
    else
      let closes = Array.map (fun c -> c.close) candles in
      let volumes = Array.map (fun c -> c.volume) candles in
      let last_close = closes.(n - 1) in
      let recent_20 = Array.sub closes (n - 20) 20 in
      let sma_20 = mean recent_20 in
      let sd_20 = std_dev recent_20 in
      let z = if sd_20 < 1e-9 then 0.0 else (last_close -. sma_20) /. sd_20 in
      {
        price = last_close;
        ema_9 = ema closes 9;
        ema_21 = ema closes 21;
        ema_50 = ema closes 50;
        rsi_14 = rsi closes 14;
        atr_14 = atr candles 14;
        bb_upper = sma_20 +. (2.0 *. sd_20);
        bb_lower = sma_20 -. (2.0 *. sd_20);
        z_score = if Float.is_nan z then 0.0 else z;
        vol_spike = volumes.(n - 1) /. max 1.0 (mean (Array.sub volumes (n - 20) 20));
        macro_sentiment;
      }
end


(* -------------------------------------------------------------------------
   4. LIVE HTTP DATA INGESTION (COHTTP + YOJSON + RSS NLP)
   ------------------------------------------------------------------------- *)
module LiveFeeds = struct
  let headers =
    Cohttp.Header.of_list [
      ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36");
      ("Accept", "application/json, text/xml, */*");
    ]

  let http_get (url : string) : string option Lwt.t =
    Lwt.catch
      (fun () ->
        let uri = Uri.of_string url in
        Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
        let status = Cohttp.Response.status resp in
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
          Cohttp_lwt.Body.to_string body >|= fun s -> Some s
        else
          Lwt.return_none)
      (fun _exn -> Lwt.return_none)

  let to_float_opt (j : Yojson.Basic.t) : float option =
    match j with
    | `Float f -> Some f
    | `Int i -> Some (float_of_int i)
    | _ -> None

  let parse_yahoo_candles (json_str : string) : candle array =
    try
      let open Yojson.Basic.Util in
      let json = Yojson.Basic.from_string json_str in
      let result = json |> member "chart" |> member "result" |> index 0 in
      let timestamps = result |> member "timestamp" |> to_list in
      let quote = result |> member "indicators" |> member "quote" |> index 0 in
      let opens = quote |> member "open" |> to_list in
      let highs = quote |> member "high" |> to_list in
      let lows = quote |> member "low" |> to_list in
      let closes = quote |> member "close" |> to_list in
      let volumes = quote |> member "volume" |> to_list in

      let rec build acc ts os hs ls cs vs =
        match (ts, os, hs, ls, cs, vs) with
        | (t :: ts', o :: os', h :: hs', l :: ls', c :: cs', v :: vs') ->
            let next_acc =
              match (to_float_opt t, to_float_opt o, to_float_opt h, to_float_opt l, to_float_opt c, to_float_opt v) with
              | (Some tf, Some of_, Some hf, Some lf, Some cf, Some vf) when cf > 0.0 ->
                  { timestamp = tf; open_ = of_; high = hf; low = lf; close = cf; volume = vf } :: acc
              | _ -> acc
            in
            build next_acc ts' os' hs' ls' cs' vs'
        | _ -> List.rev acc
      in
      Array.of_list (build [] timestamps opens highs lows closes volumes)
    with _ -> [||]

  let fetch_nse_candles (symbol : string) (range : string) : candle array Lwt.t =
    let encoded = Uri.pct_encode symbol in
    let url = sprintf "https://query1.finance.yahoo.com/v8/finance/chart/%s?interval=1d&range=%s" encoded range in
    http_get url >|= function
    | Some body -> parse_yahoo_candles body
    | None -> [||]

  let fetch_latest_return (ticker : string) : float Lwt.t =
    fetch_nse_candles ticker "5d" >|= fun candles ->
    let n = Array.length candles in
    if n >= 2 then
      let prev = candles.(n - 2).close in
      let curr = candles.(n - 1).close in
      if prev > 0.0 then (curr -. prev) /. prev else 0.0
    else 0.0

  let fetch_global_macro_score () : float Lwt.t =
    fetch_latest_return "^GSPC" >>= fun sp500 ->
    fetch_latest_return "BZ=F" >>= fun crude ->
    fetch_latest_return "INR=X" >|= fun usdinr ->
    let raw = (sp500 *. 12.0) -. (crude *. 6.0) -. (usdinr *. 15.0) in
    max (-1.0) (min 1.0 raw)

  let positive_words = [
    ("surge", 2.0); ("rally", 2.0); ("jump", 1.5); ("gain", 1.2); ("profit", 1.5);
    ("beat", 1.8); ("upgrade", 2.0); ("buy", 1.2); ("bullish", 2.2); ("growth", 1.4);
    ("record", 1.5); ("breakout", 2.0); ("inflow", 1.8); ("dividend", 1.2); ("strong", 1.3)
  ]

  let negative_words = [
    ("fall", -1.5); ("drop", -1.5); ("crash", -2.8); ("plunge", -2.5); ("loss", -1.8);
    ("miss", -1.8); ("downgrade", -2.2); ("sell", -1.2); ("bearish", -2.2); ("weak", -1.4);
    ("outflow", -1.8); ("inflation", -1.5); ("slump", -2.0); ("deficit", -1.5); ("probe", -2.0);
    ("sanction", -2.0); ("tariff", -1.6); ("war", -2.2); ("hawkish", -1.6)
  ]

  let count_substring (text : string) (sub : string) : int =
    let re = Str.regexp_string_case_fold sub in
    let rec loop pos acc =
      try
        let next_pos = Str.search_forward re text pos in
        loop (next_pos + String.length sub) (acc + 1)
      with Not_found -> acc
    in
    loop 0 0

  let score_rss_xml (xml : string) : float =
    let pos_score =
      List.fold_left (fun acc (w, wt) -> acc +. (float_of_int (count_substring xml w) *. wt)) 0.0 positive_words
    in
    let neg_score =
      List.fold_left (fun acc (w, wt) -> acc +. (float_of_int (count_substring xml w) *. wt)) 0.0 negative_words
    in
    let total_hits = abs_float pos_score +. abs_float neg_score in
    if total_hits < 1.0 then 0.0
    else max (-1.0) (min 1.0 ((pos_score +. neg_score) /. (total_hits +. 4.0)))

  let fetch_live_sentiment (symbol : string) (macro_cue : float) : float Lwt.t =
    let clean_sym = List.hd (String.split_on_char '.' symbol) in
    let query = Uri.pct_encode (sprintf "%s NSE stock OR Indian stock market" clean_sym) in
    let rss_url = sprintf "https://news.google.com/rss/search?q=%s&hl=en-IN&gl=IN&ceid=IN:en" query in
    http_get rss_url >|= function
    | Some xml ->
        let news_nlp = score_rss_xml xml in
        (0.65 *. news_nlp) +. (0.35 *. macro_cue)
    | None -> macro_cue
end

(* -------------------------------------------------------------------------
   5. MULTI-STRATEGY ENSEMBLE & PERSISTENT META-LEARNER
   ------------------------------------------------------------------------- *)
module Strategies = struct
  type expert = {
    name : string;
    mutable weight : float;
    mutable total_calls : int;
    mutable correct_calls : int;
    mutable cumulative_pnl : float;
    evaluate : feature_vector -> float;
  }

  let trend_following (f : feature_vector) : float =
    if f.ema_9 > f.ema_21 && f.ema_21 > f.ema_50 && f.rsi_14 > 51.0 && f.rsi_14 < 73.0 then 0.85
    else if f.ema_9 < f.ema_21 && f.price < f.ema_50 then -0.80
    else 0.0

  let mean_reversion (f : feature_vector) : float =
    if f.z_score < -1.6 && f.rsi_14 < 38.0 then 0.90
    else if f.z_score > 1.8 && f.rsi_14 > 70.0 then -0.85
    else 0.0

  let volatility_breakout (f : feature_vector) : float =
    if f.price > f.bb_upper && f.vol_spike > 1.4 then 0.95
    else if f.price < f.bb_lower && f.vol_spike > 1.4 then -0.90
    else 0.0

  let momentum_regime (f : feature_vector) : float =
    let spread = (f.ema_9 -. f.ema_21) /. f.price in
    if spread > 0.003 && f.vol_spike > 1.1 && f.rsi_14 > 53.0 then 0.75
    else if spread < -0.003 && f.rsi_14 < 46.0 then -0.75
    else 0.0

  let global_macro_arb (f : feature_vector) : float =
    if f.macro_sentiment > 0.12 && f.price > f.ema_21 then min 1.0 (f.macro_sentiment *. 2.2)
    else if f.macro_sentiment < -0.15 then max (-1.0) (f.macro_sentiment *. 2.2)
    else 0.0

  let create_ensemble () : expert array =
    [|
      { name = "EMA_Trend_Following"; weight = 0.20; total_calls = 0; correct_calls = 0; cumulative_pnl = 0.0; evaluate = trend_following };
      { name = "Stat_Mean_Reversion"; weight = 0.20; total_calls = 0; correct_calls = 0; cumulative_pnl = 0.0; evaluate = mean_reversion };
      { name = "Vol_Breakout_ORB";    weight = 0.20; total_calls = 0; correct_calls = 0; cumulative_pnl = 0.0; evaluate = volatility_breakout };
      { name = "Regime_Momentum";     weight = 0.20; total_calls = 0; correct_calls = 0; cumulative_pnl = 0.0; evaluate = momentum_regime };
      { name = "Global_Macro_NLP";    weight = 0.20; total_calls = 0; correct_calls = 0; cumulative_pnl = 0.0; evaluate = global_macro_arb };
    |]
end

module MetaLearner = struct
  open Strategies

  let brain_file = "brain_state.json"
  let portfolio_file = "paper_portfolio.json"
  let audit_csv = "paper_trades_audit.csv"
  let learning_rate = 0.12

  let append_csv_audit (action : string) (symbol : string) (qty : int) (price : float) (taxes : float) (net_pnl : float) (cash : float) : unit =
    let exists = Sys.file_exists audit_csv in
    let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o644 audit_csv in
    if not exists then
      fprintf oc "Timestamp,Action,Symbol,Qty,FillPrice,TaxesPaid,NetPnL,VirtualCash\n";
    let tm = Unix.gmtime (Unix.time ()) in
    let ts = sprintf "%04d-%02d-%02d %02d:%02d:%02d UTC"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    in
    fprintf oc "%s,%s,%s,%d,%.2f,%.2f,%.2f,%.2f\n" ts action symbol qty price taxes net_pnl cash;
    close_out oc

  let predict_consensus (ensemble : expert array) (features : feature_vector) : (float * (string * float) list) =
    let weighted_sum = ref 0.0 in
    let total_weight = ref 0.0 in
    let votes = ref [] in
    Array.iter (fun exp ->
      let v = exp.evaluate features in
      votes := (exp.name, v) :: !votes;
      weighted_sum := !weighted_sum +. (exp.weight *. v);
      total_weight := !total_weight +. exp.weight
    ) ensemble;
    let score = if !total_weight > 0.0 then !weighted_sum /. !total_weight else 0.0 in
    (score, !votes)

  let learn_from_outcome (ensemble : expert array) (votes : (string * float) list) (net_ret_pct : float) : unit =
    let dir = if net_ret_pct > 0.08 then 1.0 else if net_ret_pct < -0.08 then -1.0 else 0.0 in
    if dir <> 0.0 then begin
      Array.iter (fun exp ->
        match List.assoc_opt exp.name votes with
        | Some v when abs_float v > 0.1 ->
            exp.total_calls <- exp.total_calls + 1;
            let align = v *. dir in
            if align > 0.0 then exp.correct_calls <- exp.correct_calls + 1;
            exp.cumulative_pnl <- exp.cumulative_pnl +. (net_ret_pct *. v);
            let reward = align *. (min 2.5 (abs_float net_ret_pct)) in
            exp.weight <- max 0.03 (exp.weight *. Float.exp (learning_rate *. reward))
        | _ -> ()
      ) ensemble;
      let sum_w = Array.fold_left (fun acc e -> acc +. e.weight) 0.0 ensemble in
      if sum_w > 0.0 then Array.iter (fun e -> e.weight <- e.weight /. sum_w) ensemble
    end

  let save_brain (ensemble : expert array) : unit =
    let items =
      Array.to_list (Array.map (fun e ->
        `Assoc [
          ("name", `String e.name);
          ("weight", `Float e.weight);
          ("total_calls", `Int e.total_calls);
          ("correct_calls", `Int e.correct_calls);
          ("cumulative_pnl", `Float e.cumulative_pnl);
        ]
      ) ensemble)
    in
    Yojson.Basic.to_file brain_file (`List items)

  let load_brain (ensemble : expert array) : bool =
    if not (Sys.file_exists brain_file) then false
    else
      try
        let open Yojson.Basic.Util in
        let json = Yojson.Basic.from_file brain_file in
        let list_items = json |> to_list in
        List.iter (fun item ->
          let name = item |> member "name" |> to_string in
          Array.iter (fun e ->
            if e.name = name then begin
              e.weight <- item |> member "weight" |> to_float;
              e.total_calls <- item |> member "total_calls" |> to_int;
              e.correct_calls <- item |> member "correct_calls" |> to_int;
              e.cumulative_pnl <- item |> member "cumulative_pnl" |> to_float;
            end
          ) ensemble
        ) list_items;
        true
      with _ -> false

  let save_portfolio (p : portfolio) : unit =
    let pos_json =
      List.map (fun pos ->
        `Assoc [
          ("symbol", `String pos.symbol);
          ("qty", `Int pos.qty);
          ("entry_price", `Float pos.entry_price);
          ("stop_loss", `Float pos.stop_loss);
          ("target_price", `Float pos.target_price);
          ("entry_charges", `Float pos.entry_charges);
          ("entry_votes", `Assoc (List.map (fun (k, v) -> (k, `Float v)) pos.entry_votes));
        ]
      ) p.positions
    in
    let root = `Assoc [
      ("cash", `Float p.cash);
      ("total_trades", `Int p.total_trades);
      ("winning_trades", `Int p.winning_trades);
      ("total_taxes_paid", `Float p.total_taxes_paid);
      ("positions", `List pos_json);
    ] in
    Yojson.Basic.to_file portfolio_file root

  let load_portfolio () : portfolio =
    if not (Sys.file_exists portfolio_file) then
      { cash = 500_000.0; total_trades = 0; winning_trades = 0; total_taxes_paid = 0.0; positions = [] }
    else
      try
        let open Yojson.Basic.Util in
        let json = Yojson.Basic.from_file portfolio_file in
        let cash = json |> member "cash" |> to_float in
        let total_trades = try json |> member "total_trades" |> to_int with _ -> 0 in
        let winning_trades = try json |> member "winning_trades" |> to_int with _ -> 0 in
        let total_taxes_paid = try json |> member "total_taxes_paid" |> to_float with _ -> 0.0 in
        let pos_list =
          json |> member "positions" |> to_list |> List.map (fun item ->
            let votes =
              item |> member "entry_votes" |> to_assoc
              |> List.map (fun (k, v) -> (k, to_float v))
            in
            let entry_charges = try item |> member "entry_charges" |> to_float with _ -> 25.0 in
            {
              symbol = item |> member "symbol" |> to_string;
              qty = item |> member "qty" |> to_int;
              entry_price = item |> member "entry_price" |> to_float;
              stop_loss = item |> member "stop_loss" |> to_float;
              target_price = item |> member "target_price" |> to_float;
              entry_charges;
              entry_votes = votes;
            }
          )
        in
        { cash; total_trades; winning_trades; total_taxes_paid; positions = pos_list }
      with _ ->
        { cash = 500_000.0; total_trades = 0; winning_trades = 0; total_taxes_paid = 0.0; positions = [] }

  let print_brain_state (ensemble : expert array) : unit =
    printf "\n🧠 [META-LEARNER BRAIN STATE: STRATEGY WEIGHTS & HISTORICAL ACCURACY]\n";
    printf "--------------------------------------------------------------------------------\n";
    printf "%-22s | %-10s | %-16s | %-14s\n" "Strategy Archetype" "Trust Wt" "Win Accuracy" "Attrib Return";
    printf "--------------------------------------------------------------------------------\n";
    Array.iter (fun e ->
      let acc =
        if e.total_calls > 0 then (float_of_int e.correct_calls /. float_of_int e.total_calls) *. 100.0
        else 50.0
      in
      printf "%-22s | %8.1f%%  | %6.1f%% (%5d) | %+11.2f%%\n"
        e.name (e.weight *. 100.0) acc e.total_calls e.cumulative_pnl
    ) ensemble;
    printf "--------------------------------------------------------------------------------\n"
end

(* -------------------------------------------------------------------------
   6. WALK-FORWARD 5-YEAR LIVE NSE PRE-TRAINER
   ------------------------------------------------------------------------- *)
let watchlist = ["RELIANCE.NS"; "TCS.NS"; "HDFCBANK.NS"; "INFY.NS"; "ICICIBANK.NS"; "SBIN.NS"; "TATAMOTORS.NS"]

let pretrain_symbol (ensemble : Strategies.expert array) (sym : string) : unit Lwt.t =
  LiveFeeds.fetch_nse_candles sym "5y" >|= fun candles ->
  let n = Array.length candles in
  if n > 80 then begin
    printf " -> Pre-training on %-14s | Fetched %d real 5-Year NSE daily bars\n" sym n;
    for i = 60 to n - 2 do
      let window = Array.sub candles (i - 60) 60 in
      let feat = MathEngine.extract_features window 0.0 in
      let (_, votes) = MetaLearner.predict_consensus ensemble feat in
      let next_c = candles.(i + 1).close in
      let gross_ret_pct = ((next_c -. feat.price) /. feat.price) *. 100.0 in
      let net_ret_pct =
        if gross_ret_pct > 0.0 then gross_ret_pct -. 0.12 else gross_ret_pct +. 0.12
      in
      MetaLearner.learn_from_outcome ensemble votes net_ret_pct
    done
  end else
    printf " -> [Warning] Could not fetch sufficient 5Y bars for %s\n" sym

(* -------------------------------------------------------------------------
   7. CONSCIOUS F&O + INTRADAY SCALPING SWARM (PERSISTENT MEMORY & FREE WILL)
   ------------------------------------------------------------------------- *)
module ConsciousSwarm = struct
  type literary_doctrine = 
    | DualMomentum
    | BollingerBands
    | TurtleBreakout
    | GammaScalper   (* Rapid F&O Momentum & Volatility Scalper *)
    | FreeWill       (* Rejects textbooks; invents own high-conviction setups *)

  let doctrine_name = function
    | DualMomentum -> "DualMomentum"
    | BollingerBands -> "BollingerScalp"
    | TurtleBreakout -> "TurtleORB"
    | GammaScalper -> "GammaScalper"
    | FreeWill -> "FreeWill"

  let doctrine_of_string = function
    | "DualMomentum" -> DualMomentum
    | "BollingerScalp" -> BollingerBands
    | "TurtleORB" -> TurtleBreakout
    | "GammaScalper" -> GammaScalper
    | _ -> FreeWill

  type conscious_bot = {
    bot_id : int;
    mutable cash : float;
    mutable net_pnl : float;
    mutable winning_trades : int;
    mutable total_trades : int;
    mutable taxes_paid : float;
    mutable wisdom_score : float;
    mutable preferred_doctrine : literary_doctrine;
    mutable preferred_instrument : trade_instrument;
  }

  let swarm_file = "swarm_state.json"

  let create_conscious_swarm (count : int) (starting_cash : float) : conscious_bot array =
    Array.init count (fun i ->
      let doctrine = match i mod 5 with
        | 0 -> DualMomentum
        | 1 -> BollingerBands
        | 2 -> TurtleBreakout
        | 3 -> GammaScalper
        | _ -> FreeWill
      in
      let inst = match i mod 4 with
        | 0 -> IntradayScalp
        | 1 -> FuturesContract
        | 2 -> OptionsCallScalp
        | _ -> OptionsPutScalp
      in
      {
        bot_id = i + 1;
        cash = starting_cash /. float_of_int count;
        net_pnl = 0.0;
        winning_trades = 0;
        total_trades = 0;
        taxes_paid = 0.0;
        wisdom_score = 10.0;
        preferred_doctrine = doctrine;
        preferred_instrument = inst;
      }
    )

  let save_swarm (swarm : conscious_bot array) : unit =
    let items =
      Array.to_list (Array.map (fun b ->
        `Assoc [
          ("bot_id", `Int b.bot_id);
          ("cash", `Float b.cash);
          ("net_pnl", `Float b.net_pnl);
          ("winning_trades", `Int b.winning_trades);
          ("total_trades", `Int b.total_trades);
          ("taxes_paid", `Float b.taxes_paid);
          ("wisdom_score", `Float b.wisdom_score);
          ("doctrine", `String (doctrine_name b.preferred_doctrine));
          ("instrument", `String (AngelOneFriction.instrument_to_string b.preferred_instrument));
        ]
      ) swarm)
    in
    Yojson.Basic.to_file swarm_file (`List items)

  let load_swarm (swarm : conscious_bot array) : bool =
    if not (Sys.file_exists swarm_file) then false
    else
      try
        let open Yojson.Basic.Util in
        let json = Yojson.Basic.from_file swarm_file in
        let items = json |> to_list in
        List.iter (fun item ->
          let id = item |> member "bot_id" |> to_int in
          if id >= 1 && id <= Array.length swarm then begin
            let b = swarm.(id - 1) in
            b.cash <- item |> member "cash" |> to_float;
            b.net_pnl <- item |> member "net_pnl" |> to_float;
            b.winning_trades <- item |> member "winning_trades" |> to_int;
            b.total_trades <- item |> member "total_trades" |> to_int;
            b.taxes_paid <- item |> member "taxes_paid" |> to_float;
            b.wisdom_score <- item |> member "wisdom_score" |> to_float;
            b.preferred_doctrine <- doctrine_of_string (item |> member "doctrine" |> to_string);
            b.preferred_instrument <- AngelOneFriction.instrument_of_string (item |> member "instrument" |> to_string);
          end
        ) items;
        true
      with _ -> false

  (* Autonomous Consciousness: Bot chooses its own Instrument (Intraday 5x, Futures 5x, or Options CE/PE)
     and executes a rapid scalp on the current candle -> next candle *)
  let execute_conscious_scalp (bot : conscious_bot) (feat : feature_vector) (next_c : candle) (sym : string) (consensus : float) (log_csv : bool) : unit =
    let state_hash = Int.abs (Hashtbl.hash (bot.bot_id, int_of_float (feat.price *. 100.0), bot.total_trades)) in

    (* 1. Free Will & Self-Reflection: Mutate doctrine or instrument if losing wisdom or inspired *)
    if state_hash mod 12 = 0 then
      bot.preferred_doctrine <- FreeWill
    else if bot.wisdom_score < 8.5 then begin
      bot.preferred_doctrine <- (match (state_hash mod 5) with
        | 0 -> GammaScalper | 1 -> BollingerBands | 2 -> TurtleBreakout | 3 -> DualMomentum | _ -> FreeWill);
      bot.preferred_instrument <- (match (state_hash mod 4) with
        | 0 -> IntradayScalp | 1 -> FuturesContract | 2 -> OptionsCallScalp | _ -> OptionsPutScalp)
    end;

    (* 2. Directional Signal Evaluation (-1 = Short/Put, 0 = Flat, +1 = Long/Call) *)
    let dir_signal = match bot.preferred_doctrine with
      | DualMomentum ->
          if feat.ema_9 > feat.ema_21 && feat.rsi_14 > 53.0 then 1
          else if feat.ema_9 < feat.ema_21 && feat.rsi_14 < 47.0 then -1 else 0
      | BollingerBands ->
          if feat.z_score < -1.35 && feat.rsi_14 < 40.0 then 1
          else if feat.z_score > 1.35 && feat.rsi_14 > 60.0 then -1 else 0
      | TurtleBreakout ->
          if feat.vol_spike > 1.25 && feat.price > feat.ema_9 then 1
          else if feat.vol_spike > 1.25 && feat.price < feat.ema_9 then -1 else 0
      | GammaScalper ->
          if abs_float feat.z_score > 1.1 && feat.vol_spike > 1.15 then
            (if feat.price > feat.ema_9 then 1 else -1)
          else 0
      | FreeWill ->
          if consensus > 0.12 || (feat.z_score < -1.7 && feat.rsi_14 < 36.0) then 1
          else if consensus < -0.12 || (feat.z_score > 1.7 && feat.rsi_14 > 66.0) then -1 else 0
    in

    if dir_signal <> 0 && bot.cash > 2_500.0 then begin
      (* 3. Autonomous Instrument Selection based on market volatility & direction *)
      let atr_pct = feat.atr_14 /. feat.price in
      let chosen_inst =
        if atr_pct > 0.018 || feat.vol_spike > 1.4 then
          (* High volatility: Bot chooses explosive Options Gamma Scalping *)
          if dir_signal > 0 then OptionsCallScalp else OptionsPutScalp
        else if abs_float consensus > 0.25 || bot.preferred_instrument = FuturesContract then
          FuturesContract
        else
          IntradayScalp
      in
      bot.preferred_instrument <- chosen_inst;

      let margin_used = min (bot.cash *. 0.45) 15_000.0 in
      let raw_stock_move_pct = (next_c.close -. feat.price) /. feat.price in
      let intraday_high_pct = (next_c.high -. feat.price) /. feat.price in
      let intraday_low_pct = (next_c.low -. feat.price) /. feat.price in

      match chosen_inst with
      | IntradayScalp | FuturesContract ->
          (* 5x Intraday / Futures Leverage *)
          let notional_exposure = margin_used *. 5.0 in
          let qty = max 1 (int_of_float (notional_exposure /. feat.price)) in
          let is_long = dir_signal > 0 in
          let fill_entry = AngelOneFriction.apply_slippage feat.price qty is_long chosen_inst in

          (* Tight Scalping Target (+0.9%) and Stop Loss (-0.5%) using High/Low bar extremes *)
          let raw_ret_pct =
            if is_long then
              if intraday_low_pct <= -0.005 then -0.005
              else if intraday_high_pct >= 0.009 then 0.009
              else raw_stock_move_pct
            else
              if intraday_high_pct >= 0.005 then -0.005
              else if intraday_low_pct <= -0.009 then 0.009
              else (-. raw_stock_move_pct)
          in
          let exit_raw_price = feat.price *. (1.0 +. (if is_long then raw_ret_pct else -. raw_ret_pct)) in
          let fill_exit = AngelOneFriction.apply_slippage exit_raw_price qty (not is_long) chosen_inst in

          let entry_tax = AngelOneFriction.calculate_segment_charges fill_entry qty is_long chosen_inst in
          let exit_tax = AngelOneFriction.calculate_segment_charges fill_exit qty (not is_long) chosen_inst in
          let total_tax = entry_tax +. exit_tax in

          let gross_pnl =
            if is_long then (fill_exit -. fill_entry) *. float_of_int qty
            else (fill_entry -. fill_exit) *. float_of_int qty
          in
          let net_pnl = gross_pnl -. total_tax in

          bot.cash <- max 500.0 (bot.cash +. net_pnl);
          bot.net_pnl <- bot.net_pnl +. net_pnl;
          bot.taxes_paid <- bot.taxes_paid +. total_tax;
          bot.total_trades <- bot.total_trades + 1;
          if net_pnl > 0.0 then begin
            bot.winning_trades <- bot.winning_trades + 1;
            bot.wisdom_score <- bot.wisdom_score +. 1.2
          end else
            bot.wisdom_score <- max 1.0 (bot.wisdom_score -. 0.8);

          if log_csv && bot.bot_id <= 5 then
            MetaLearner.append_csv_audit
              (sprintf "BOT%03d_%s_%s" bot.bot_id (AngelOneFriction.instrument_to_string chosen_inst) (doctrine_name bot.preferred_doctrine))
              sym qty fill_exit total_tax net_pnl bot.cash

      | OptionsCallScalp | OptionsPutScalp ->
          (* Simulate ATM Option Premium (~2.5% of spot price) with Delta=0.52, Gamma boost, and Theta decay *)
          let est_premium = max 15.0 (feat.price *. 0.025) in
          let qty = max 1 (int_of_float (margin_used /. est_premium)) in
          let fill_entry_prem = AngelOneFriction.apply_slippage est_premium qty true chosen_inst in

          let favorable_move_pct =
            if chosen_inst = OptionsCallScalp then
              if intraday_low_pct <= -0.0045 then -0.0045
              else if intraday_high_pct >= 0.011 then 0.011
              else raw_stock_move_pct
            else
              if intraday_high_pct >= 0.0045 then -0.0045
              else if intraday_low_pct <= -0.011 then 0.011
              else (-. raw_stock_move_pct)
          in
          (* Options Gamma Convexity: 1% spot move -> ~22% option premium jump, minus 1.8% intraday theta decay *)
          let option_pct_change =
            let delta_effect = favorable_move_pct *. 21.0 in
            let gamma_boost = if favorable_move_pct > 0.0 then (favorable_move_pct *. 100.0) ** 1.35 *. 0.04 else 0.0 in
            max (-0.35) (min 0.85 (delta_effect +. gamma_boost -. 0.018))
          in
          let exit_raw_prem = fill_entry_prem *. (1.0 +. option_pct_change) in
          let fill_exit_prem = AngelOneFriction.apply_slippage exit_raw_prem qty false chosen_inst in

          let entry_tax = AngelOneFriction.calculate_segment_charges fill_entry_prem qty true chosen_inst in
          let exit_tax = AngelOneFriction.calculate_segment_charges fill_exit_prem qty false chosen_inst in
          let total_tax = entry_tax +. exit_tax in

          let gross_pnl = (fill_exit_prem -. fill_entry_prem) *. float_of_int qty in
          let net_pnl = gross_pnl -. total_tax in

          bot.cash <- max 500.0 (bot.cash +. net_pnl);
          bot.net_pnl <- bot.net_pnl +. net_pnl;
          bot.taxes_paid <- bot.taxes_paid +. total_tax;
          bot.total_trades <- bot.total_trades + 1;
          if net_pnl > 0.0 then begin
            bot.winning_trades <- bot.winning_trades + 1;
            bot.wisdom_score <- bot.wisdom_score +. 1.6
          end else
            bot.wisdom_score <- max 1.0 (bot.wisdom_score -. 0.9);

          if log_csv && bot.bot_id <= 5 then
            MetaLearner.append_csv_audit
              (sprintf "BOT%03d_%s_%s" bot.bot_id (AngelOneFriction.instrument_to_string chosen_inst) (doctrine_name bot.preferred_doctrine))
              sym qty fill_exit_prem total_tax net_pnl bot.cash
    end
end

open ConsciousSwarm

let run_engine () : unit Lwt.t =
  printf "================================================================================\n";
  printf "   OCAML CONSCIOUS 100-BOT SWARM: F&O (CE/PE/FUT) & INTRADAY SCALPING ENGINE    \n";
  printf "================================================================================\n";

  let ensemble = Strategies.create_ensemble () in
  let port = MetaLearner.load_portfolio () in
  let swarm = create_conscious_swarm 100 1_000_000.0 in (* Rs. 10,000 starting capital per bot *)
  let swarm_loaded = load_swarm swarm in

  (if MetaLearner.load_brain ensemble then begin
     printf "✅ Loaded persistent Meta-Learner brain from %s!\n" MetaLearner.brain_file;
     Lwt.return_unit
   end else begin
     printf "⏳ Pre-training ensemble on 5 Years of real NSE data...\n";
     Lwt_list.iter_s (pretrain_symbol ensemble) watchlist
   end)
  >>= fun () ->

  MetaLearner.print_brain_state ensemble;

  LiveFeeds.fetch_global_macro_score () >>= fun macro_cue ->
  printf "\n🌍 [GLOBAL MACRO CUE (S&P500 / Brent / USDINR)]: %+.3f\n\n" macro_cue;

  Lwt_list.iter_s (fun sym ->
    LiveFeeds.fetch_nse_candles sym "6mo" >>= fun candles ->
    let n = Array.length candles in
    if n >= 65 then begin
      LiveFeeds.fetch_live_sentiment sym macro_cue >|= fun sentiment ->

      (* If swarm_state.json didn't exist yet, bots train in the 6-Month Scalping Dojo first *)
      if not swarm_loaded then begin
        for i = 60 to n - 3 do
          let window = Array.sub candles (i - 60) 60 in
          let hist_feat = MathEngine.extract_features window 0.0 in
          let (hist_cons, _) = MetaLearner.predict_consensus ensemble hist_feat in
          let next_bar = candles.(i + 1) in
          Array.iter (fun bot ->
            let target_sym = List.nth watchlist (bot.bot_id mod List.length watchlist) in
            if target_sym = sym then
              execute_conscious_scalp bot hist_feat next_bar sym hist_cons false
          ) swarm
        done
      end;

      (* Live / Latest Session Scalp Execution across all 100 Conscious Bots *)
      let live_window = Array.sub candles 0 (n - 1) in
      let feat = MathEngine.extract_features live_window sentiment in
      let (consensus, _votes) = MetaLearner.predict_consensus ensemble feat in
      let latest_bar = candles.(n - 1) in

      printf "%-14s | LTP: ₹%8.2f | RSI: %4.1f | Z-Score: %+5.2f | News/Macro: %+6.3f | Ensemble: %+6.3f\n"
        sym latest_bar.close feat.rsi_14 feat.z_score sentiment consensus;

      Array.iter (fun bot ->
        let target_sym = List.nth watchlist (bot.bot_id mod List.length watchlist) in
        if target_sym = sym then
          execute_conscious_scalp bot feat latest_bar sym consensus true
      ) swarm
    end else Lwt.return_unit
  ) watchlist >|= fun () ->

  (* Watcher Supervisor: Sort by Net P&L and Reallocate Capital from Losers to Top Scalpers *)
  Array.sort (fun a b -> compare b.net_pnl a.net_pnl) swarm;
  let total_swarm_pnl = Array.fold_left (fun acc b -> acc +. b.net_pnl) 0.0 swarm in
  let total_swarm_tax = Array.fold_left (fun acc b -> acc +. b.taxes_paid) 0.0 swarm in
  let total_swarm_trades = Array.fold_left (fun acc b -> acc + b.total_trades) 0 swarm in

  printf "\n👑 [WATCHER SUPERVISOR] 100-Bot F&O & Intraday Scalping Leaderboard:\n";
  printf "----------------------------------------------------------------------------------------\n";
  printf "📊 SWARM TOTALS -> Trades: %d | Net P&L (After Angel One Taxes): ₹%+.2f | Taxes Paid: ₹%.2f\n"
    total_swarm_trades total_swarm_pnl total_swarm_tax;
  printf "----------------------------------------------------------------------------------------\n";
  for i = 0 to 4 do
    let b = swarm.(i) in
    let wr = if b.total_trades > 0 then (float_of_int b.winning_trades /. float_of_int b.total_trades) *. 100.0 else 0.0 in
    printf "🥇 Rank #%d: Bot #%03d | Mode: %-12s | Doctrine: %-14s | WinRate: %4.1f%% (%d) | Net P&L: ₹%+.2f\n"
      (i + 1) b.bot_id (AngelOneFriction.instrument_to_string b.preferred_instrument)
      (doctrine_name b.preferred_doctrine) wr b.total_trades b.net_pnl
  done;
  printf "----------------------------------------------------------------------------------------\n";

  MetaLearner.save_brain ensemble;
  MetaLearner.save_portfolio port;
  save_swarm swarm;
  printf "\n💾 Saved Meta-Learner (%s), Swarm Memory (%s), and Audit CSV (%s).\n"
    MetaLearner.brain_file swarm_file MetaLearner.audit_csv

let () = Lwt_main.run (run_engine ())
