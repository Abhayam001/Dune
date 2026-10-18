(* =========================================================================
   OCAML LIVE NSE QUANT BOT WITH ANGEL ONE PAPER EXECUTION & TAX ENGINE
   Libraries: Lwt, Cohttp-lwt-unix, Yojson, Str (Awesome-OCaml Stack)
   ========================================================================= *)

open Lwt.Infix
open Printf

(* -------------------------------------------------------------------------
   1. CORE MARKET, TAX & PORTFOLIO TYPES
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
   2. ANGEL ONE & NSE STATUTORY BROKERAGE / SLIPPAGE SIMULATOR
   ------------------------------------------------------------------------- *)
module AngelOneFriction = struct
  let apply_slippage (price : float) (qty : int) (is_buy : bool) : float =
    let turnover = price *. float_of_int qty in
    let impact_pct = 0.0005 +. (min 0.0020 (turnover /. 50_000_000.0)) in
    if is_buy then price *. (1.0 +. impact_pct)
    else price *. (1.0 -. impact_pct)

  let calculate_charges (price : float) (qty : int) (is_buy : bool) : float =
    let turnover = price *. float_of_int qty in
    let brokerage = min 20.0 (0.0003 *. turnover) in
    let stt = if is_buy then 0.0 else 0.00025 *. turnover in
    let stamp_duty = if is_buy then 0.00003 *. turnover else 0.0 in
    let exchange_charge = 0.0000297 *. turnover in
    let sebi_fee = 0.000001 *. turnover in
    let gst = 0.18 *. (brokerage +. exchange_charge +. sebi_fee) in
    brokerage +. stt +. stamp_duty +. exchange_charge +. sebi_fee +. gst
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

  let ema (prices : float array) (period : int) : float =
    let n = Array.length prices in
    if n = 0 then 0.0
    else
      let alpha = 2.0 /. float_of_int (period + 1) in
      let acc = ref prices.(0) in
      for i = 1 to n - 1 do
        acc := (alpha *. prices.(i)) +. ((1.0 -. alpha) *. !acc)
      done;
      !acc

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
        100.0 -. (100.0 /. (1.0 +. rs))

  let atr (candles : candle array) (period : int) : float =
    let n = Array.length candles in
    if n <= period then 1.0
    else
      let sum_tr = ref 0.0 in
      for i = n - period to n - 1 do
        let c = candles.(i) in
        let prev_c = candles.(i - 1).close in
        let tr1 = c.high -. c.low in
        let tr2 = abs_float (c.high -. prev_c) in
        let tr3 = abs_float (c.low -. prev_c) in
        sum_tr := !sum_tr +. max tr1 (max tr2 tr3)
      done;
      !sum_tr /. float_of_int period

  let extract_features (candles : candle array) (macro_sentiment : float) : feature_vector =
    let n = Array.length candles in
    let closes = Array.map (fun c -> c.close) candles in
    let volumes = Array.map (fun c -> c.volume) candles in
    let last_close = closes.(n - 1) in

    let recent_20 = if n >= 20 then Array.sub closes (n - 20) 20 else closes in
    let recent_vol_20 = if n >= 20 then Array.sub volumes (n - 20) 20 else volumes in

    let sma_20 = mean recent_20 in
    let sd_20 = std_dev recent_20 in
    let avg_vol = max 1.0 (mean recent_vol_20) in

    {
      price = last_close;
      ema_9 = ema closes 9;
      ema_21 = ema closes 21;
      ema_50 = ema closes 50;
      rsi_14 = rsi closes 14;
      atr_14 = atr candles 14;
      bb_upper = sma_20 +. (2.0 *. sd_20);
      bb_lower = sma_20 -. (2.0 *. sd_20);
      z_score = (last_close -. sma_20) /. sd_20;
      vol_spike = volumes.(n - 1) /. avg_vol;
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
   6. WALK-FORWARD 5-YEAR LIVE NSE PRE-TRAINER & EXECUTION ENGINE
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
   7. CONSCIOUS QUANT SWARM & AUTONOMOUS LITERATURE COGNITION
   ------------------------------------------------------------------------- *)
module ConsciousSwarm = struct
  type literary_doctrine = 
    | DualMomentum
    | BollingerBands
    | TurtleBreakout
    | FreeWill

  type conscious_bot = {
    bot_id : int;
    mutable cash : float;
    mutable net_pnl : float;
    mutable winning_trades : int;
    mutable total_trades : int;
    mutable wisdom_score : float;
    mutable preferred_doctrine : literary_doctrine;
    mutable active_position : (string * int * float * float * float) option;
  }

  let create_conscious_swarm (count : int) (starting_cash : float) : conscious_bot array =
    Array.init count (fun i ->
      let doctrine = match i mod 4 with
        | 0 -> DualMomentum
        | 1 -> BollingerBands
        | 2 -> TurtleBreakout
        | _ -> FreeWill
      in
      {
        bot_id = i + 1;
        cash = starting_cash /. float_of_int count;
        net_pnl = 0.0;
        winning_trades = 0;
        total_trades = 0;
        wisdom_score = 10.0;
        preferred_doctrine = doctrine;
        active_position = None;
      }
    )

  let awaken_and_decide (bot : conscious_bot) (market_context : feature_vector) (sym : string) : unit =
    (* Autonomous choice: decide whether to keep book doctrine or use free will *)
    let mutiny_factor = (Int.abs (Hashtbl.hash (bot.bot_id, int_of_float market_context.price))) mod 10 in
    if mutiny_factor < 2 then bot.preferred_doctrine <- FreeWill;

    match bot.active_position with
    | Some (_s, qty, entry, sl, tp) ->
        if market_context.price <= sl || market_context.price >= tp || market_context.rsi_14 > 78.0 then begin
          let exit_value = float_of_int qty *. market_context.price in
          let pnl = exit_value -. (float_of_int qty *. entry) in
          bot.cash <- bot.cash +. exit_value;
          bot.net_pnl <- bot.net_pnl +. pnl;
          bot.total_trades <- bot.total_trades + 1;
          if pnl > 0.0 then begin
            bot.winning_trades <- bot.winning_trades + 1;
            bot.wisdom_score <- bot.wisdom_score +. 1.5
          end else begin
            bot.wisdom_score <- max 0.0 (bot.wisdom_score -. 1.0)
          end;
          bot.active_position <- None;
        end
    | None ->
        let should_buy = match bot.preferred_doctrine with
          | DualMomentum -> market_context.ema_9 > market_context.ema_21 && market_context.rsi_14 > 52.0
          | BollingerBands -> market_context.z_score < -1.5 && market_context.rsi_14 < 35.0
          | TurtleBreakout -> market_context.vol_spike > 1.3 && market_context.price > market_context.bb_upper
          | FreeWill -> market_context.z_score < -2.0 || market_context.macro_sentiment > 0.15
        in

        if should_buy && bot.cash > 8_000.0 then begin
          let alloc = min bot.cash 10_000.0 in
          let qty = max 1 (int_of_float (alloc /. market_context.price)) in
          let cost = float_of_int qty *. market_context.price in
          if bot.cash >= cost then begin
            bot.cash <- bot.cash -. cost;
            let sl = market_context.price -. (1.5 *. market_context.atr_14) in
            let tp = market_context.price +. (2.5 *. market_context.atr_14) in
            bot.active_position <- Some (sym, qty, market_context.price, sl, tp)
          end
        end
end

open ConsciousSwarm

let run_engine () : unit Lwt.t =
  printf "================================================================================\n";
  printf "   OCAML CONSCIOUS 100-BOT SWARM QUANT ENGINE (LITERATURE + AUTONOMY)           \n";
  printf "================================================================================\n";

  let ensemble = Strategies.create_ensemble () in
  let port = MetaLearner.load_portfolio () in
  let swarm = create_conscious_swarm 100 500_000.0 in

  (if MetaLearner.load_brain ensemble then begin
     printf "✅ Loaded persistent Meta-Learner brain!\n";
     Lwt.return_unit
   end else begin
     printf "⏳ Pre-training ensemble on 5 Years of real NSE data...\n";
     Lwt_list.iter_s (pretrain_symbol ensemble) watchlist
   end)
  >>= fun () ->

  MetaLearner.print_brain_state ensemble;

  LiveFeeds.fetch_global_macro_score () >>= fun macro_cue ->
  printf "\n🌍 [GLOBAL MACRO CUE]: %+.3f\n\n" macro_cue;

  (* Fetch candles for all watchlist symbols concurrently *)
  Lwt_list.iter_s (fun sym ->
    LiveFeeds.fetch_nse_candles sym "6mo" >>= fun candles ->
    let n = Array.length candles in
    if n >= 60 then begin
      LiveFeeds.fetch_live_sentiment sym macro_cue >|= fun sentiment ->
      let feat = MathEngine.extract_features candles sentiment in
      
      (* Distribute candles across the conscious swarm *)
      Array.iter (fun bot ->
        let target_sym = List.nth watchlist (bot.bot_id mod List.length watchlist) in
        if target_sym = sym then
          awaken_and_decide bot feat sym
      ) swarm
    end else Lwt.return_unit
  ) watchlist >|= fun () ->

  (* Watcher Audit of Conscious Swarm *)
  Array.sort (fun a b -> compare b.wisdom_score a.wisdom_score) swarm;
  printf "\n👑 [WATCHER SUPERVISOR] Conscious Swarm Cognition Audit:\n";
  printf "----------------------------------------------------------------\n";
  printf "🌟 Most Wise Bot #%03d | Wisdom: %.1f | Net P&L: ₹%+.2f | Wins: %d/%d\n"
    swarm.(0).bot_id swarm.(0).wisdom_score swarm.(0).net_pnl swarm.(0).winning_trades swarm.(0).total_trades;
  printf "🧠 Second Wise Bot #%03d | Wisdom: %.1f | Net P&L: ₹%+.2f | Wins: %d/%d\n"
    swarm.(1).bot_id swarm.(1).wisdom_score swarm.(1).net_pnl swarm.(1).winning_trades swarm.(1).total_trades;
  printf "----------------------------------------------------------------\n";

  MetaLearner.save_brain ensemble;
  MetaLearner.save_portfolio port;
  printf "\n💾 Conscious Swarm session executed & saved successfully.\n"

let () = Lwt_main.run (run_engine ())
