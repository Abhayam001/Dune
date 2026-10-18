# OCaml Live NSE Self-Learning Quant Bot (Angel One Paper Simulator)

Built with the **Awesome-OCaml** quantitative stack (`lwt`, `cohttp-lwt-unix`, `tls-lwt`, `yojson`), this autonomous trading bot pre-trains on 5 years of real National Stock Exchange (NSE) historical data, monitors live global macro indicators (`^GSPC` S&P 500, `BZ=F` Brent Crude, `INR=X` USD/INR) and Google News RSS feeds, and executes realistic paper trades with full Angel One statutory tax and slippage deductions.

## Included Files
- `dune-project` & `dune`: Build definitions for OCaml 5.2.x.
- `main.ml`: Complete multi-strategy ensemble, Multiplicative Weights Meta-Learner, Yahoo Finance + Google News RSS live ingestion, and Angel One brokerage/tax simulator.
- `.github/workflows/run_bot.yml`: Automated GitHub Actions runner that executes every 30 minutes during NSE market hours and commits learned weights (`brain_state.json`), virtual positions (`paper_portfolio.json`), and trade logs (`paper_trades_audit.csv`) back to the repository.

## How to Deploy from Android or Desktop
1. Create a new empty repository on GitHub (https://github.com/new).
2. Extract `ocaml-nse-quant-bot.zip` and upload all files (including `.github/workflows/run_bot.yml`) to the repository.
3. Open the **Actions** tab on GitHub and trigger **OCaml Live NSE Quant Bot** -> **Run workflow**.
