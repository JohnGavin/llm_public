# llmpublic

Small, generic R helpers with no personal configuration. Every function takes
its paths and data as explicit arguments; none defaults to a location on your
machine.

Status: early development, version 0.0.0.9000, not released.

## What it does

- **ccusage summaries.** Parse and summarise reports from the
  [`ccusage`](https://github.com/ryoppippi/ccusage) command line tool, show
  terminal progress bars for daily, weekly and 5-hour-block usage, and keep a
  history in a DuckDB file.
- **Static call analysis.** Extract function calls and definitions from R
  source files, then build a frequency table or a one-level call network.
- **Notes search.** Build a DuckDB full-text (BM25, a standard relevance
  ranking) index over a directory of markdown notes and query it.
- **Hover popups.** Generate `<span>` elements whose popup text appears on
  hover or keyboard focus, with a small bundled stylesheet and script.

## Installation (Nix, with rix)

The development environment is reproducible with Nix and the
[rix](https://docs.ropensci.org/rix/) package. `default.R` lists the
packages; `default.nix` is generated from it.

```sh
# 1. Generate default.nix from default.R (needs a shell with rix available)
nix-shell --expr "$(curl -sl https://raw.githubusercontent.com/b-rodrigues/rix/master/inst/extdata/default.nix)" \
  --run "Rscript default.R"

# 2. Enter the project shell
nix-shell default.nix

# 3. Load the package for development
Rscript -e 'devtools::load_all()'
```

Do not use `install.packages()` inside the Nix shell: add packages to
`default.R` and regenerate `default.nix` instead.

## Functions

Usage tracking (ccusage):

| Function | Purpose |
|----------|---------|
| `fetch_ccusage()` | Run `npx ccusage` and parse its JSON output |
| `parse_ccusage_json()` | Turn parsed ccusage JSON into a tibble |
| `summarize_llm_usage()` | Totals and averages for a daily table |
| `get_model_breakdown()` | Cost and tokens per model |
| `find_activity_gaps()` | Runs of days with no usage |
| `append_to_duckdb()`, `query_latest_usage()` | Store usage in DuckDB and read the latest snapshot |
| `show_usage_progress()` | One coloured progress bar |
| `show_daily_progress()`, `show_weekly_progress()`, `show_usage_dashboard()` | Daily, 7-day and combined views |
| `get_current_block_window()`, `calculate_block_usage()`, `show_max5_block_status()`, `get_block_history()` | 5-hour block tracking |

Code analysis:

| Function | Purpose |
|----------|---------|
| `extract_file_calls()` | Function calls in one R file |
| `extract_function_defs()` | Top-level function definitions in a directory |
| `classify_call()` | Package, base, internal or unknown |
| `build_frequency_table()` | Call counts across a directory |
| `build_call_network()` | Which functions call which |

Notes search:

| Function | Purpose |
|----------|---------|
| `kb_index()` | Index a directory of `.md` / `.qmd` files |
| `kb_search()` | BM25 search of that index |

Hover popups:

| Function | Purpose |
|----------|---------|
| `tt()` | A popup `<span>` |
| `tt_dependency()` | The stylesheet and script that make it work |

## Environment variables

The usage helpers read these optional variables (all unset by default):

- `LLMPUBLIC_DAILY_LIMIT`: daily cost limit in USD (default 30).
- `LLMPUBLIC_DAILY_TOKEN_LIMIT`: daily token limit (default 500000).
- `LLMPUBLIC_WEEKLY_LIMIT`: 7-day cost limit in USD (default 120).
- `LLMPUBLIC_PLAN`: set to `max5` to include 5-hour block status in the dashboard (default empty).
- `LLMPUBLIC_BLOCK_LIMIT_TOKENS`: token limit per 5-hour block (default 88000).
- `LLMPUBLIC_WARN_THRESHOLD`: block usage fraction for a warning (default 0.75).
- `LLMPUBLIC_CRITICAL_THRESHOLD`: block usage fraction for a critical alert (default 0.90).

## Example

```r
library(llmpublic)

# Search a folder of markdown notes
index <- file.path(tempdir(), "notes.kbidx.duckdb")
kb_index("path/to/notes", index)
kb_search("cache results", index)

# Show today's usage from a directory holding ccusage_daily_all.json
show_daily_progress("path/to/cache", daily_limit = 30)
```

## Licence

MIT, see `LICENSE`.
