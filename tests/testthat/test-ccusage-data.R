# Tests for ccusage parsing, summarising and the DuckDB store.
# All data is small and invented.

# ---- normalize_to_char_vec() ------------------------------------------------

test_that("normalize_to_char_vec() handles NULL, list, string and empty input", {
  expect_equal(normalize_to_char_vec(NULL), character(0))
  expect_equal(normalize_to_char_vec(list("a", "b")), c("a", "b"))
  expect_equal(normalize_to_char_vec("a"), "a")
  expect_equal(normalize_to_char_vec(character(0)), character(0))
  expect_equal(normalize_to_char_vec(list()), character(0))
})

# ---- parse_ccusage_json() ---------------------------------------------------

make_json <- function() {
  list(projects = list(
    alpha = data.frame(date = c("2026-01-01", "2026-01-02"),
                       totalCost = c(1, 2), totalTokens = c(10, 20)),
    beta  = data.frame(date = "2026-01-01", totalCost = 5, totalTokens = 50)
  ))
}

test_that("parse_ccusage_json() combines projects and adds a project column", {
  res <- parse_ccusage_json(make_json())
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 3L)
  expect_setequal(res$project, c("alpha", "beta"))
  expect_equal(sum(res$totalCost), 8)
})

test_that("parse_ccusage_json() filters by project substring", {
  res <- parse_ccusage_json(make_json(), project_filter = "alp")
  expect_equal(unique(res$project), "alpha")
  expect_null(parse_ccusage_json(make_json(), project_filter = "zzz"))
})

test_that("parse_ccusage_json() treats the filter literally, not as a regex", {
  json <- list(projects = list(
    `a.b` = data.frame(date = "2026-01-01", totalCost = 1, totalTokens = 1),
    axb   = data.frame(date = "2026-01-01", totalCost = 1, totalTokens = 1)
  ))
  expect_equal(unique(parse_ccusage_json(json, "a.b")$project), "a.b")
})

test_that("parse_ccusage_json() returns NULL when there is no projects element", {
  expect_null(parse_ccusage_json(list(other = 1)))
})

test_that("parse_ccusage_json() normalises inconsistent modelsUsed values", {
  json <- list(projects = list(p = data.frame(date = c("d1", "d2"))))
  json$projects$p$modelsUsed <- list("m1", c("m1", "m2"))
  res <- parse_ccusage_json(json)
  expect_equal(res$modelsUsed[[1]], "m1")
  expect_equal(res$modelsUsed[[2]], c("m1", "m2"))
})

test_that("parse_ccusage_json() validates its arguments", {
  expect_error(parse_ccusage_json("not a list"))
  expect_error(parse_ccusage_json(make_json(), project_filter = 1))
})

# ---- read_ccusage_cache() ---------------------------------------------------

test_that("read_ccusage_cache() reads a daily file from an explicit directory", {
  cache <- write_ccusage_cache(list(demo = daily_df("2026-01-05", 3, 30)))
  res <- read_ccusage_cache("daily", cache_dir = cache)
  expect_equal(res$totalCost, 3)
  expect_equal(res$project, "demo")
})

test_that("read_ccusage_cache() filters by project and informs on a missing file", {
  cache <- write_ccusage_cache(list(a = daily_df(), b = daily_df()))
  expect_equal(unique(read_ccusage_cache("daily", cache, project_filter = "b")$project), "b")
  expect_message(
    res <- read_ccusage_cache("daily", withr::local_tempdir()),
    "Cache file not found"
  )
  expect_null(res)
})

test_that("read_ccusage_cache() converts block timestamps to POSIXct", {
  t0 <- as.POSIXct("2026-01-22 10:30:00", tz = "UTC")
  cache <- write_ccusage_cache(list(demo = blocks_df(t0, 100)), "blocks")
  res <- read_ccusage_cache("blocks", cache_dir = cache)
  expect_s3_class(res$timestamp, "POSIXct")
  expect_equal(as.numeric(res$timestamp), as.numeric(t0))
})

test_that("read_ccusage_cache() reads session files", {
  dir <- withr::local_tempdir()
  jsonlite::write_json(
    list(sessions = data.frame(sessionId = c("s1", "s2"), totalCost = c(1, 2))),
    file.path(dir, "ccusage_session_all.json"), auto_unbox = TRUE
  )
  res <- read_ccusage_cache("session", dir)
  expect_equal(res$sessionId, c("s1", "s2"))
})

test_that("read_ccusage_cache() requires cache_dir", {
  expect_error(read_ccusage_cache("daily"))
})

# ---- summarize_llm_usage() --------------------------------------------------

test_that("summarize_llm_usage() returns the expected metrics", {
  daily <- tibble::tibble(
    date = c("2026-01-01", "2026-01-02", "2026-01-02"),
    totalCost = c(10, 20, 30),
    totalTokens = c(1000, 2000, 3000)
  )
  res <- summarize_llm_usage(daily)

  expect_named(res, c("metric", "value"))
  expect_equal(res$value[res$metric == "Total Cost (USD)"], "$60.00")
  expect_equal(res$value[res$metric == "Total Tokens"], "6,000")
  expect_equal(res$value[res$metric == "Days Active"], "2")
  expect_equal(res$value[res$metric == "Date Range"], "2026-01-01 to 2026-01-02")
  expect_equal(res$value[res$metric == "Avg Cost/Day"], "$20.00")
})

test_that("summarize_llm_usage() returns an empty tibble for NULL or empty input", {
  expect_equal(nrow(summarize_llm_usage(NULL)), 0L)
  expect_equal(nrow(summarize_llm_usage(tibble::tibble())), 0L)
})

# ---- get_model_breakdown() --------------------------------------------------

test_that("get_model_breakdown() aggregates per model", {
  bd <- function(model, cost) {
    tibble::tibble(modelName = model, cost = cost, inputTokens = 10,
                   outputTokens = 5, cacheCreationTokens = 1, cacheReadTokens = 2)
  }
  daily <- tibble::tibble(date = c("2026-01-01", "2026-01-02"))
  daily$modelBreakdowns <- list(bd(c("m1", "m2"), c(1, 4)), bd("m1", 3))

  res <- get_model_breakdown(daily)
  expect_equal(res$modelName, c("m1", "m2"))
  expect_equal(res$total_cost, c(4, 4))
  expect_equal(res$days_used, c(2L, 1L))
  expect_equal(res$total_input, c(20, 10))
})

test_that("get_model_breakdown() returns NULL for empty data or missing column", {
  expect_null(get_model_breakdown(NULL))
  expect_null(get_model_breakdown(tibble::tibble()))
  expect_null(get_model_breakdown(tibble::tibble(date = "2026-01-01")))
})

# ---- find_activity_gaps() ---------------------------------------------------

test_that("find_activity_gaps() finds runs of missing days, newest first", {
  daily <- tibble::tibble(date = c(
    "2026-01-01", "2026-01-02", "2026-01-05", "2026-01-06", "2026-01-08"
  ))
  res <- find_activity_gaps(daily)

  expect_equal(res$gap_start, as.Date(c("2026-01-07", "2026-01-03")))
  expect_equal(res$gap_end, as.Date(c("2026-01-07", "2026-01-04")))
  expect_equal(res$gap_days, c(1L, 2L))
})

test_that("find_activity_gaps() returns an empty tibble when there are no gaps", {
  res <- find_activity_gaps(tibble::tibble(date = c("2026-01-01", "2026-01-02")))
  expect_equal(nrow(res), 0L)
  expect_named(res, c("gap_start", "gap_end", "gap_days"))
})

test_that("find_activity_gaps() returns NULL for NULL, empty or single-date input", {
  expect_null(find_activity_gaps(NULL))
  expect_null(find_activity_gaps(tibble::tibble(date = character())))
  expect_null(find_activity_gaps(tibble::tibble(date = "2026-01-01")))
})

# ---- append_to_duckdb() / query_latest_usage() ------------------------------

test_that("append_to_duckdb() creates the table and adds fetch_timestamp", {
  db <- file.path(withr::local_tempdir(), "sub", "usage.duckdb")
  data <- tibble::tibble(date = "2026-01-01", totalCost = 1.5, totalTokens = 100)

  res <- suppressMessages(append_to_duckdb(data, db))
  expect_true(file.exists(db))
  expect_true("fetch_timestamp" %in% names(res))
  expect_s3_class(res$fetch_timestamp, "POSIXct")
})

test_that("append_to_duckdb() announces what it wrote", {
  db <- file.path(withr::local_tempdir(), "usage.duckdb")
  data <- tibble::tibble(date = "2026-01-01", totalCost = 1.5, totalTokens = 100)
  expect_message(append_to_duckdb(data, db), "Appended 1 row")
})

test_that("append_to_duckdb() ignores NULL and empty data without creating a file", {
  db <- file.path(withr::local_tempdir(), "usage.duckdb")
  expect_message(res <- append_to_duckdb(NULL, db), "No data")
  expect_null(res)
  expect_message(append_to_duckdb(tibble::tibble(), db), "No data")
  expect_false(file.exists(db))
})

test_that("append_to_duckdb() requires db_path and validates arguments", {
  data <- tibble::tibble(date = "2026-01-01", totalCost = 1)
  expect_error(append_to_duckdb(data))
  expect_error(append_to_duckdb("not a data frame", "x.duckdb"))
  expect_error(append_to_duckdb(data, "x.duckdb", table_name = ""))
})

test_that("query_latest_usage() returns only the most recent append", {
  db <- file.path(withr::local_tempdir(), "usage.duckdb")
  suppressMessages(append_to_duckdb(
    tibble::tibble(date = "2026-01-01", totalCost = 1), db
  ))
  Sys.sleep(1.1)
  suppressMessages(append_to_duckdb(
    tibble::tibble(date = c("2026-01-02", "2026-01-03"), totalCost = c(2, 3)), db
  ))

  res <- query_latest_usage(db)
  expect_s3_class(res, "tbl_df")
  expect_equal(res$date, c("2026-01-02", "2026-01-03"))
})

test_that("query_latest_usage() honours table_name", {
  db <- file.path(withr::local_tempdir(), "usage.duckdb")
  suppressMessages(append_to_duckdb(
    tibble::tibble(date = "2026-01-01", totalCost = 1), db, table_name = "other_table"
  ))
  expect_equal(nrow(query_latest_usage(db, table_name = "other_table")), 1L)
})

test_that("query_latest_usage() returns NULL with a message for a missing database", {
  missing_db <- file.path(tempdir(), "no-such-usage.duckdb")
  expect_message(res <- query_latest_usage(missing_db), "Database not found")
  expect_null(res)
})

test_that("query_latest_usage() requires db_path", {
  expect_error(query_latest_usage())
})

# ---- fetch_ccusage() --------------------------------------------------------

# A fake `npx` placed first on PATH, so no network or Node.js is needed.
fake_npx <- function(script_body, .env = parent.frame()) {
  skip_on_os("windows")
  bin <- withr::local_tempdir(.local_envir = .env)
  path <- file.path(bin, "npx")
  writeLines(c("#!/bin/sh", script_body), path)
  Sys.chmod(path, "0755")
  withr::local_path(bin, action = "prefix", .local_envir = .env)
  invisible(bin)
}

test_that("fetch_ccusage() parses the command's JSON output", {
  fake_npx(c(
    "cat <<'EOF'",
    '{"projects": {"demo": [{"date": "2026-01-01", "totalCost": 2, "totalTokens": 20}]}}',
    "EOF"
  ))
  res <- fetch_ccusage("daily")
  expect_equal(res$project, "demo")
  expect_equal(res$totalCost, 2)
})

test_that("fetch_ccusage() passes the project filter through", {
  fake_npx(c(
    "cat <<'EOF'",
    '{"projects": {"keep": [{"date": "d", "totalCost": 1}], "drop": [{"date": "d", "totalCost": 9}]}}',
    "EOF"
  ))
  expect_equal(unique(fetch_ccusage("daily", project_filter = "keep")$project), "keep")
})

test_that("fetch_ccusage() warns and returns NULL when the command output is not JSON", {
  fake_npx("echo 'not json'")
  expect_warning(res <- fetch_ccusage("daily"), "ccusage command")
  expect_null(res)
})

test_that("fetch_ccusage() validates its arguments", {
  expect_error(fetch_ccusage("hourly"))
  expect_error(fetch_ccusage("daily", project_filter = 1))
})
