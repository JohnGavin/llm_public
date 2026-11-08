# Tests for the ccusage progress / dashboard / block functions.
# All data comes from small invented JSON caches written by helper-fixtures.R.

# ============================================================================
# 1. show_usage_progress()
# ============================================================================

test_that("show_usage_progress calculates percentage correctly", {
  expect_equal(show_usage_progress(current = 50, limit = 100, label = "Test"), 50)
  expect_equal(show_usage_progress(current = 75, limit = 100, label = "Test"), 75)
  expect_equal(show_usage_progress(current = 100, limit = 100, label = "Test"), 100)
})

test_that("show_usage_progress handles edge cases", {
  expect_equal(show_usage_progress(current = 0, limit = 100, label = "Test"), 0)
  # Over limit caps at 100%
  expect_equal(show_usage_progress(current = 150, limit = 100, label = "Test"), 100)
})

test_that("show_usage_progress validates inputs", {
  expect_error(show_usage_progress(current = NULL, limit = 100, label = "Test"))
  expect_error(show_usage_progress(current = 50, limit = NULL, label = "Test"))
  expect_error(show_usage_progress(current = -10, limit = 100, label = "Test"), ">= 0")
  expect_error(show_usage_progress(current = 50, limit = 0, label = "Test"), ">= 1e-05")
  expect_error(show_usage_progress(current = "50", limit = 100, label = "Test"), "number")
})

test_that("show_usage_progress handles token display", {
  result <- show_usage_progress(
    current = 15, limit = 30, label = "Test",
    show_tokens = TRUE, tokens_current = 250000, tokens_limit = 500000
  )
  expect_equal(result, 50)

  # Missing token values when show_tokens = TRUE are tolerated
  result <- show_usage_progress(
    current = 15, limit = 30,
    show_tokens = TRUE, tokens_current = NULL, tokens_limit = 500000
  )
  expect_equal(result, 50)
})

test_that("show_usage_progress returns invisibly and prints the bar", {
  expect_invisible(show_usage_progress(current = 50, limit = 100, label = "Test"))
  withr::local_options(list(cli.num_colors = 1, cli.dynamic = FALSE))
  out <- paste(testthat::capture_messages(
    show_usage_progress(current = 50, limit = 100, label = "Bar")
  ), collapse = "")
  expect_match(out, "50%", fixed = TRUE)
  expect_match(out, "Bar", fixed = TRUE)
})

# ============================================================================
# 2. show_daily_progress()
# ============================================================================

test_that("show_daily_progress calculates today's usage correctly", {
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date(), 15.5, 275000)))
  result <- show_daily_progress(cache, daily_limit = 30, token_limit = 500000)

  expect_equal(result$cost, 15.5)
  expect_equal(result$tokens, 275000)
  expect_equal(result$cost_pct, 52)
  expect_equal(result$token_pct, 55)
})

test_that("show_daily_progress handles no data for today", {
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date() - 1, 20, 300000)))
  result <- show_daily_progress(cache, daily_limit = 30)

  expect_equal(result$cost, 0)
  expect_equal(result$tokens, 0)
  expect_equal(result$cost_pct, 0)
})

test_that("show_daily_progress returns NULL when the cache file is missing or empty", {
  empty_dir <- withr::local_tempdir()
  expect_null(show_daily_progress(empty_dir))

  cache <- write_ccusage_cache(list())
  expect_null(show_daily_progress(cache))
})

test_that("show_daily_progress uses environment variables for limits", {
  withr::local_envvar(LLM_DAILY_LIMIT = "50", LLM_DAILY_TOKEN_LIMIT = "1000000")
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date(), 25, 500000)))

  result <- show_daily_progress(cache)
  expect_equal(result$cost_pct, 50)
  expect_equal(result$token_pct, 50)
})

test_that("show_daily_progress sums several projects for today", {
  cache <- write_ccusage_cache(list(
    proj1 = daily_df(Sys.Date(), 5, 100000),
    proj2 = daily_df(Sys.Date(), 10, 150000),
    proj3 = daily_df(Sys.Date(), 7.5, 125000)
  ))
  result <- show_daily_progress(cache, daily_limit = 30)

  expect_equal(result$cost, 22.5)
  expect_equal(result$tokens, 375000)
})

test_that("show_daily_progress requires an existing cache_dir", {
  expect_error(show_daily_progress())
  expect_error(show_daily_progress(file.path(tempdir(), "no-such-cache-dir")))
})

# ============================================================================
# 3. show_weekly_progress()
# ============================================================================

test_that("show_weekly_progress calculates the 7-day window correctly", {
  dates <- seq(Sys.Date() - 6, Sys.Date(), by = "day")
  cache <- write_ccusage_cache(list(demo = daily_df(dates, rep(10, 7), rep(200000, 7))))
  result <- show_weekly_progress(cache, weekly_limit = 120)

  expect_equal(result$cost, 70)
  expect_equal(result$cost_pct, 58)
  expect_equal(result$days_with_usage, 7)
})

test_that("show_weekly_progress excludes data outside the window", {
  cache <- write_ccusage_cache(list(demo = daily_df(
    c(Sys.Date() - 10, Sys.Date() - 3, Sys.Date()),
    c(50, 20, 15), c(500000, 300000, 250000)
  )))
  result <- show_weekly_progress(cache, weekly_limit = 120)

  expect_equal(result$cost, 35)
  expect_equal(result$days_with_usage, 2)
})

test_that("show_weekly_progress handles no recent data", {
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date() - 30, 50, 500000)))
  expect_equal(show_weekly_progress(cache, weekly_limit = 120)$cost, 0)
})

test_that("show_weekly_progress uses the environment variable for the limit", {
  withr::local_envvar(LLM_WEEKLY_LIMIT = "200")
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date(), 100, 1000000)))
  expect_equal(show_weekly_progress(cache)$cost_pct, 50)
})

test_that("show_weekly_progress returns NULL without data", {
  expect_null(show_weekly_progress(withr::local_tempdir()))
})

# ============================================================================
# 4. show_usage_dashboard()
# ============================================================================

test_that("show_usage_dashboard combines daily and weekly stats", {
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date(), 25, 400000)))
  result <- show_usage_dashboard(
    cache, daily_limit = 30, weekly_limit = 120, token_limit = 500000,
    show_max5 = FALSE
  )

  expect_named(result, c("daily", "weekly", "max5"))
  expect_equal(result$daily$cost, 25)
  expect_equal(result$weekly$cost, 25)
  expect_null(result$max5)
})

test_that("show_usage_dashboard includes block status when LLM_PLAN is max5", {
  withr::local_envvar(LLM_PLAN = "max5")
  cache <- write_ccusage_cache(list(demo = daily_df()))
  write_ccusage_file(cache, list(demo = blocks_df(in_current_block())), "blocks")

  result <- show_usage_dashboard(cache, show_max5 = TRUE)
  expect_false(is.null(result$max5))
  expect_equal(result$max5$tokens_used, 20000)
})

test_that("show_usage_dashboard handles a missing cache gracefully", {
  result <- show_usage_dashboard(withr::local_tempdir(), show_max5 = FALSE)
  expect_null(result$daily)
  expect_null(result$weekly)
})

test_that("show_usage_dashboard warns near the limits", {
  withr::local_options(list(cli.num_colors = 1, cli.dynamic = FALSE))
  cache <- write_ccusage_cache(list(demo = daily_df(Sys.Date(), 29, 100)))
  out <- paste(testthat::capture_messages(
    show_usage_dashboard(cache, daily_limit = 30, weekly_limit = 1000, show_max5 = FALSE)
  ), collapse = "")
  expect_match(out, "Daily cost limit nearly reached", fixed = TRUE)
})

test_that("Integration: dashboard with a week of data", {
  dates <- seq(Sys.Date() - 6, Sys.Date(), by = "day")
  costs <- c(15, 20, 18, 22, 25, 19, 17)
  cache <- write_ccusage_cache(list(demo = daily_df(dates, costs, rep(300000, 7))))

  result <- show_usage_dashboard(
    cache, daily_limit = 30, weekly_limit = 150, token_limit = 500000,
    show_max5 = FALSE
  )
  expect_equal(result$daily$cost, 17)
  expect_equal(result$weekly$cost, sum(costs))
})

# ============================================================================
# 5. get_current_block_window()
# ============================================================================

test_that("get_current_block_window calculates blocks correctly", {
  hours <- function(x) as.integer(format(x, "%H"))

  r <- get_current_block_window(as.POSIXct("2026-01-22 00:30:00"))
  expect_equal(c(hours(r$block_start), hours(r$block_end)), c(0, 5))

  r <- get_current_block_window(as.POSIXct("2026-01-22 07:30:00"))
  expect_equal(c(hours(r$block_start), hours(r$block_end)), c(5, 10))

  r <- get_current_block_window(as.POSIXct("2026-01-22 14:30:00"))
  expect_equal(c(hours(r$block_start), hours(r$block_end)), c(10, 15))

  # Last block of the day ends at 01:00 the next day
  r <- get_current_block_window(as.POSIXct("2026-01-22 22:30:00"))
  expect_equal(c(hours(r$block_start), hours(r$block_end)), c(20, 1))
})

test_that("get_current_block_window calculates time remaining correctly", {
  r <- get_current_block_window(as.POSIXct("2026-01-22 10:00:00"))
  expect_equal(as.numeric(r$time_remaining, units = "hours"), 5)

  r <- get_current_block_window(as.POSIXct("2026-01-22 12:30:00"))
  expect_equal(as.numeric(r$time_remaining, units = "hours"), 2.5)

  r <- get_current_block_window(as.POSIXct("2026-01-22 14:55:00"))
  expect_lt(as.numeric(r$time_remaining, units = "mins"), 10)
})

test_that("get_current_block_window handles boundary hours", {
  r <- get_current_block_window(as.POSIXct("2026-01-22 05:00:00"))
  expect_equal(as.integer(format(r$block_start, "%H")), 5)
  r <- get_current_block_window(as.POSIXct("2026-01-22 15:00:00"))
  expect_equal(as.integer(format(r$block_start, "%H")), 15)
})

test_that("get_current_block_window accepts Date and character input", {
  expect_s3_class(get_current_block_window(as.Date("2026-01-22"))$block_start, "POSIXct")
  expect_s3_class(get_current_block_window("2026-01-22 12:00:00")$block_start, "POSIXct")
})

test_that("get_current_block_window validates input", {
  expect_error(get_current_block_window(NULL), "POSIXct|Date|character")
  expect_error(get_current_block_window("not a time"), "convertible to POSIXct")
  expect_error(get_current_block_window(12345), "POSIXct|Date|character")
  expect_error(get_current_block_window(as.POSIXct(NA)), "NA")
})

# ============================================================================
# 6. calculate_block_usage()
# ============================================================================

test_that("calculate_block_usage sums tokens in the current block", {
  window <- get_current_block_window(as.POSIXct("2026-01-22 12:00:00"))
  blocks_data <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2026-01-22 10:30:00", "2026-01-22 12:00:00", "2026-01-22 14:30:00",
      "2026-01-22 09:00:00", "2026-01-22 16:00:00"
    )),
    totalTokens = c(10000, 15000, 20000, 50000, 60000)
  )

  result <- calculate_block_usage(blocks_data, window)
  expect_equal(result$tokens_used, 45000)
  expect_equal(result$usage_pct, 51)
})

test_that("calculate_block_usage handles empty data", {
  window <- get_current_block_window()
  for (empty in list(NULL, tibble::tibble())) {
    result <- calculate_block_usage(empty, window)
    expect_equal(result$tokens_used, 0)
    expect_equal(result$usage_pct, 0)
  }
})

test_that("calculate_block_usage handles no data in the current block", {
  window <- get_current_block_window(as.POSIXct("2026-01-22 12:00:00"))
  blocks_data <- tibble::tibble(
    timestamp = as.POSIXct("2026-01-22 16:00:00"),
    totalTokens = 50000
  )
  result <- calculate_block_usage(blocks_data, window)
  expect_equal(result$tokens_used, 0)
  expect_equal(result$usage_pct, 0)
})

test_that("calculate_block_usage uses the environment variable for the limit", {
  withr::local_envvar(LLM_BLOCK_LIMIT_TOKENS = "100000")
  window <- get_current_block_window()
  blocks_data <- tibble::tibble(timestamp = in_current_block(), totalTokens = 50000)

  result <- calculate_block_usage(blocks_data, window)
  expect_equal(result$tokens_limit, 100000)
  expect_equal(result$usage_pct, 50)
})

test_that("calculate_block_usage empty-data fallback honours LLM_BLOCK_LIMIT_TOKENS", {
  withr::local_envvar(LLM_BLOCK_LIMIT_TOKENS = "123456")
  window <- get_current_block_window()

  expect_equal(calculate_block_usage(NULL, window)$tokens_limit, 123456)
  expect_equal(calculate_block_usage(tibble::tibble(), window)$tokens_limit, 123456)
})

test_that("calculate_block_usage validates inputs", {
  expect_error(calculate_block_usage(tibble::tibble(), NULL))
  expect_error(calculate_block_usage(tibble::tibble(), list()), "names|named")
})

test_that("calculate_block_usage ignores NA tokens", {
  window <- get_current_block_window()
  blocks_data <- tibble::tibble(
    timestamp = rep(in_current_block(), 4),
    totalTokens = c(10000, NA, 20000, NA)
  )
  expect_equal(calculate_block_usage(blocks_data, window)$tokens_used, 30000)
})

test_that("Integration: block window and usage work together", {
  window <- get_current_block_window(as.POSIXct("2026-01-22 12:30:00"))
  expect_equal(as.integer(format(window$block_start, "%H")), 10)

  blocks_data <- tibble::tibble(
    timestamp = as.POSIXct(c(
      "2026-01-22 10:15:00", "2026-01-22 11:30:00", "2026-01-22 13:00:00"
    )),
    totalTokens = c(20000, 30000, 15000)
  )
  usage <- calculate_block_usage(blocks_data, window)
  expect_equal(usage$tokens_used, 65000)
  expect_equal(usage$usage_pct, 74)
})

# ============================================================================
# 7. show_max5_block_status()
# ============================================================================

test_that("show_max5_block_status displays the current block", {
  cache <- write_ccusage_cache(list(demo = blocks_df(in_current_block(), 44000)), "blocks")
  result <- show_max5_block_status(cache)

  expect_equal(result$tokens_used, 44000)
  expect_equal(result$usage_pct, 50)
  expect_equal(result$tokens_limit, 88000)
})

test_that("show_max5_block_status handles a missing blocks file", {
  result <- show_max5_block_status(withr::local_tempdir())
  expect_equal(result$tokens_used, 0)
  expect_equal(result$usage_pct, 0)
})

test_that("show_max5_block_status uses the threshold environment variables", {
  withr::local_envvar(LLM_WARN_THRESHOLD = "0.60", LLM_CRITICAL_THRESHOLD = "0.80")
  withr::local_options(list(cli.num_colors = 1, cli.dynamic = FALSE))
  cache <- write_ccusage_cache(list(demo = blocks_df(in_current_block(), 70400)), "blocks")

  out <- paste(testthat::capture_messages(
    result <- show_max5_block_status(cache)
  ), collapse = "")
  expect_equal(result$usage_pct, 80)
  expect_match(out, "CRITICAL", fixed = TRUE)
})

test_that("show_max5_block_status requires cache_dir", {
  expect_error(show_max5_block_status())
})

# ============================================================================
# 8. get_block_history()
# ============================================================================

test_that("get_block_history groups by 5-hour blocks", {
  now <- Sys.time()
  cache <- write_ccusage_cache(list(demo = blocks_df(
    c(now - 3600, now - 2400, now - 600, now - 300),
    c(10000, 15000, 20000, 25000)
  )), "blocks")

  result <- get_block_history(cache, days = 1)

  expect_gte(nrow(result), 1)
  expect_lte(nrow(result), 3)
  expect_equal(sum(result$total_tokens), 70000)
  expect_true(all(c("block_hour", "total_tokens", "usage_pct", "status") %in% names(result)))
})

test_that("get_block_history filters by the days parameter", {
  now <- Sys.time()
  cache <- write_ccusage_cache(list(demo = blocks_df(
    c(now - 5 * 24 * 3600, now - 2 * 24 * 3600, now),
    c(10000, 15000, 20000)
  )), "blocks")

  result <- get_block_history(cache, days = 3)
  expect_equal(sum(result$total_tokens), 35000)
})

test_that("get_block_history returns NULL without data", {
  expect_null(get_block_history(withr::local_tempdir()))
})

test_that("get_block_history honours LLM_BLOCK_LIMIT_TOKENS for usage_pct and day_pct", {
  withr::local_envvar(LLM_BLOCK_LIMIT_TOKENS = "100000")
  withr::local_options(list(cli.num_colors = 1, cli.dynamic = FALSE))
  cache <- write_ccusage_cache(list(demo = blocks_df(Sys.time() - 300, 50000)), "blocks")

  # cli writes via message(), so capture_messages() is required here
  out <- testthat::capture_messages(
    result <- get_block_history(cache, days = 1, grouped = TRUE)
  )
  out <- paste(out, collapse = "")

  # Per-block percentage: 50000 / 100000
  expect_equal(result$usage_pct, 50)
  # Day percentage (printed only): 50000 / (100000 * 5) = 10%, not 11%
  expect_true(grepl("10%\\)", out))
  expect_false(grepl("11%\\)", out))
})

test_that("get_block_history assigns status indicators", {
  today <- Sys.Date()
  ts <- as.POSIXct(paste(today, c("01:00:00", "06:00:00", "11:00:00", "16:00:00")))
  cache <- write_ccusage_cache(list(demo = blocks_df(ts, c(44000, 66000, 79200, 88000))), "blocks")

  result <- get_block_history(cache, days = 2)

  expect_equal(nrow(result), 4)
  expect_setequal(result$usage_pct, c(50, 75, 90, 100))
  expect_true("\U0001F534" %in% result$status)
  expect_true("\U0001F7E1" %in% result$status)
  expect_true("\U0001F7E2" %in% result$status)
})

test_that("get_block_history sorts by date and hour descending", {
  today <- Sys.Date()
  ts <- as.POSIXct(c(
    paste(today - 4, "10:00:00"), paste(today, "05:00:00"), paste(today - 2, "15:00:00")
  ))
  cache <- write_ccusage_cache(list(demo = blocks_df(ts, c(10000, 20000, 15000))), "blocks")

  result <- get_block_history(cache, days = 7)
  expect_equal(result$block_date, c(today, today - 2, today - 4))
})

test_that("get_block_history flat output lists one line per block", {
  withr::local_options(list(cli.num_colors = 1, cli.dynamic = FALSE))
  cache <- write_ccusage_cache(list(demo = blocks_df(Sys.time() - 300, 20000)), "blocks")
  out <- paste(testthat::capture_messages(
    get_block_history(cache, days = 1, grouped = FALSE)
  ), collapse = "")
  expect_match(out, "tokens", fixed = TRUE)
  expect_no_match(out, "Day total")
})

test_that("get_block_history validates its arguments", {
  cache <- withr::local_tempdir()
  expect_error(get_block_history())
  expect_error(get_block_history(cache, days = 0))
  expect_error(get_block_history(cache, grouped = "yes"))
})
