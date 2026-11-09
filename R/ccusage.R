# LLM Usage Tracking Functions
# Functions for fetching, storing, and analysing ccusage data

#' Fetch ccusage data from the command line
#'
#' Runs `npx ccusage <type> --json --instances` and parses the result. Needs
#' Node.js and network access the first time `npx` fetches `ccusage`.
#'
#' @param type One of "daily", "weekly", "session", "blocks".
#' @param project_filter Project name to filter (NULL for all).
#' @return A tibble of usage data, or `NULL` (with a warning) if the command
#'   fails or produces no parseable output.
#' @export
fetch_ccusage <- function(type = c("daily", "weekly", "session", "blocks"),
                          project_filter = NULL) {
  type <- match.arg(type)
  checkmate::assert_string(project_filter, null.ok = TRUE)

  # Build command
  cmd <- sprintf("npx ccusage %s --json --instances", type)
  if (type == "blocks") {
    cmd <- paste(cmd, "--breakdown")
  }

  # Execute command; a failure is reported, not hidden
  result <- tryCatch({
    output <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
    jsonlite::fromJSON(paste(output, collapse = "\n"))
  }, error = function(e) {
    cli::cli_warn(c(
      "!" = "ccusage command not available or returned invalid JSON.",
      "i" = conditionMessage(e)
    ))
    NULL
  })

  if (is.null(result)) return(NULL)

  # Parse projects data
  parse_ccusage_json(result, project_filter)
}

#' Normalize a value to character vector (helper for modelsUsed)
#'
#' Handles the inconsistent JSON types: NULL, empty array, single string, or array
#' @param x A value that may be NULL, empty, list, or character
#' @return character vector
#' @keywords internal
normalize_to_char_vec <- function(x) {
  switch(
    class(x)[1],
    "NULL" = character(0),
    "list" = as.character(unlist(x)),
    as.character(x)
  ) |>
    (\(v) if (length(v) == 0) character(0) else v)()
}

#' Parse ccusage JSON output
#'
#' Turns the parsed `projects` element of `ccusage --instances --json`
#' output into one tibble, adding a `project` column.
#'
#' @param json_data Parsed JSON from ccusage (a list with a `projects`
#'   element).
#' @param project_filter Project name (substring, matched literally) to
#'   filter on. `NULL` keeps all projects.
#' @return A tibble of usage data, or `NULL` if there are no matching
#'   projects.
#' @export
parse_ccusage_json <- function(json_data, project_filter = NULL) {
  checkmate::assert_list(json_data)
  checkmate::assert_string(project_filter, null.ok = TRUE)
  if (is.null(json_data$projects)) return(NULL)

  projects <- names(json_data$projects) |>
    purrr::keep(~ is.null(project_filter) || grepl(project_filter, .x, fixed = TRUE))

  if (length(projects) == 0) return(NULL)

  # Combine all project data
  # Note: modelsUsed can be string, array, or empty - normalize to list for bind_rows
  purrr::map_dfr(projects, \(proj) {
    proj_data <- json_data$projects[[proj]]
    if (is.null(proj_data) || length(proj_data) == 0) return(NULL)

    tibble::as_tibble(proj_data) |>
      dplyr::mutate(
        project = proj,
        dplyr::across(
          dplyr::any_of("modelsUsed"),
          ~ purrr::map(.x, normalize_to_char_vec)
        )
      )
  })
}

#' Read a cached ccusage JSON file from an explicit directory
#'
#' Reads `ccusage_<type>_all.json` from `cache_dir`. The directory is always
#' supplied by the caller; nothing is searched for.
#'
#' @param type One of "daily", "session", "blocks".
#' @param cache_dir Directory containing the cached JSON files.
#' @param project_filter Project name (substring) to filter on, or `NULL`.
#' @return A tibble of usage data, or `NULL` (with a message) if the file is
#'   missing. For `type = "blocks"`, a character `timestamp` column holding
#'   ISO 8601 UTC times is converted to POSIXct.
#' @keywords internal
read_ccusage_cache <- function(type = c("daily", "session", "blocks"),
                               cache_dir,
                               project_filter = NULL) {
  type <- match.arg(type)
  checkmate::assert_string(cache_dir, min.chars = 1L)
  checkmate::assert_string(project_filter, null.ok = TRUE)

  cache_file <- file.path(cache_dir, sprintf("ccusage_%s_all.json", type))
  if (!file.exists(cache_file)) {
    cli::cli_inform(c("i" = "Cache file not found: {.file {cache_file}}"))
    return(NULL)
  }

  json_data <- jsonlite::fromJSON(cache_file)

  # Session data has a different structure
  if (type == "session" && !is.null(json_data$sessions)) {
    return(tibble::as_tibble(json_data$sessions))
  }

  result <- parse_ccusage_json(json_data, project_filter)

  # JSON has no date-time type: turn ISO 8601 timestamps (for example
  # "2026-01-22T10:30:00Z", read as UTC) into POSIXct shown in local time, so
  # the block functions can compare them with POSIXct windows.
  if (type == "blocks" && !is.null(result) && is.character(result$timestamp)) {
    ts <- sub("Z$", "", sub("T", " ", result$timestamp))
    ts <- as.POSIXct(ts, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M"))
    attr(ts, "tzone") <- ""
    result$timestamp <- ts
  }
  result
}

#' Get summary statistics for LLM usage
#'
#' @param daily_data Daily usage tibble with `totalCost`, `totalTokens` and
#'   `date` columns.
#' @return A tibble with columns `metric` and `value`.
#' @export
summarize_llm_usage <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(tibble::tibble(
      metric = character(),
      value = character()
    ))
  }

  # Rows are one per project per day, so average per distinct date, not per row
  n_days <- dplyr::n_distinct(daily_data$date)

  tibble::tibble(
    metric = c(
      "Total Cost (USD)",
      "Total Tokens",
      "Days Active",
      "Date Range",
      "Avg Cost/Day",
      "Avg Tokens/Day"
    ),
    value = c(
      sprintf("$%.2f", sum(daily_data$totalCost, na.rm = TRUE)),
      scales::comma(sum(daily_data$totalTokens, na.rm = TRUE)),
      as.character(n_days),
      sprintf("%s to %s", min(daily_data$date), max(daily_data$date)),
      sprintf("$%.2f", sum(daily_data$totalCost, na.rm = TRUE) / n_days),
      scales::comma(round(sum(daily_data$totalTokens, na.rm = TRUE) / n_days))
    )
  )
}

#' Get cost breakdown by model
#'
#' @param daily_data Daily usage tibble with modelBreakdowns
#' @return tibble with model costs
#' @export
get_model_breakdown <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(NULL)
  }

  # Extract model breakdowns
  if (!"modelBreakdowns" %in% names(daily_data)) {
    return(NULL)
  }

  purrr::map_dfr(seq_len(nrow(daily_data)), function(i) {
    breakdowns <- daily_data$modelBreakdowns[[i]]
    if (is.null(breakdowns) || length(breakdowns) == 0) return(NULL)

    tibble::as_tibble(breakdowns) |>
      dplyr::mutate(date = daily_data$date[i])
  }) |>
    dplyr::group_by(modelName) |>
    dplyr::summarise(
      total_cost = sum(cost, na.rm = TRUE),
      total_input = sum(inputTokens, na.rm = TRUE),
      total_output = sum(outputTokens, na.rm = TRUE),
      total_cache_creation = sum(cacheCreationTokens, na.rm = TRUE),
      total_cache_read = sum(cacheReadTokens, na.rm = TRUE),
      days_used = dplyr::n_distinct(date),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_cost))
}

#' Identify gaps in activity
#'
#' @param daily_data Daily usage tibble
#' @return tibble with gap information
#' @export
find_activity_gaps <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(NULL)
  }

  dates <- sort(unique(as.Date(daily_data$date)))

  if (length(dates) < 2) return(NULL)

  # Find all dates in range
  all_dates <- seq(min(dates), max(dates), by = "day")
  missing_dates <- all_dates[!all_dates %in% dates]

  if (length(missing_dates) == 0) {
    return(tibble::tibble(
      gap_start = as.Date(character()),
      gap_end = as.Date(character()),
      gap_days = integer()
    ))
  }

  # Group consecutive missing dates into gaps
  gaps <- tibble::tibble(date = missing_dates) |>
    dplyr::mutate(
      diff = c(1, diff(date)),
      group = cumsum(diff != 1)
    ) |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      gap_start = min(date),
      gap_end = max(date),
      gap_days = as.integer(gap_end - gap_start + 1),
      .groups = "drop"
    ) |>
    dplyr::select(-group) |>
    dplyr::filter(gap_days >= 1) |>
    dplyr::arrange(dplyr::desc(gap_start))

  gaps
}

#' Append usage data to DuckDB
#'
#' Adds a `fetch_timestamp` column and appends `data` to `table_name`,
#' creating the table (and the database file's directory) if needed.
#'
#' @param data Usage data tibble.
#' @param db_path Path to the DuckDB file. Required: there is no default
#'   location.
#' @param table_name Table name.
#' @return Invisibly, the data that was written (with `fetch_timestamp`), or
#'   `NULL` if `data` was `NULL` or empty.
#' @export
append_to_duckdb <- function(data, db_path, table_name = "daily_usage") {
  checkmate::assert_data_frame(data, null.ok = TRUE)
  checkmate::assert_string(db_path, min.chars = 1L)
  checkmate::assert_string(table_name, min.chars = 1L)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_inform(c("i" = "No data to append"))
    return(invisible(NULL))
  }

  # Ensure directory exists
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb(), db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Add fetch timestamp
  data <- data |>
    dplyr::mutate(fetch_timestamp = Sys.time())

  # Create table if not exists
  if (!DBI::dbExistsTable(con, table_name)) {
    DBI::dbWriteTable(con, table_name, data)
  } else {
    DBI::dbAppendTable(con, table_name, data)
  }

  cli::cli_inform(c("v" = "Appended {nrow(data)} row{?s} to {.val {table_name}}"))
  invisible(data)
}

#' Query latest usage data from DuckDB
#'
#' Returns the rows of `table_name` written by the most recent
#' [append_to_duckdb()] call (those with the maximum `fetch_timestamp`).
#'
#' @param db_path Path to the DuckDB file. Required: there is no default
#'   location.
#' @param table_name Table name.
#' @return A tibble of the latest data, or `NULL` (with a message) if the
#'   database file does not exist.
#' @export
query_latest_usage <- function(db_path, table_name = "daily_usage") {
  checkmate::assert_string(db_path, min.chars = 1L)
  checkmate::assert_string(table_name, min.chars = 1L)

  if (!file.exists(db_path)) {
    cli::cli_inform(c("i" = "Database not found: {.path {db_path}}"))
    return(NULL)
  }

  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  all_rows <- tibble::as_tibble(DBI::dbReadTable(con, table_name))
  all_rows |>
    dplyr::filter(fetch_timestamp == max(fetch_timestamp, na.rm = TRUE))
}

#' Read a numeric setting from an environment variable
#'
#' Aborts with a message naming the variable when the value is not a finite
#' number (or, with `positive = TRUE`, not greater than zero).
#'
#' @param name Environment variable name.
#' @param default Value (as a string) used when the variable is unset.
#' @param positive If TRUE the value must be > 0, otherwise >= 0.
#' @return A single finite number.
#' @noRd
env_number <- function(name, default, positive = TRUE) {
  raw <- Sys.getenv(name, as.character(default))
  is_number <- grepl(
    "^\\s*[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?\\s*$", raw
  )
  value <- if (is_number) as.numeric(raw) else NA_real_
  ok <- is.finite(value) && (if (positive) value > 0 else value >= 0)
  if (!ok) {
    cli::cli_abort(c(
      "x" = "Environment variable {.envvar {name}} must be a {if (positive) 'positive' else 'non-negative'} number.",
      "i" = "Got: {.val {raw}}"
    ))
  }
  value
}

#' Format remaining minutes as "Xh Ym" or "Ym"
#'
#' Rounds to whole minutes first, then splits, so 119.7 gives "2h 0m", never
#' "1h 60m".
#'
#' @param minutes Numeric minutes remaining.
#' @return A string.
#' @noRd
format_block_remaining <- function(minutes) {
  total <- as.integer(round(minutes))
  if (total > 60) {
    sprintf("%dh %dm", total %/% 60L, total %% 60L)
  } else {
    sprintf("%dm", total)
  }
}

#' Display progress bar for LLM usage
#'
#' @title Display Terminal Progress Bar for LLM Usage
#'
#' @description
#' Shows a terminal progress bar with current usage vs limits. The bar includes
#' a visual indicator using Unicode box characters, percentage completion, and
#' formatted values. Colors change based on usage thresholds (green < 75%, yellow
#' 75-89%, red >= 90%).
#'
#' @param current Current usage value (typically cost in USD)
#' @param limit Maximum limit value (upper bound for the progress bar)
#' @param label Character string label for the progress bar (default: "Usage")
#' @param show_tokens Logical whether to show token count on a second line (default: FALSE)
#' @param tokens_current Current token count (required if show_tokens = TRUE)
#' @param tokens_limit Token limit (required if show_tokens = TRUE)
#'
#' @return Invisibly returns the percentage used (numeric 0-100)
#'
#' @export
#'
#' @examples
#' # Show basic cost progress bar
#' show_usage_progress(15, 30, label = "Daily Cost")
#'
#' # Show cost and token usage
#' show_usage_progress(
#'   current = 25,
#'   limit = 30,
#'   label = "Daily Usage",
#'   show_tokens = TRUE,
#'   tokens_current = 450000,
#'   tokens_limit = 500000
#' )
#'
#' @seealso
#' - [show_daily_progress()] for today's complete usage summary
#' - [show_weekly_progress()] for weekly usage tracking
#' - [show_usage_dashboard()] for combined daily/weekly dashboard
#'
#' @keywords internal
show_usage_progress <- function(current, limit, label = "Usage",
                                 show_tokens = FALSE,
                                 tokens_current = NULL,
                                 tokens_limit = NULL) {
  # Validate inputs
  checkmate::assert_number(current, lower = 0, finite = TRUE)
  checkmate::assert_number(limit, lower = 0.00001, finite = TRUE) # limit must be > 0
  checkmate::assert_string(label)
  checkmate::assert_flag(show_tokens)
  
  if (show_tokens) {
    checkmate::assert_number(tokens_current, lower = 0, finite = TRUE, null.ok = TRUE)
    checkmate::assert_number(tokens_limit, lower = 1, finite = TRUE, null.ok = TRUE)
  }

  # Calculate percentage
  pct <- min(100, round((current / limit) * 100))

  # Determine color based on percentage
  color <- if (pct >= 90) "red" else if (pct >= 75) "yellow" else "green"

  # Create progress bar
  bar_width <- 40
  filled <- round(bar_width * pct / 100)
  empty <- bar_width - filled

  bar <- paste0(
    strrep("\U00002588", filled),  # full block
    strrep("\U00002591", empty)     # light shade
  )

  # Format output with color
  cli::cli_div(theme = list(
    ".pbar" = list(color = color),
    ".label" = list("font-weight" = "bold")
  ))

  cli::cli_text(
    "{.label {label}} {.pbar [{bar}]} {pct}% (${format(current, nsmall=2)} / ${format(limit, nsmall=2)})"
  )

  # Show tokens if requested
  if (show_tokens && !is.null(tokens_current) && !is.null(tokens_limit)) {
    token_pct <- min(100, round((tokens_current / tokens_limit) * 100))
    token_filled <- round(bar_width * token_pct / 100)
    token_empty <- bar_width - token_filled
    token_bar <- paste0(
      strrep("\U00002588", token_filled),  # full block
      strrep("\U00002591", token_empty)     # light shade
    )

    cli::cli_text(
      "Tokens:       [{token_bar}] {token_pct}% ({scales::comma(tokens_current)} / {scales::comma(tokens_limit)})"
    )
  }

  cli::cli_end()

  invisible(pct)
}

#' Display daily usage progress bars
#'
#' @title Show Today's LLM Usage Progress
#'
#' @description
#' Displays progress bars for today's LLM usage against configured daily limits.
#' Shows both cost and token consumption with visual progress bars and color coding.
#' Fetches data from cached ccusage files and calculates statistics for the current day.
#'
#' @param cache_dir Directory containing the cached `ccusage_daily_all.json`
#'   file. Required: no location is searched.
#' @param daily_limit Daily cost limit in USD. If NULL, reads from environment variable
#'   `LLMPUBLIC_DAILY_LIMIT` or defaults to 30.
#' @param token_limit Daily token limit. If NULL, reads from environment variable
#'   `LLMPUBLIC_DAILY_TOKEN_LIMIT` or defaults to 500000.
#'
#' @return Invisibly returns a list with elements:
#'   - `cost`: Numeric, current daily cost
#'   - `tokens`: Numeric, current daily token usage
#'   - `cost_pct`: Integer, percentage of daily cost limit used
#'   - `token_pct`: Integer, percentage of daily token limit used
#'
#' @export
#'
#' @examples
#' # Build a tiny invented cache, then show today's usage against it
#' cache <- file.path(tempdir(), "ccusage-cache")
#' dir.create(cache, showWarnings = FALSE)
#' jsonlite::write_json(
#'   list(projects = list(demo = data.frame(
#'     date = as.character(Sys.Date()), totalCost = 12.5, totalTokens = 200000
#'   ))),
#'   file.path(cache, "ccusage_daily_all.json"),
#'   auto_unbox = TRUE
#' )
#' show_daily_progress(cache, daily_limit = 50, token_limit = 750000)
#'
#' @seealso
#' - [show_usage_progress()] for low-level progress bar display
#' - [show_weekly_progress()] for weekly usage tracking
#' - [show_usage_dashboard()] for combined daily/weekly view
#'
#' @keywords internal
show_daily_progress <- function(cache_dir,
                                 daily_limit = NULL,
                                 token_limit = NULL) {
  # Validate inputs
  checkmate::assert_directory_exists(cache_dir)
  checkmate::assert_number(daily_limit, lower = 1e-5, finite = TRUE, null.ok = TRUE)
  checkmate::assert_number(token_limit, lower = 1, finite = TRUE, null.ok = TRUE)

  # Get limits from environment or defaults
  if (is.null(daily_limit)) {
    daily_limit <- env_number("LLMPUBLIC_DAILY_LIMIT", "30")
  }
  if (is.null(token_limit)) {
    token_limit <- env_number("LLMPUBLIC_DAILY_TOKEN_LIMIT", "500000")
  }

  # Load today's data
  daily_data <- read_ccusage_cache("daily", cache_dir = cache_dir)

  if (is.null(daily_data) || nrow(daily_data) == 0) {
    cli::cli_alert_warning("No usage data available")
    return(invisible(NULL))
  }

  # Filter to today
  today <- Sys.Date()
  today_data <- daily_data |>
    dplyr::filter(as.Date(date) == today)

  if (nrow(today_data) == 0) {
    # No data for today, show zero usage
    current_cost <- 0
    current_tokens <- 0
  } else {
    current_cost <- sum(today_data$totalCost, na.rm = TRUE)
    current_tokens <- sum(today_data$totalTokens, na.rm = TRUE)
  }

  cli::cli_h2("Today's Usage")

  show_usage_progress(
    current = current_cost,
    limit = daily_limit,
    label = "Today's Usage",
    show_tokens = TRUE,
    tokens_current = current_tokens,
    tokens_limit = token_limit
  )

  invisible(list(
    cost = current_cost,
    tokens = current_tokens,
    cost_pct = round((current_cost / daily_limit) * 100),
    token_pct = round((current_tokens / token_limit) * 100)
  ))
}

#' Display weekly usage progress bar
#'
#' @title Show 7-Day LLM Usage Progress
#'
#' @description
#' Displays a progress bar for the last 7 days of LLM usage against a configured
#' weekly cost limit. Aggregates daily usage data from the past 7 days and shows
#' the cumulative cost with visual progress indication.
#'
#' @param cache_dir Directory containing the cached `ccusage_daily_all.json`
#'   file. Required: no location is searched.
#' @param weekly_limit Weekly cost limit in USD. If NULL, reads from environment
#'   variable `LLMPUBLIC_WEEKLY_LIMIT` or defaults to 120.
#'
#' @return Invisibly returns a list with elements:
#'   - `cost`: Numeric, total cost for the last 7 days
#'   - `cost_pct`: Integer, percentage of weekly limit used
#'   - `days_with_usage`: Integer, number of days with recorded usage
#'
#' @export
#'
#' @examples
#' cache <- file.path(tempdir(), "ccusage-cache-weekly")
#' dir.create(cache, showWarnings = FALSE)
#' jsonlite::write_json(
#'   list(projects = list(demo = data.frame(
#'     date = as.character(Sys.Date() - 0:2),
#'     totalCost = c(10, 20, 30),
#'     totalTokens = c(100000, 200000, 300000)
#'   ))),
#'   file.path(cache, "ccusage_daily_all.json"),
#'   auto_unbox = TRUE
#' )
#' show_weekly_progress(cache, weekly_limit = 200)
#'
#' @seealso
#' - [show_usage_progress()] for low-level progress bar display
#' - [show_daily_progress()] for daily usage tracking
#' - [show_usage_dashboard()] for combined daily/weekly view
#'
#' @keywords internal
show_weekly_progress <- function(cache_dir, weekly_limit = NULL) {
  # Validate inputs
  checkmate::assert_directory_exists(cache_dir)
  checkmate::assert_number(weekly_limit, lower = 1e-5, finite = TRUE, null.ok = TRUE)

  # Get limit from environment or default
  if (is.null(weekly_limit)) {
    weekly_limit <- env_number("LLMPUBLIC_WEEKLY_LIMIT", "120")
  }

  # Load daily data
  daily_data <- read_ccusage_cache("daily", cache_dir = cache_dir)

  if (is.null(daily_data) || nrow(daily_data) == 0) {
    cli::cli_alert_warning("No usage data available")
    return(invisible(NULL))
  }

  # Filter to last 7 days
  week_start <- Sys.Date() - 6
  week_data <- daily_data |>
    dplyr::filter(as.Date(date) >= week_start)

  if (nrow(week_data) == 0) {
    current_cost <- 0
  } else {
    current_cost <- sum(week_data$totalCost, na.rm = TRUE)
  }

  cli::cli_h2("This Week")

  show_usage_progress(
    current = current_cost,
    limit = weekly_limit,
    label = "This Week",
    show_tokens = FALSE
  )

  invisible(list(
    cost = current_cost,
    cost_pct = round((current_cost / weekly_limit) * 100),
    days_with_usage = dplyr::n_distinct(week_data$date)
  ))
}

#' Display combined usage dashboard
#'
#' @title Show Complete LLM Usage Dashboard
#'
#' @description
#' Displays a comprehensive usage dashboard combining daily and weekly progress bars,
#' token usage, and Max5 block status (if applicable). Includes automated warnings
#' when approaching cost limits. This is the main entry point for monitoring LLM usage
#' across different time horizons.
#'
#' @param cache_dir Directory containing the cached `ccusage_*_all.json`
#'   files. Required: no location is searched.
#' @param daily_limit Daily cost limit in USD. If NULL, reads from `LLMPUBLIC_DAILY_LIMIT`
#'   environment variable or defaults to 30.
#' @param weekly_limit Weekly cost limit in USD. If NULL, reads from `LLMPUBLIC_WEEKLY_LIMIT`
#'   environment variable or defaults to 120.
#' @param token_limit Daily token limit. If NULL, reads from `LLMPUBLIC_DAILY_TOKEN_LIMIT`
#'   environment variable or defaults to 500000.
#' @param show_max5 Logical whether to show Max5 5-hour block status when the
#'   `LLMPUBLIC_PLAN` environment variable is set to "max5". Default: TRUE.
#'
#' @return Invisibly returns a list containing:
#'   - `daily`: List from [show_daily_progress()]
#'   - `weekly`: List from [show_weekly_progress()]
#'   - `max5`: List from [show_max5_block_status()] if applicable, otherwise NULL
#'
#' @export
#'
#' @examples
#' cache <- file.path(tempdir(), "ccusage-cache-dash")
#' dir.create(cache, showWarnings = FALSE)
#' jsonlite::write_json(
#'   list(projects = list(demo = data.frame(
#'     date = as.character(Sys.Date()), totalCost = 12.5, totalTokens = 200000
#'   ))),
#'   file.path(cache, "ccusage_daily_all.json"),
#'   auto_unbox = TRUE
#' )
#' show_usage_dashboard(cache, daily_limit = 50, weekly_limit = 200,
#'                      show_max5 = FALSE)
#'
#' @seealso
#' - [show_daily_progress()] for daily breakdown
#' - [show_weekly_progress()] for weekly breakdown
#' - [show_max5_block_status()] for Max5 plan block tracking
#' - [show_usage_progress()] for individual progress bars
#'
#' @keywords internal
show_usage_dashboard <- function(cache_dir,
                                  daily_limit = NULL,
                                  weekly_limit = NULL,
                                  token_limit = NULL,
                                  show_max5 = TRUE) {
  # Validate inputs
  checkmate::assert_directory_exists(cache_dir)
  checkmate::assert_number(daily_limit, lower = 1e-5, finite = TRUE, null.ok = TRUE)
  checkmate::assert_number(weekly_limit, lower = 1e-5, finite = TRUE, null.ok = TRUE)
  checkmate::assert_number(token_limit, lower = 1, finite = TRUE, null.ok = TRUE)
  checkmate::assert_flag(show_max5)

  cli::cli_h1("LLM Usage Dashboard")

  # Show daily progress
  daily_stats <- show_daily_progress(cache_dir, daily_limit, token_limit)

  cli::cli_text("")

  # Show weekly progress
  weekly_stats <- show_weekly_progress(cache_dir, weekly_limit)

  # Show Max5 block status if enabled
  max5_stats <- NULL
  if (show_max5 && tolower(Sys.getenv("LLMPUBLIC_PLAN", "")) == "max5") {
    cli::cli_text("")
    max5_stats <- show_max5_block_status(cache_dir)
  }

  # Add warnings if approaching limits
  if (!is.null(daily_stats)) {
    if (daily_stats$cost_pct >= 90) {
      cli::cli_alert_danger("Daily cost limit nearly reached!")
    } else if (daily_stats$cost_pct >= 75) {
      cli::cli_alert_warning("Approaching daily cost limit")
    }
  }

  if (!is.null(weekly_stats)) {
    if (weekly_stats$cost_pct >= 90) {
      cli::cli_alert_danger("Weekly cost limit nearly reached!")
    } else if (weekly_stats$cost_pct >= 75) {
      cli::cli_alert_warning("Approaching weekly cost limit")
    }
  }

  invisible(list(
    daily = daily_stats,
    weekly = weekly_stats,
    max5 = max5_stats
  ))
}

#' Get current Max5 5-hour block window
#'
#' @title Determine Current 5-Hour Block Window
#'
#' @description
#' Calculates which 5-hour block the current time falls into for Max5 plan users.
#' The day is divided into blocks starting at hours 0, 5, 10, 15, and 20 (UTC or local
#' timezone). Each block has a 5-hour duration and independent token usage limits.
#'
#' @param current_time Current time to calculate block for. Accepts POSIXct or can be
#'   converted to POSIXct. Defaults to [Sys.time()].
#'
#' @return List containing:
#'   - `block_start`: POSIXct, start time of the current block
#'   - `block_end`: POSIXct, end time of the current block (5 hours later, or
#'     the next local midnight for the last block of the day)
#'   - `time_remaining`: difftime object, time remaining in the current block
#'
#' @export
#'
#' @examples
#' # Get current block window
#' current_window <- get_current_block_window()
#' print(current_window$time_remaining)
#'
#' # Get block window for a specific time
#' get_current_block_window(as.POSIXct("2025-01-22 12:30:00"))
#'
#' @seealso
#' - [calculate_block_usage()] to calculate usage within a block window
#' - [show_max5_block_status()] to display block status
#' - [get_block_history()] to view historical block usage
#'
#' @keywords internal
get_current_block_window <- function(current_time = Sys.time()) {
  # Validate input
  # checkmate doesn't have a direct assert_posixct but we can check class or use testDate
  if (!inherits(current_time, c("POSIXct", "POSIXlt", "Date", "character"))) {
    cli::cli_abort(c("x" = "{.arg current_time} must be POSIXct, Date, or character", "i" = "Got: {.cls {class(current_time)}}"))
  }

  # Try to convert to POSIXct
  current_time <- tryCatch(
    as.POSIXct(current_time),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "{.arg current_time} must be convertible to POSIXct",
        "i" = conditionMessage(e)
      ))
    }
  )

  if (is.na(current_time)) {
    cli::cli_abort(c("x" = "{.arg current_time} is NA and must be a valid date/time"))
  }

  # Local calendar date and hour of day (0-23), in the time zone of `current_time`
  tz <- attr(current_time, "tzone")[1]
  if (is.null(tz) || is.na(tz)) tz <- ""
  lt <- as.POSIXlt(current_time, tz = tz)
  hour <- lt$hour

  # Block start hour (0, 5, 10, 15, 20)
  block_start_hour <- (hour %/% 5L) * 5L

  # Build both ends from the local date and hour rather than by adding elapsed
  # seconds, so a DST change does not shift them. The last block (20:00) is
  # clamped to the next local midnight.
  local_time <- function(date, hr) {
    as.POSIXct(sprintf("%s %02d:00:00", format(date, "%Y-%m-%d"), hr), tz = tz)
  }
  local_date <- as.Date(sprintf("%04d-%02d-%02d", lt$year + 1900L, lt$mon + 1L, lt$mday))
  block_start <- local_time(local_date, block_start_hour)
  block_end <- if (block_start_hour + 5L >= 24L) {
    local_time(local_date + 1, 0L)
  } else {
    local_time(local_date, block_start_hour + 5L)
  }

  list(
    block_start = block_start,
    block_end = block_end,
    time_remaining = difftime(block_end, current_time, units = "mins")
  )
}

#' Calculate Max5 block usage from blocks data
#'
#' @title Calculate Token Usage for 5-Hour Block
#'
#' @description
#' Calculates token usage statistics for the current 5-hour block. Filters the blocks
#' data to entries within the specified block window and aggregates token counts.
#' Returns usage metrics and percentage of the 88,000-token block limit.
#'
#' @param blocks_data Blocks data tibble (ccusage "blocks" report). Must
#'   contain POSIXct `timestamp` and numeric `totalTokens` columns.
#' @param current_window List from [get_current_block_window()] containing
#'   `block_start`, `block_end`, and `time_remaining` fields.
#'
#' @return List containing:
#'   - `tokens_used`: Numeric, tokens used in the current block
#'   - `tokens_limit`: Numeric, token limit per block (default 88000)
#'   - `usage_pct`: Integer, percentage of block limit used (not capped: above
#'     100 when the limit is exceeded)
#'   - `time_remaining`: difftime object, time remaining in the block
#'   - `block_start`: POSIXct, block start time
#'   - `block_end`: POSIXct, block end time
#'
#' @export
#'
#' @examples
#' window <- get_current_block_window()
#'
#' # Invented blocks data
#' blocks <- data.frame(
#'   timestamp = Sys.time(),
#'   totalTokens = 22000
#' )
#'
#' usage <- calculate_block_usage(blocks, window)
#' usage$usage_pct
#'
#' @seealso
#' - [get_current_block_window()] to get the block window
#' - [show_max5_block_status()] to display formatted block status
#' - [get_block_history()] to view historical blocks
#'
#' @keywords internal
calculate_block_usage <- function(blocks_data, current_window) {
  # Validate inputs
  checkmate::assert_data_frame(blocks_data, null.ok = TRUE)
  checkmate::assert_list(current_window, names = "named")
  checkmate::assert_names(names(current_window), must.include = c("block_start", "block_end", "time_remaining"))

  if (is.null(blocks_data) || nrow(blocks_data) == 0) {
    # Use the same overridable limit as the non-empty path below, rather than
    # a second hardcoded 88000; otherwise this function reports a different
    # tokens_limit depending on whether blocks_data is empty, even with
    # LLMPUBLIC_BLOCK_LIMIT_TOKENS set.
    return(list(
      tokens_used = 0,
      tokens_limit = env_number("LLMPUBLIC_BLOCK_LIMIT_TOKENS", "88000"),
      usage_pct = 0,
      time_remaining = current_window$time_remaining,
      block_start = current_window$block_start,
      block_end = current_window$block_end
    ))
  }

  # Filter to current block
  current_block <- blocks_data |>
    dplyr::filter(
      timestamp >= current_window$block_start,
      timestamp < current_window$block_end
    )

  if (nrow(current_block) == 0) {
    tokens_used <- 0
  } else {
    tokens_used <- sum(current_block$totalTokens, na.rm = TRUE)
  }

  # Max5 limit per 5-hour block
  tokens_limit <- env_number("LLMPUBLIC_BLOCK_LIMIT_TOKENS", "88000")

  list(
    tokens_used = tokens_used,
    tokens_limit = tokens_limit,
    usage_pct = round((tokens_used / tokens_limit) * 100),
    time_remaining = current_window$time_remaining,
    block_start = current_window$block_start,
    block_end = current_window$block_end
  )
}

#' Show Max5 5-hour block status
#'
#' @title Display Max5 5-Hour Block Status
#'
#' @description
#' Displays the current token usage for the active 5-hour block with a progress bar,
#' remaining time indicator, and threshold-based warnings. Designed for users on the
#' Max5 plan which has per-block token limits. Automatically loads cached block data
#' and calculates the current block window.
#'
#' @param cache_dir Directory containing the cached `ccusage_blocks_all.json`
#'   file. Required: no location is searched.
#'
#' @return Invisibly returns a list from [calculate_block_usage()] containing:
#'   - `tokens_used`: Numeric, tokens consumed in current block
#'   - `tokens_limit`: Numeric, token limit per block
#'   - `usage_pct`: Integer, percentage of limit used
#'   - `time_remaining`: difftime object, time until block ends
#'   - `block_start`: POSIXct, when the block started
#'   - `block_end`: POSIXct, when the block ends
#'
#' @export
#'
#' @examples
#' # With no blocks file in the directory, the status shows zero usage
#' show_max5_block_status(cache_dir = tempdir())
#'
#' @details
#' This function is most useful when called from [show_usage_dashboard()] with
#' `LLMPUBLIC_PLAN=max5` environment variable set. It automatically applies warning
#' thresholds based on:
#' - `LLMPUBLIC_WARN_THRESHOLD` (default 0.75): Yellow warning
#' - `LLMPUBLIC_CRITICAL_THRESHOLD` (default 0.90): Red critical alert
#'
#' @seealso
#' - [get_current_block_window()] to determine the active block
#' - [calculate_block_usage()] for the underlying calculation
#' - [get_block_history()] to view historical block usage
#' - [show_usage_dashboard()] for integration with daily/weekly views
#'
#' @keywords internal
show_max5_block_status <- function(cache_dir) {
  # Validate input
  checkmate::assert_directory_exists(cache_dir)

  cli::cli_h2("Max5 5-Hour Block Status")

  # Load blocks data
  blocks_data <- read_ccusage_cache("blocks", cache_dir = cache_dir)

  # Get current block window
  current_window <- get_current_block_window()

  # Calculate usage
  block_stats <- calculate_block_usage(blocks_data, current_window)

  # Format time remaining
  time_str <- format_block_remaining(as.numeric(block_stats$time_remaining, units = "mins"))

  cli::cli_text("{.dim Block: {format(block_stats$block_start, '%H:%M')} - {format(block_stats$block_end, '%H:%M')} | Remaining: {time_str}}")

  # Show progress bar
  pct <- block_stats$usage_pct
  color <- if (pct >= 90) "red" else if (pct >= 75) "yellow" else "green"

  bar_width <- 40
  # Cap only the bar; the text and warnings keep the real percentage
  bar_pct <- min(100, max(0, pct))
  filled <- round(bar_width * bar_pct / 100)
  empty <- bar_width - filled
  bar <- paste0(strrep("\U00002588", filled), strrep("\U00002591", empty))  # full block and light shade

  cli::cli_div(theme = list(
    ".pbar" = list(color = color),
    ".label" = list("font-weight" = "bold")
  ))

  cli::cli_text(
    "{.label Tokens} {.pbar [{bar}]} {pct}% ({scales::comma(block_stats$tokens_used)} / {scales::comma(block_stats$tokens_limit)})"
  )

  cli::cli_end()

  # Add warnings based on thresholds
  warn_threshold <- env_number("LLMPUBLIC_WARN_THRESHOLD", "0.75", positive = FALSE)
  critical_threshold <- env_number("LLMPUBLIC_CRITICAL_THRESHOLD", "0.90", positive = FALSE)

  if (pct >= critical_threshold * 100) {
    cli::cli_alert_danger("CRITICAL: {pct}% of 5-hour block limit reached - consider pausing")
  } else if (pct >= warn_threshold * 100) {
    cli::cli_alert_warning("WARNING: {pct}% of 5-hour block limit used")
  }

  invisible(block_stats)
}

#' Get Max5 block history
#'
#' @title Display Recent 5-Hour Block History
#'
#' @description
#' Displays historical usage for recent 5-hour blocks with status indicators and
#' completion percentages. Groups block data by date and block hour, showing up to
#' the last 10 blocks from the requested time period. Each block is tagged with a
#' status emoji (white = low, green = moderate, yellow = warning, red = critical).
#'
#' @param cache_dir Directory containing the cached `ccusage_blocks_all.json`
#'   file. Required: no location is searched.
#' @param days Number of days of history to show (default 5). Retrieves all blocks
#'   from the past N days.
#' @param grouped Logical. When TRUE (default), prints day headers with day totals
#'   and indented blocks underneath. When FALSE, prints a flat list (legacy format).
#'   Empty blocks (0 tokens) are always omitted.
#'
#' @return Invisibly returns a tibble with columns:
#'   - `block_id`: Character, unique identifier (date_hour format)
#'   - `block_date`: Date, date of the block
#'   - `block_hour`: Integer, starting hour of the block (0, 5, 10, 15, 20)
#'   - `total_tokens`: Numeric, tokens consumed in the block
#'   - `usage_pct`: Integer, percentage of 88000-token limit
#'   - `status`: Character, emoji status indicator
#'
#' @export
#'
#' @examples
#' # With no blocks file in the directory there is no history to show
#' get_block_history(cache_dir = tempdir())
#'
#' @details
#' Block status indicators:
#' - White circle = 0-49% of limit used (light usage)
#' - Green circle = 50-74% of limit used (normal usage)
#' - Yellow circle = 75-89% of limit used (warning)
#' - Red circle = 90%+ of limit used (critical)
#'
#' When `grouped = TRUE`, each day is shown as a header line:
#' ```
#' 2026-01-17 (Saturday)                    Day total: 45,200 tokens (51%)
#'   (white)  00:00-05:00    2,100 tokens   2%
#'   (green)  05:00-10:00   18,400 tokens  21%
#' ```
#'
#' @seealso
#' - [show_max5_block_status()] to see current block status
#' - [get_current_block_window()] to get the active block window
#' - [calculate_block_usage()] for individual block calculations
#'
#' @keywords internal
get_block_history <- function(cache_dir, days = 5, grouped = TRUE) {
  # Validate inputs
  checkmate::assert_directory_exists(cache_dir)
  checkmate::assert_count(days, positive = TRUE)
  checkmate::assert_flag(grouped)

  # Load blocks data
  blocks_data <- read_ccusage_cache("blocks", cache_dir = cache_dir)

  if (is.null(blocks_data) || nrow(blocks_data) == 0) {
    cli::cli_alert_warning("No block history available")
    return(NULL)
  }

  # Max5 limit per 5-hour block. This MUST match the LLMPUBLIC_BLOCK_LIMIT_TOKENS
  # override used by calculate_block_usage(), so setting the env var changes
  # both show_max5_block_status() and get_block_history().
  tokens_limit <- env_number("LLMPUBLIC_BLOCK_LIMIT_TOKENS", "88000")

  # Get cutoff time
  cutoff <- Sys.time() - days * 24 * 3600

  # Filter to recent data and summarise into 5-hour blocks
  recent_blocks <- blocks_data |>
    dplyr::filter(timestamp >= cutoff) |>
    dplyr::mutate(
      # Calculate which 5-hour block each entry belongs to
      block_hour = floor(as.integer(format(timestamp, "%H")) / 5) * 5,
      block_date = as.Date(timestamp),
      block_id = paste0(block_date, "_", sprintf("%02d", block_hour))
    ) |>
    dplyr::group_by(block_id, block_date, block_hour) |>
    dplyr::summarise(
      total_tokens = sum(totalTokens, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(total_tokens > 0) |>
    dplyr::mutate(
      usage_pct = round((total_tokens / tokens_limit) * 100),
      status = dplyr::case_when(
        usage_pct >= 90 ~ "\U0001F534",  # red circle
        usage_pct >= 75 ~ "\U0001F7E1",  # yellow circle
        usage_pct >= 50 ~ "\U0001F7E2",  # green circle
        TRUE ~ "\U000026AA"              # white circle
      )
    ) |>
    dplyr::arrange(dplyr::desc(block_date), dplyr::desc(block_hour))

  if (nrow(recent_blocks) == 0) {
    cli::cli_alert_info("No block activity in the last {days} days")
    return(invisible(recent_blocks))
  }

  cli::cli_h3("Recent Block History")

  if (!grouped) {
    # Legacy flat list
    for (i in seq_len(nrow(recent_blocks))) {
      row <- recent_blocks[i, ]
      cli::cli_text(
        "{row$status} {row$block_date} {sprintf('%02d:00', row$block_hour)}-{sprintf('%02d:00', (row$block_hour + 5L) %% 24L)}: {row$usage_pct}% ({scales::comma(row$total_tokens)} tokens)"
      )
    }
  } else {
    # Grouped output: day headers with subtotals, blocks indented below
    day_totals <- recent_blocks |>
      dplyr::group_by(block_date) |>
      dplyr::summarise(
        day_total = sum(total_tokens, na.rm = TRUE),
        .groups = "drop"
      ) |>
      # The "* 5" assumes 5 blocks/day (a second assumption alongside the
      # per-block token limit above). It is a round-number approximation, not
      # a derived value:
      # a 5-hour Max5 block reset cycle only fits into a 24h day ~4.8 times,
      # so 5 is a conservative round-up, not an exact daily block count.
      dplyr::mutate(day_pct = round((day_total / (tokens_limit * 5)) * 100))

    for (d in seq_len(nrow(day_totals))) {
      day_row <- day_totals[d, ]
      weekday_name <- weekdays(day_row$block_date)

      # Day header line
      cli::cli_text(
        "{day_row$block_date} ({weekday_name})  Day total: {scales::comma(day_row$day_total)} tokens ({day_row$day_pct}%)"
      )

      # Blocks for this day, sorted ascending by hour
      day_blocks <- recent_blocks |>
        dplyr::filter(block_date == day_row$block_date) |>
        dplyr::arrange(block_hour)

      for (i in seq_len(nrow(day_blocks))) {
        row <- day_blocks[i, ]
        start_label <- sprintf("%02d:00", row$block_hour)
        end_label <- sprintf("%02d:00", (row$block_hour + 5L) %% 24L)
        cli::cli_text(
          "  {row$status} {start_label}\u2013{end_label}  {format(scales::comma(row$total_tokens), width = 12)} tokens  {formatC(row$usage_pct, width = 3)}%"
        )
      }
    }
  }

  invisible(recent_blocks)
}
