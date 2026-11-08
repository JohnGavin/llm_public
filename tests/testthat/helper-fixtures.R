# Synthetic fixtures for the ccusage tests. Every value is invented.

#' Write ccusage_<type>_all.json into a fresh temporary directory.
#'
#' @param projects Named list: project name -> data.frame of rows.
#' @param type "daily", "session" or "blocks".
#' @return The directory path.
write_ccusage_cache <- function(projects, type = "daily", .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  write_ccusage_file(dir, projects, type)
  dir
}

write_ccusage_file <- function(dir, projects, type = "daily") {
  jsonlite::write_json(
    list(projects = projects),
    file.path(dir, sprintf("ccusage_%s_all.json", type)),
    auto_unbox = TRUE,
    dataframe = "rows"
  )
  invisible(dir)
}

daily_df <- function(dates = Sys.Date(), costs = 15, tokens = 250000) {
  data.frame(
    date = as.character(dates),
    totalCost = costs,
    totalTokens = tokens,
    stringsAsFactors = FALSE
  )
}

iso_utc <- function(x) format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

blocks_df <- function(timestamps = Sys.time(), tokens = 20000) {
  data.frame(
    timestamp = iso_utc(timestamps),
    totalTokens = tokens,
    stringsAsFactors = FALSE
  )
}

# A POSIXct strictly inside the current 5-hour block
in_current_block <- function() get_current_block_window()$block_start + 60
