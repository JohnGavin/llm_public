# Tests for the static function-call analysis helpers.
# All fixtures are tiny R files written to a temporary directory.

make_code_dir <- function(.env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = .env)
  writeLines(c(
    "helper <- function(x) x + 1",
    "main <- function(x) {",
    "  y <- helper(x)",
    "  paste(y, stats::sd(x), utils::head(x))",
    "}"
  ), file.path(d, "a.R"))
  writeLines(c(
    "other <- function(v) {",
    "  paste(v, nchar(v))",
    "}"
  ), file.path(d, "b.R"))
  d
}

test_that("extract_file_calls() finds calls and flags namespaced ones", {
  d <- make_code_dir()
  res <- extract_file_calls(file.path(d, "a.R"))

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("file", "call", "has_namespace"))
  expect_true(all(res$file == "a.R"))
  expect_true("helper" %in% res$call)
  expect_true("stats::sd" %in% res$call)
  expect_true(res$has_namespace[res$call == "stats::sd"])
  expect_false(res$has_namespace[res$call == "paste"])
})

test_that("extract_file_calls() skips operators and keywords", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("x <- if (TRUE) 1 + 2 else c(1, 2)", f)
  res <- extract_file_calls(f)
  expect_equal(nrow(res), 0L)
  expect_named(res, c("file", "call", "has_namespace"))
})

test_that("extract_file_calls() copes with empty arguments such as x[, 1]", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("m <- matrix(1:4, 2)\nm[, 1]\nmean(m[1, ])", f)
  res <- extract_file_calls(f)
  expect_true("matrix" %in% res$call)
  expect_true("mean" %in% res$call)
})

test_that("extract_file_calls() warns and returns an empty tibble on a parse error", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines("f <- function( {", f)
  expect_warning(res <- extract_file_calls(f), "Failed to parse")
  expect_equal(nrow(res), 0L)
})

test_that("extract_file_calls() validates its input", {
  expect_error(extract_file_calls(NULL))
  expect_error(extract_file_calls(file.path(tempdir(), "no-such-file.R")))
})

test_that("extract_function_defs() returns bodies and defining files", {
  d <- make_code_dir()
  defs <- extract_function_defs(d)

  expect_setequal(names(defs), c("helper", "main", "other"))
  expect_equal(defs$helper$file, "a.R")
  expect_equal(defs$other$file, "b.R")
  expect_true(is.language(defs$main$body))
})

test_that("extract_function_defs() ignores non-function assignments", {
  d <- withr::local_tempdir()
  writeLines(c("x <- 1", "f <- function() 2"), file.path(d, "z.R"))
  expect_equal(names(extract_function_defs(d)), "f")
})

test_that("extract_function_defs() requires an existing directory", {
  expect_error(extract_function_defs(file.path(tempdir(), "no-such-dir")))
})

test_that("classify_call() classifies each kind of call", {
  expect_equal(classify_call("dplyr::filter"), "dplyr")
  expect_equal(classify_call("my_fn", our_functions = "my_fn"), "internal")
  expect_equal(classify_call("paste"), "base")
  expect_equal(classify_call("sd"), "stats")
  expect_equal(classify_call("head"), "utils")
  expect_equal(classify_call("definitely_not_a_function_zz"), "unknown")
})

test_that("classify_call() validates its input", {
  expect_error(classify_call(1))
  expect_error(classify_call("paste", our_functions = 1))
})

test_that("build_frequency_table() counts calls and files", {
  d <- make_code_dir()
  freq <- build_frequency_table(d)

  expect_named(freq, c("call", "package", "n_calls", "n_files"))
  # paste is called once in each of two files
  paste_row <- freq[freq$call == "paste", ]
  expect_equal(paste_row$n_calls, 2L)
  expect_equal(paste_row$n_files, 2L)
  expect_equal(paste_row$package, "base")
  expect_equal(freq$package[freq$call == "helper"], "internal")
  # Sorted by n_calls descending
  expect_false(is.unsorted(rev(freq$n_calls)))
})

test_that("build_frequency_table() returns an empty tibble for a directory with no calls", {
  d <- withr::local_tempdir()
  expect_equal(nrow(build_frequency_table(d)), 0L)
})

test_that("build_call_network() lists external calls only, one row per pair", {
  d <- make_code_dir()
  net <- build_call_network(d)

  expect_named(net, c("from", "to", "to_package"))
  # main calls helper (internal) - must not appear as an edge
  expect_false("helper" %in% net$to)
  main_edges <- net[net$from == "main", ]
  expect_true("stats::sd" %in% main_edges$to)
  expect_equal(main_edges$to_package[main_edges$to == "stats::sd"], "stats")
  expect_false(anyDuplicated(net[, c("from", "to")]) > 0)
})

test_that("build_call_network() returns an empty tibble when nothing is defined", {
  d <- withr::local_tempdir()
  writeLines("x <- 1", file.path(d, "x.R"))
  net <- build_call_network(d)
  expect_equal(nrow(net), 0L)
  expect_named(net, c("from", "to", "to_package"))
})
