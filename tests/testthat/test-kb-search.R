# Tests for kb_index() and kb_search()
#
# Hermetic: all tests use invented fixtures under a temporary directory.
#
# Coverage:
#  - kb_index() builds an index and returns the chunk count
#  - kb_search() finds the expected file for a matching query (provenance cols)
#  - kb_search() returns 0 rows for a no-match query
#  - kb_search() respects k
#  - both functions error informatively on bad paths / arguments
#  - chunk_file() internal: heading-less files fall back to block chunking,
#    text before the first heading becomes a preamble chunk

make_fixture <- function(.env = parent.frame()) {
  # Cleanup is registered in the calling test's environment so the directory
  # persists for the whole test.
  tmpdir <- withr::local_tempdir(.local_envir = .env)
  db_path <- file.path(tmpdir, "test.kbidx.duckdb")

  writeLines(c(
    "# DuckDB FTS Overview",
    "",
    "DuckDB provides a full-text search extension via the fts pragma.",
    "It supports BM25 ranking out of the box.",
    "",
    "## Installation",
    "",
    "Run LOAD fts to enable the extension.",
    "",
    "## Query syntax",
    "",
    "Use fts_main_table.match_bm25(id, query) to score documents."
  ), file.path(tmpdir, "duckdb-fts.md"))

  writeLines(c(
    "# Agent Dispatch Worktree",
    "",
    "Agents run in isolated git worktrees for sandboxed file writes.",
    "The dispatch ID propagates through commit footers.",
    "",
    "## Isolation",
    "",
    "Never write outside the worktree sandbox.",
    "Use git -C for all git operations."
  ), file.path(tmpdir, "agent-dispatch.md"))

  # No headings: exercises the block fallback
  writeLines(c(
    "This is a heading-less document.",
    "It discusses note organisation.",
    "Files go in raw or wiki sub-directories.",
    "Sources sections are mandatory.",
    "Cross-links use double-bracket notation.",
    "The system tracks provenance for every entry."
  ), file.path(tmpdir, "no-headings.md"))

  list(dir = tmpdir, db_path = db_path)
}

test_that("kb_index() returns chunk count and creates database file", {
  fx <- make_fixture()
  n <- kb_index(fx$dir, fx$db_path)

  expect_type(n, "integer")
  expect_gt(n, 0L)
  expect_true(file.exists(fx$db_path))
})

test_that("kb_index() chunk count is sensible (>= 1 per file)", {
  fx <- make_fixture()
  n <- kb_index(fx$dir, fx$db_path)
  # file 1: 3 headings, file 2: 2 headings, file 3: 1 block chunk => 6
  expect_equal(n, 6L)
})

test_that("kb_search() returns tibble with required provenance columns", {
  fx <- make_fixture()
  kb_index(fx$dir, fx$db_path)
  res <- kb_search("BM25 full-text search", fx$db_path)

  expect_s3_class(res, "tbl_df")
  expect_named(res, c("loc", "path", "heading", "line_start", "score", "snippet"))
})

test_that("kb_search() loc column is basename:line_start form", {
  fx <- make_fixture()
  kb_index(fx$dir, fx$db_path)
  res <- kb_search("worktree sandbox isolation", fx$db_path)

  expect_gt(nrow(res), 0L)
  expect_true(all(grepl("^[^:]+:\\d+$", res$loc)))
  expect_equal(res$loc, paste0(basename(res$path), ":", res$line_start))
})

test_that("kb_search() finds relevant file for a targeted query", {
  fx <- make_fixture()
  kb_index(fx$dir, fx$db_path)
  res <- kb_search("worktree sandbox isolation", fx$db_path)

  expect_gt(nrow(res), 0L)
  expect_true(any(grepl("agent-dispatch", res$path)))
})

test_that("kb_search() provenance: line_start is a positive integer in every row", {
  fx <- make_fixture()
  kb_index(fx$dir, fx$db_path)
  res <- kb_search("provenance entry", fx$db_path)

  expect_gt(nrow(res), 0L)
  expect_false(anyNA(res$line_start))
  expect_true(all(res$line_start >= 1L))
})

test_that("kb_search() returns 0 rows for a no-match query", {
  fx <- make_fixture()
  kb_index(fx$dir, fx$db_path)
  res <- kb_search("xyzzy99foobarbaz999zzz", fx$db_path)
  expect_equal(nrow(res), 0L)
  expect_named(res, c("loc", "path", "heading", "line_start", "score", "snippet"))
})

test_that("kb_search() respects k argument", {
  fx <- make_fixture()
  kb_index(fx$dir, fx$db_path)
  res <- kb_search("the", fx$db_path, k = 2L)
  expect_lte(nrow(res), 2L)
})

test_that("kb_index() overwrites an existing index rather than appending", {
  fx <- make_fixture()
  n1 <- kb_index(fx$dir, fx$db_path)
  n2 <- kb_index(fx$dir, fx$db_path)
  expect_equal(n1, n2)
})

test_that("kb_index() warns on a directory with no markdown and still writes an index", {
  empty <- withr::local_tempdir()
  db_path <- file.path(withr::local_tempdir(), "empty.kbidx.duckdb")
  expect_warning(n <- kb_index(empty, db_path), "No .md")
  expect_equal(n, 0L)
  expect_true(file.exists(db_path))
})

test_that("kb_index() errors when dir does not exist", {
  db_path <- file.path(withr::local_tempdir(), "test.kbidx.duckdb")
  missing_dir <- file.path(tempdir(), "no-such-notes-dir")
  expect_error(kb_index(missing_dir, db_path), regexp = "does not exist")
})

test_that("kb_search() errors when db_path does not exist", {
  missing_db <- file.path(tempdir(), "no-such-index.duckdb")
  expect_error(kb_search("anything", missing_db), regexp = "not found")
})

test_that("kb_index() and kb_search() validate their arguments", {
  expect_error(kb_index(NULL, "x.duckdb"))
  expect_error(kb_index(tempdir(), ""))
  expect_error(kb_search("", "x.duckdb"))
  expect_error(kb_search("q", "x.duckdb", k = 0L))
})

test_that("chunk_file() splits on headings and keeps a preamble", {
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(c("Intro text before any heading.", "", "# First", "a", "## Second", "b"), f)
  ch <- chunk_file(f)
  expect_equal(ch$heading, c("(preamble)", "First", "Second"))
  expect_equal(ch$line_start, c(1L, 3L, 5L))
})

test_that("chunk_file() falls back to ~40-line blocks without headings", {
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(paste("line", 1:85), f)
  ch <- chunk_file(f)
  expect_equal(ch$heading, c("(block 1)", "(block 2)", "(block 3)"))
  expect_equal(ch$line_start, c(1L, 41L, 81L))
})

test_that("chunk_file() returns zero rows for an empty file", {
  f <- withr::local_tempfile(fileext = ".md")
  file.create(f)
  expect_equal(nrow(chunk_file(f)), 0L)
})

# Structural guard (not a reproduction): DuckDB rejects a same-SELECT alias
# reference when the aliased expression has side effects, as match_bm25()
# does. The score must be computed in a subquery and only filtered/ordered in
# the outer query.
test_that("kb_search() SQL does not reference the score alias in its own SELECT", {
  sql <- paste(deparse(body(kb_search)), collapse = "\n")
  alias_pos <- regexpr("match_bm25", sql, fixed = TRUE)[[1]]
  close_pos <- regexpr(") AS scored", sql, fixed = TRUE)[[1]]
  filter_pos <- regexpr("WHERE score IS NOT NULL", sql, fixed = TRUE)[[1]]
  expect_true(alias_pos > 0 && close_pos > 0 && filter_pos > 0)
  expect_lt(alias_pos, close_pos)
  expect_lt(close_pos, filter_pos)
})
