# Tests for tt() and tt_dependency() (R/hover_popup_helper.R)

test_that("tt() produces a span with the correct CSS class and attribute", {
  result <- tt("LLM", "Large Language Model. See <a href='https://example.com'>example</a>.")
  expect_true(grepl('class="tt"', result, fixed = TRUE))
  expect_true(grepl("data-tippy-content=", result, fixed = TRUE))
  expect_true(grepl('tabindex="0"', result, fixed = TRUE))
  expect_true(grepl(">LLM<", result, fixed = TRUE))
})

test_that("tt() HTML-escapes double-quotes in body to prevent attribute breakage", {
  body_with_quotes <- 'Use "quotes" carefully. See <a href="https://example.com">link</a>.'
  result <- tt("term", body_with_quotes)
  attr_content <- regmatches(result, regexpr('data-tippy-content="[^"]*"', result))
  expect_false(grepl('"quotes"', attr_content, fixed = TRUE))
  expect_true(grepl("&quot;quotes&quot;", attr_content, fixed = TRUE))
})

test_that("tt() HTML-escapes ampersands in body", {
  result <- tt("R&D", "Research & Development. See <a href='https://example.com'>ref</a>.")
  attr_content <- regmatches(result, regexpr('data-tippy-content="[^"]*"', result))
  expect_true(grepl("Research &amp; Development", attr_content, fixed = TRUE))
})

test_that("tt() preserves an embedded <a href> link in the attribute value", {
  link_body <- paste0(
    "<b>Targets pipeline</b>. A make-like build system for R. ",
    "See <a href='https://example.com/targets'>targets book</a>."
  )
  result <- tt("targets", link_body)
  expect_true(grepl("href=", result, fixed = TRUE))
  expect_true(grepl("targets book", result, fixed = TRUE))
})

test_that("tt() returns a single line for a multi-line-style body", {
  multi_line <- paste(
    "<b>Quality Assurance</b>. Systematic checks applied to outputs",
    "before deployment. Includes error scans and methodology checks.",
    "See <a href='https://example.com/qa'>the QA guide</a>."
  )
  result <- tt("QA", multi_line)
  expect_length(result, 1L)
  expect_false(grepl("\n", result, fixed = TRUE))
})

test_that("tt() rejects empty term or body", {
  expect_error(tt("", "Some body text."))
  expect_error(tt("term", ""))
})

test_that("tt() wraps term text verbatim (term not HTML-escaped)", {
  result <- tt("CI/CD", "Continuous integration. See <a href='https://example.com'>ref</a>.")
  expect_true(grepl(">CI/CD<", result, fixed = TRUE))
})

test_that("tt() raises an error when body has fewer than 2 sentences", {
  expect_error(
    tt("term", "Only one sentence here with <a href='https://example.com'>link</a>."),
    regexp = "at least 2 sentences"
  )
})

test_that("tt() raises an error when body has no <a href> anchor", {
  expect_error(
    tt("term", "First sentence. Second sentence without any link."),
    regexp = "at least one"
  )
})

test_that("tt() accepts a body with exactly 2 sentences and one link", {
  result <- tt(
    "API",
    "Application Programming Interface. Used to integrate services, see <a href='https://example.com/api'>docs</a>."
  )
  expect_true(grepl('class="tt"', result, fixed = TRUE))
  expect_true(grepl("href=", result, fixed = TRUE))
})

test_that("tt_dependency() points at shipped, non-empty script and stylesheet files", {
  dep <- tt_dependency()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "llmpublic-hover-popup")
  js <- file.path(dep$src$file, dep$script)
  css <- file.path(dep$src$file, dep$stylesheet)
  expect_true(file.exists(js))
  expect_true(file.exists(css))
  expect_gt(file.size(js), 0)
  expect_gt(file.size(css), 0)
  # The script must read the attribute that tt() writes
  expect_true(any(grepl("data-tippy-content", readLines(js, warn = FALSE), fixed = TRUE)))
})
