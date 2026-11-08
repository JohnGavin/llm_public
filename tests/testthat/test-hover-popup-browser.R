# Event-logic tests for inst/hover-popup/hover-popup.js, run in a real
# headless Chrome via chromote. Skipped when chromote or a browser is absent.

popup_page <- function() {
  dir <- system.file("hover-popup", package = "llmpublic")
  if (!nzchar(dir)) {
    dir <- testthat::test_path("..", "..", "inst", "hover-popup")
  }
  css <- paste(readLines(file.path(dir, "hover-popup.css")), collapse = "\n")
  js <- paste(readLines(file.path(dir, "hover-popup.js")), collapse = "\n")
  term <- function(label, id) {
    tt(
      label,
      paste0(
        "First sentence. See <a href='#", id, "' id='", id, "'>the ",
        label, " page</a>."
      )
    )
  }
  html <- paste0(
    "<!doctype html><html><head><style>", css,
    " body{margin:0;height:600px;font:16px sans-serif;padding:40px}",
    "</style></head><body><p>Some ", term("alpha", "linkA"),
    " text and ", term("beta", "linkB"), " more text.</p>",
    "<script>", js, "</script></body></html>"
  )
  path <- tempfile(fileext = ".html")
  writeLines(html, path)
  path
}

open_session <- function(path) {
  force(path)
  testthat::skip_if_not_installed("chromote")
  b <- tryCatch(
    {
      s <- chromote::ChromoteSession$new()
      s$Page$navigate(paste0("file://", path))
      Sys.sleep(0.4)
      s
    },
    error = function(e) NULL
  )
  if (is.null(b)) {
    testthat::skip("no headless Chrome available for chromote")
  }
  b
}

js_eval <- function(b, expr) {
  b$Runtime$evaluate(expr, returnByValue = TRUE)$result$value
}

mouse <- function(b, x, y) {
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
  Sys.sleep(0.05)
}

key <- function(b, name, code, vk, shift = FALSE) {
  mod <- if (shift) 8L else 0L
  b$Input$dispatchKeyEvent(
    type = "rawKeyDown", key = name, code = code,
    windowsVirtualKeyCode = vk, modifiers = mod
  )
  b$Input$dispatchKeyEvent(
    type = "keyUp", key = name, code = code,
    windowsVirtualKeyCode = vk, modifiers = mod
  )
  Sys.sleep(0.1)
}

rect <- function(b, selector) {
  r <- js_eval(b, sprintf(
    paste0(
      "(function(){var r=document.querySelectorAll('%s')[%d]",
      ".getBoundingClientRect();",
      "return {l:r.left,t:r.top,r:r.right,b:r.bottom};})()"
    ),
    selector, 0L
  ))
  r
}

popup_count <- function(b) js_eval(b, "document.querySelectorAll('.tt-popup').length")
popup_text <- function(b) {
  js_eval(b, "(document.querySelector('.tt-popup')||{textContent:''}).textContent")
}

test_that("pointer can travel term -> gap -> popup and click the link", {
  b <- open_session(popup_page())
  withr::defer(b$close())
  t1 <- rect(b, "span.tt")
  mouse(b, (t1$l + t1$r) / 2, (t1$t + t1$b) / 2)
  expect_equal(popup_count(b), 1L)
  mouse(b, (t1$l + t1$r) / 2, t1$b + 3) # the 6 px gap below the term
  p <- rect(b, ".tt-popup")
  expect_equal(popup_count(b), 1L)
  mouse(b, p$l + 10, p$t + 10)
  Sys.sleep(0.4) # longer than the hide delay
  expect_equal(popup_count(b), 1L)
  l <- rect(b, ".tt-popup a")
  x <- (l$l + l$r) / 2
  y <- (l$t + l$b) / 2
  b$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y,
                             button = "left", clickCount = 1L)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y,
                             button = "left", clickCount = 1L)
  Sys.sleep(0.1)
  expect_equal(js_eval(b, "location.hash"), "#linkA")
})

test_that("popup hides after the pointer leaves term and popup", {
  b <- open_session(popup_page())
  withr::defer(b$close())
  t1 <- rect(b, "span.tt")
  mouse(b, (t1$l + t1$r) / 2, (t1$t + t1$b) / 2)
  expect_equal(popup_count(b), 1L)
  mouse(b, 700, 500)
  Sys.sleep(0.5)
  expect_equal(popup_count(b), 0L)
})

test_that("Tab from a focused term reaches the popup link; Escape closes", {
  b <- open_session(popup_page())
  withr::defer(b$close())
  js_eval(b, "document.querySelector('span.tt').focus(); 1")
  Sys.sleep(0.1)
  expect_equal(popup_count(b), 1L)
  key(b, "Tab", "Tab", 9L)
  expect_equal(popup_count(b), 1L)
  expect_equal(js_eval(b, "document.activeElement.id"), "linkA")
  key(b, "Escape", "Escape", 27L)
  expect_equal(popup_count(b), 0L)
  expect_true(js_eval(
    b, "document.activeElement === document.querySelector('span.tt')"
  ))
})

test_that("focus leaving term and popup hides the popup", {
  b <- open_session(popup_page())
  withr::defer(b$close())
  js_eval(b, "document.querySelector('span.tt').focus(); 1")
  Sys.sleep(0.1)
  expect_equal(popup_count(b), 1L)
  js_eval(b, "document.activeElement.blur(); 1")
  Sys.sleep(0.1)
  expect_equal(popup_count(b), 0L)
})

test_that("moving to another term switches popups; term gets aria-describedby", {
  b <- open_session(popup_page())
  withr::defer(b$close())
  t1 <- rect(b, "span.tt")
  mouse(b, (t1$l + t1$r) / 2, (t1$t + t1$b) / 2)
  expect_match(popup_text(b), "alpha")
  expect_equal(
    js_eval(b, "document.querySelector('span.tt').getAttribute('aria-describedby')"),
    js_eval(b, "document.querySelector('.tt-popup').id")
  )
  r2 <- js_eval(b, paste0(
    "(function(){var r=document.querySelectorAll('span.tt')[1]",
    ".getBoundingClientRect();return {l:r.left,r:r.right,t:r.top,b:r.bottom};})()"
  ))
  mouse(b, (r2$l + r2$r) / 2, (r2$t + r2$b) / 2)
  expect_equal(popup_count(b), 1L)
  expect_match(popup_text(b), "beta")
})

test_that("Shift+Tab from the first popup link returns focus to the term", {
  b <- open_session(popup_page())
  withr::defer(b$close())
  js_eval(b, "document.querySelector('span.tt').focus(); 1")
  Sys.sleep(0.1)
  key(b, "Tab", "Tab", 9L)
  expect_equal(js_eval(b, "document.activeElement.id"), "linkA")
  key(b, "Tab", "Tab", 9L, shift = TRUE)
  expect_true(js_eval(
    b, "document.activeElement === document.querySelector('span.tt')"
  ))
  expect_equal(popup_count(b), 1L)
})
