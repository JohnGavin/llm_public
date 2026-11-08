#' Hover-popup helper: generate a popup span
#'
#' Produces an HTML `<span>` element that carries its popup content in a
#' `data-tippy-content` attribute. Pair it with [tt_dependency()], which
#' supplies a small stylesheet and script that turn those spans into hover
#' (and keyboard-focus) popups. The attribute name follows the Tippy.js
#' convention, so a page that already loads Tippy.js can use the spans
#' without [tt_dependency()].
#'
#' The `body` argument is HTML-escaped so that double-quotes inside the
#' popup content do not break the attribute. Authors may include HTML tags
#' (`<b>`, `<a href="...">`, etc.); they are rendered as HTML when the popup
#' opens.
#'
#' @param term Character scalar. The visible text the reader hovers over.
#'   Not HTML-escaped; it may itself contain Markdown or inline HTML that
#'   knitr/Quarto processes before `cat()`.
#' @param body Character scalar. HTML body for the popup. Must contain at
#'   least two sentences and at least one `<a href="...">` anchor.
#'   Double-quotes in `body` are escaped automatically (`"` becomes
#'   `&quot;`). Ampersands are also escaped (`&` becomes `&amp;`), so
#'   pass literal characters rather than pre-escaped entities.
#'
#' @return Character scalar. An HTML string suitable for passing to
#'   `cat()` or `htmltools::HTML()` inside a knitr chunk with
#'   `results = "asis"`.
#'
#' @seealso [tt_dependency()] for the stylesheet and script.
#' @export
#' @examples
#' # Basic usage: output via cat() in a chunk with results = "asis"
#' cat(tt(
#'   "LLM",
#'   paste0(
#'     "<b>Large Language Model</b>. A neural network trained on large text ",
#'     "corpora to generate, summarise, and classify text. See ",
#'     "<a href='https://en.wikipedia.org/wiki/Large_language_model'>",
#'     "Wikipedia</a>."
#'   )
#' ))
#'
#' # Inline use with htmltools::HTML()
#' htmltools::HTML(tt(
#'   "QA",
#'   paste0(
#'     "Quality assurance. Systematic checks applied to pipeline outputs. ",
#'     "See <a href='https://r-pkgs.org/testing-basics.html'>",
#'     "R packages testing guide</a>."
#'   )
#' ))
tt <- function(term, body) {
  checkmate::assert_string(term, min.chars = 1L)
  checkmate::assert_string(body, min.chars = 1L)

  # Validate: body must contain at least one <a href> anchor
  if (!grepl("<a[[:space:]]+href=", body)) {
    cli::cli_abort(c(
      "x" = "{.fn tt}: {.arg body} must contain at least one {.code <a href=\"...\">} anchor.",
      "i" = "Add an external reference link to the popup body."
    ))
  }

  # Validate: body must contain at least 2 sentences.
  # Strip HTML tags before counting sentence-terminal punctuation.
  body_text <- gsub("<[^>]+>", "", body)
  parts <- strsplit(body_text, "[.!?]+\\s*")[[1L]]
  n_sentences <- sum(nzchar(trimws(parts)))
  if (n_sentences < 2L) {
    cli::cli_abort(c(
      "x" = "{.fn tt}: {.arg body} must contain at least 2 sentences (found {n_sentences}).",
      "i" = "Provide at least two sentences of contextual explanation."
    ))
  }

  # Escape for the HTML attribute-value context:
  # 1. & must come first (avoid double-escaping)
  # 2. " would end the double-quoted attribute value
  escaped_body <- gsub("&", "&amp;", body, fixed = TRUE)
  escaped_body <- gsub('"', "&quot;", escaped_body, fixed = TRUE)

  sprintf(
    '<span class="tt" data-tippy-content="%s" tabindex="0">%s</span>',
    escaped_body,
    term
  )
}

#' HTML dependency for hover popups
#'
#' Returns an [htmltools::htmlDependency()] bundling a minimal stylesheet and
#' script (written for this package, no external library) that show the
#' `data-tippy-content` attribute of every `<span class="tt">` produced by
#' [tt()] as a popup on hover and on keyboard focus. Escape closes the popup.
#'
#' Attach it to a page with [htmltools::attachDependencies()], or in an R
#' Markdown / Quarto document by returning it from a chunk, e.g.
#' `htmltools::tagList(tt_dependency())`.
#'
#' The popup content is inserted as HTML, so only pass text you trust to
#' [tt()].
#'
#' @return An `html_dependency` object.
#' @seealso [tt()]
#' @export
#' @examples
#' dep <- tt_dependency()
#' dep$name
#' dep$script
tt_dependency <- function() {
  htmltools::htmlDependency(
    name = "llmpublic-hover-popup",
    version = as.character(utils::packageVersion("llmpublic")),
    src = system.file("hover-popup", package = "llmpublic"),
    script = "hover-popup.js",
    stylesheet = "hover-popup.css"
  )
}
