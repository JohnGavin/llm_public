#' Function call analysis (static AST analysis)
#'
#' Extract function calls and definitions from R source files, then summarise
#' them as a frequency table or a one-level call network.
#'
#' @name function_analysis
#' @family function analysis
NULL

#' Extract all function calls from an R file
#'
#' Parses `file` and walks the abstract syntax tree, recording the name of
#' every called function. Common operators and control-flow keywords (such as
#' `<-`, `if`, `c` and `list`) are skipped.
#'
#' @param file Path to an R file.
#' @return A tibble with columns `file` (base name), `call` and
#'   `has_namespace` (`TRUE` when the call is written `pkg::fn`). Zero rows if
#'   the file cannot be parsed (a warning is raised) or contains no calls.
#' @family function analysis
#' @export
#' @examples
#' f <- tempfile(fileext = ".R")
#' writeLines("x <- stats::sd(rnorm(10))", f)
#' extract_file_calls(f)
extract_file_calls <- function(file) {
  checkmate::assert_string(file, min.chars = 1L)
  checkmate::assert_file_exists(file)

  empty <- tibble::tibble(
    file = character(), call = character(), has_namespace = logical()
  )

  # Top-level parse failure: warn so the caller knows the file was skipped
  parsed <- tryCatch(parse(file), error = function(e) {
    cli::cli_warn("Failed to parse {.file {file}}: {conditionMessage(e)}")
    NULL
  })
  if (is.null(parsed)) return(empty)

  calls <- character()

  skip_ops <- c("<-", "<<-", "=", "{", "(", "if", "for",
                "while", "repeat", "function", "~", "!",
                "&&", "||", "&", "|", "+", "-", "*", "/",
                "^", "%%", "%>%", "|>", "$", "@", "[",
                "[[", "c", "list", "return")

  walk_ast <- function(node) {
    if (is.call(node)) {
      fn_name <- paste(deparse(node[[1]], width.cutoff = 500L), collapse = "")
      if (nzchar(fn_name) && !fn_name %in% skip_ops) {
        calls <<- c(calls, fn_name)
      }
      children <- as.list(node)[-1L]
    } else if (is.recursive(node) && !is.function(node)) {
      children <- as.list(node)
    } else {
      return(invisible(NULL))
    }
    for (i in seq_along(children)) {
      # Empty arguments such as the missing index in x[, 1] cannot be walked
      if (identical(children[[i]], quote(expr = ))) next
      walk_ast(children[[i]])
    }
    invisible(NULL)
  }

  walk_ast(parsed)

  if (length(calls) == 0L) return(empty)

  tibble::tibble(
    file = basename(file),
    call = calls,
    has_namespace = grepl("::", calls, fixed = TRUE)
  )
}

#' Extract function definitions from R source files
#'
#' Finds top-level `name <- function(...)` assignments in every `.R` file
#' under `r_dir`.
#'
#' @param r_dir Path to a directory of R source files (searched recursively).
#' @return A named list: function name -> `list(body, file)`, where `body` is
#'   the function body (a language object) and `file` the base name of the
#'   defining file.
#' @family function analysis
#' @export
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' writeLines("add <- function(a, b) a + b", file.path(d, "add.R"))
#' names(extract_function_defs(d))
extract_function_defs <- function(r_dir) {
  checkmate::assert_directory_exists(r_dir)

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE,
                        recursive = TRUE)
  defs <- list()

  for (file in r_files) {
    parsed <- tryCatch(parse(file), error = function(e) {
      cli::cli_warn("Failed to parse {.file {file}}: {conditionMessage(e)}")
      NULL
    })
    if (is.null(parsed)) next

    for (expr in parsed) {
      # Match: fn_name <- function(...) { ... }
      if (is.call(expr) && length(expr) >= 3 &&
          identical(expr[[1]], as.name("<-")) &&
          is.name(expr[[2]]) &&
          is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) {
        fn_name <- as.character(expr[[2]])
        defs[[fn_name]] <- list(
          body = body(eval(expr[[3]], envir = baseenv())),
          file = basename(file)
        )
      }
    }
  }
  defs
}

#' Classify a function call as package, base, or internal
#'
#' @param call_name Character string of the function call, for example
#'   `"dplyr::filter"` or `"paste"`.
#' @param our_functions Character vector of functions defined in the code
#'   under analysis. A call matching one of these is `"internal"`.
#' @return A character string: the package name for `pkg::fn` calls,
#'   `"internal"`, `"base"`, one of the default-attached packages
#'   (`"utils"`, `"stats"`, `"grDevices"`, `"graphics"`, `"methods"`), or
#'   `"unknown"`.
#' @family function analysis
#' @export
#' @examples
#' classify_call("dplyr::filter")
#' classify_call("paste")
#' classify_call("my_helper", our_functions = "my_helper")
classify_call <- function(call_name, our_functions = character()) {
  checkmate::assert_string(call_name)
  checkmate::assert_character(our_functions, any.missing = FALSE)

  if (grepl("::", call_name, fixed = TRUE)) {
    sub("::.*", "", call_name)
  } else if (call_name %in% our_functions) {
    "internal"
  } else if (exists(call_name, envir = baseenv(), inherits = FALSE)) {
    "base"
  } else {
    # Check the packages attached by default
    for (pkg in c("utils", "stats", "grDevices", "graphics", "methods")) {
      if (exists(call_name, envir = asNamespace(pkg), inherits = FALSE)) {
        return(pkg)
      }
    }
    "unknown"
  }
}

#' Build a function frequency table
#'
#' Counts how often each function is called across all `.R` files in
#' `r_dir`, and in how many files.
#'
#' @param r_dir Path to a directory of R source files (searched recursively).
#' @return A tibble with columns `call`, `package`, `n_calls` and `n_files`,
#'   sorted by `n_calls` descending. An empty tibble if no calls are found.
#' @family function analysis
#' @export
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' writeLines("f <- function(x) paste(x, nchar(x))", file.path(d, "f.R"))
#' build_frequency_table(d)
build_frequency_table <- function(r_dir) {
  checkmate::assert_directory_exists(r_dir)

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE,
                        recursive = TRUE)
  all_calls <- do.call(rbind, lapply(r_files, extract_file_calls))
  if (is.null(all_calls) || nrow(all_calls) == 0L) return(tibble::tibble())

  our_fns <- names(extract_function_defs(r_dir))

  all_calls$package <- vapply(all_calls$call, classify_call,
                              character(1), our_functions = our_fns,
                              USE.NAMES = FALSE)

  all_calls |>
    dplyr::count(call, package, name = "n_calls") |>
    dplyr::mutate(
      n_files = vapply(call, function(fn) {
        length(unique(all_calls$file[all_calls$call == fn]))
      }, integer(1), USE.NAMES = FALSE)
    ) |>
    dplyr::arrange(dplyr::desc(n_calls))
}

#' Build a call network
#'
#' One level deep: for each function defined under `r_dir`, the distinct
#' external functions its body calls (calls to the other functions defined in
#' the same code are left out).
#'
#' @param r_dir Path to a directory of R source files (searched recursively).
#' @return A tibble with columns `from`, `to` and `to_package`. Zero rows if
#'   no function definitions are found.
#' @family function analysis
#' @export
#' @examples
#' d <- tempfile()
#' dir.create(d)
#' writeLines(c(
#'   "helper <- function(x) x + 1",
#'   "main <- function(x) paste(helper(x), stats::sd(x))"
#' ), file.path(d, "code.R"))
#' build_call_network(d)
build_call_network <- function(r_dir) {
  checkmate::assert_directory_exists(r_dir)

  defs <- extract_function_defs(r_dir)
  if (length(defs) == 0L) {
    return(tibble::tibble(
      from = character(), to = character(), to_package = character()
    ))
  }

  our_fn_names <- names(defs)

  edges <- lapply(names(defs), function(fn_name) {
    # Write the body to a temp file so it can be parsed like any other file
    body_text <- deparse(defs[[fn_name]]$body)

    tf <- tempfile(fileext = ".R")
    writeLines(body_text, tf)
    on.exit(unlink(tf), add = TRUE)

    calls <- extract_file_calls(tf)
    if (nrow(calls) == 0L) return(NULL)

    calls$package <- vapply(calls$call, classify_call,
                            character(1), our_functions = our_fn_names,
                            USE.NAMES = FALSE)
    # Keep only external calls (not internal to the analysed code)
    external <- calls[calls$package != "internal", ]
    if (nrow(external) == 0L) return(NULL)

    unique_calls <- unique(external[, c("call", "package")])
    tibble::tibble(
      from = fn_name,
      to = unique_calls$call,
      to_package = unique_calls$package
    )
  })

  edges <- Filter(Negate(is.null), edges)
  if (length(edges) == 0L) {
    return(tibble::tibble(
      from = character(), to = character(), to_package = character()
    ))
  }
  do.call(rbind, edges)
}
