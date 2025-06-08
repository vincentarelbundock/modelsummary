rm(list = ls())
suppressWarnings(modelsummary::config_modelsummary(reset = TRUE))

ON_CRAN <- !identical(Sys.getenv("R_NOT_CRAN"), "true")
ON_GH <- identical(Sys.getenv("R_GH"), "true")
ON_CI <- isTRUE(ON_CRAN) || isTRUE(ON_GH)
ON_WINDOWS <- isTRUE(Sys.info()[["sysname"]] == "Windows")
ON_OSX <- isTRUE(Sys.info()[["sysname"]] == "Darwin")

requiet <- function(package) {
  suppressMessages(suppressWarnings(suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )))
}

random_string <- function() {
  paste(sample(letters, 30, replace = TRUE), collapse = "")
}

compare_files <- function(x, y) {
  known <- digest::digest(x, file = TRUE)
  unknown <- digest::digest(y, file = TRUE)
  expect_equivalent(known, unknown)
}

print.custom_html_string <- function(x, ...) {
  cat(x, "\n", sep = "", ...)
  invisible(x)
}

strip_random_sequential <- function(x, stem = "tinytable_css_") {
  pattern <- paste0(stem, "\\w+\\b")
  match_list <- lapply(x, function(s) {
    regmatches(s, gregexpr(pattern, s, perl = TRUE))[[1]]
  })
  all_matches <- unlist(match_list)
  unique_matches <- unique(all_matches)
  new_ids <- sprintf("tinytable_css_%02d", seq_along(unique_matches))
  replaced <- x
  for (i in seq_along(unique_matches)) {
    replaced <- gsub(unique_matches[i], new_ids[i], replaced, perl = TRUE)
  }
  replaced
}
strip_random <- function(x) {
  for (stem in c(
    "tinytable_css_",
    "tinytable_(?!css)",
    "styleCell_",
    "spanCell_",
    "insertSpanRow",
    "styleHeaderCell_",
    "tinytable/"
  )) {
    x <- strip_random_sequential(x, stem)
  }
  x
}
options(tinysnapshot_fn_current = strip_random)
options(tinysnapshot_fn_target = strip_random)

print_html <- function(x) {
  set.seed(1024)
  if (inherits(x, "gt_tbl")) {
    x <- gt::as_raw_html(x)
    x <- gsub('div id="\\w+"', "", x)
    x <- gsub(".*<table", "<table", x)
    class(x) <- c("custom_html_string", "character")
    return(x)
  }
  if (inherits(x, "tinytable")) {
    x <- tinytable::save_tt(x, output = "html")
  }
  class(x) <- c("custom_html_string", "character")
  return(x)
}


requiet("tinytest")
requiet("tinysnapshot")
options(width = 10000)
options("tinysnapshot_device" = "svglite")
