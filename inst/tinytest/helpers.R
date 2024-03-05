rm(list = ls())
suppressWarnings(modelsummary::config_modelsummary(reset = TRUE))

ON_CRAN <- !identical(Sys.getenv("R_NOT_CRAN"), "true")
ON_GH <- identical(Sys.getenv("R_GH"), "true")
ON_CI <- isTRUE(ON_CRAN) || isTRUE(ON_GH)
ON_WINDOWS <- isTRUE(Sys.info()[['sysname']] == "Windows")
ON_OSX <- isTRUE(Sys.info()[['sysname']] == "Darwin")

requiet <- function(package) {
  suppressMessages(suppressWarnings(suppressPackageStartupMessages(
    require(package, warn.conflicts = FALSE, character.only = TRUE)
  )))
}

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
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


print_html <- function(x) {
  if (inherits(x, "gt_tbl")) {
    x <- gt::as_raw_html(x)
    x <- gsub('div id="\\w+"', '', x)
    class(x) <- c("custom_html_string", "character")
    return(x)
  }
  if (inherits(x, "tinytable")) {
    x <- tinytable::save_tt(x, output = "html")
  }
  x <- gsub("tinytable_\\w+\\b", "tinytable", x)
  x <- gsub("styleCell_\\w+\\b", "tinytable", x)
  x <- gsub("insertSpanRow\\w+\\b", "tinytable", x)
  x <- gsub("styleHeaderCell_\\w+\\b", "tinytable", x)
  class(x) <- c("custom_html_string", "character")
  return(x)
}


requiet("tinytest")
requiet("tinysnapshot")
options(width = 10000)
options("tinysnapshot_device" = "svglite")