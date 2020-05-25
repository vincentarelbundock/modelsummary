#' Deprecated function
#'
#' The `gt::as_latex` function does not (yet) produce compilable LaTeX code.
#' This function used to clean up LaTeX output to allow compilation to PDF.
#' This function is now deprecated since `modelsummary` currently supports
#' `kableExtra`, which has a mature LaTeX rendering engine.
#' @param ... catch everything
#' @export
knit_latex <- function(...) {
    stop("The `clean_latex` function is deprecated. If you want to use a
         `modelsummary` table inside an Rmarkdown document, call
         `msummary(models)`. If you want to save a latex file, call
         `msummary(models, output='filename.tex')`. If you wnat to print LaTeX
         code to the console, call `msummary(models, output='latex')`.")
}

#' Deprecated function
#'
#' The `gt::as_latex` function does not (yet) produce compilable LaTeX code.
#' This function used to clean up LaTeX output to allow compilation to PDF.
#' This function is now deprecated since `modelsummary` currently supports
#' `kableExtra`, which has a mature LaTeX rendering engine.
#' @param ... catch everything
#' @export
clean_latex <- function(...) {
    stop("The `clean_latex` function is deprecated. If you want to use a
         `modelsummary` table inside an Rmarkdown document, call
         `msummary(models)`. If you want to save a latex file, call
         `msummary(models, output='filename.tex')`. If you wnat to print LaTeX
         code to the console, call `msummary(models, output='latex')`.")
}
