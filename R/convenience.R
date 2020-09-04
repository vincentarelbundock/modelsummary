# This file includes convenience functions for `modelsummary`. These functions
# accomplish tasks we can already be done using functions from the `gt`
# package. They are only here for *convenience*, and not because they
# accomplish core functions of the `modelsummary` package. Since they are only
# convenience functions, they will not be exported to the package
# documentation. Moreover, they are only semi-supported. This means that the
# maintainer does not plan to improve those functions unless someone
# contributes a pull request on Github. Bug fixes may also take longer than for
# core functions.

#' A convenience function to render markdown to html in row and column labels
#' 
#' @param tab a `gt` table object
#' @param position character string determines wither row, column or both
#'   labels should be rendered.
#' @keywords internal
#' @note This function only works for HTML output, since the `gt` render tools
#' are less developed for LaTeX and RTF output.
fmt_labels_md <- function(tab, position = c('both', 'row', 'column')) {
    out <- tab
    if (match.arg(position) %in% c('both', 'row')) {
        out <- out %>% gt::fmt_markdown(columns = 1)
    }
    if (match.arg(position) %in% c('both', 'column')) {
        f <- function(x) stats::setNames(lapply(names(x$`_data`), gt::md), names(x$`_data`))
        out <- out %>% gt::cols_label(.list = f(.))
    }
    return(out)
}


#' An unsupported and unexported function to tidy a lfe::felm instrumental
#' variable model
#'
#' @keywords internal
tidy_felm_iv <- function(x, ...) {
  # we have changed the class of the model to "iv". Now, we want to use the
  # standard broom:::tidy.felm function, so we switch it back.
  class(x) <- "felm"

  # extract results
  stage2 <- generics::tidy(x, ...)
  stage1 <- generics::tidy(x$stage1, ...) %>%
            # keep only the stage1 terms if they are not in stage2
            dplyr::filter(!(term %in% stage2$term)) %>%
            # label
            dplyr::mutate(term = paste("Stage 1", term))

  # combine stage1 and stage2 results
  out <- dplyr::bind_rows(stage1, stage2)
  out
}


#' An unsupported and unexported function to clean the term names from a lfe::felm
#' instrumental variable model
#'
#' @keywords internal
coef_map_felm_iv <- function(x) {
  # this function must work for models and lists of models, so we define a
  # couple of helpers functions to extract term names from both stages of
  # estimation.
  get_stage2 <- function(m) row.names(m$coefficients)
  get_stage1 <- function(m) row.names(m$stage1$coefficients)

  # single model
  if (inherits(x, "felm")) {
    stage2 <- get_stage2(x)
    stage1 <- get_stage1(x)

  # list of models
  } else {
    stage2 <- unique(unlist(lapply(x, get_stage2)))
    stage1 <- unique(unlist(lapply(x, get_stage1)))
  }

  # discard stage1 terms if they are also in stage2
  stage1 <- setdiff(stage1, stage2)

  # label terms as Stage 1-specific
  stage1 <- paste("Stage 1", stage1)

  # display stage1 results after stage2
  out <- c(stage2, stage1)
  stats::setNames(out, out)
}
