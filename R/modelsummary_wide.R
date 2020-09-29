#' internal tidy method
#'
#' @export
#' @keywords internal
tidy.modelsummary_internal_coef_groups <- function(x) x$tidy

#' internal glance method
#'
#' @export
#' @keywords internal
glance.modelsummary_internal_coef_groups <- function(x) x$glance

#' Beautiful, customizable summaries of statistical models
#'
#' `modelsummary_wide` is a specialized function to display groups of
#' parameters from a single model in separate columns. This can be useful, for
#' example, to display the different levels of coefficients in a multinomial
#' regression model (e.g., `nnet::multinom`). 
#'
#' @param model a single model object with "grouped" terms.
#' @param coef_group the name of the coefficient groups to use as columns (NULL or character). See details. 
#' @inheritParams modelsummary
#' @return a regression table in a format determined by the `output` argument.
#' @details If `coef_group` is NULL, `modelsummary` tries to guess the correct
#'   coefficient group identifier. To be valid, this identifier must be a column
#'   in the data.frame produced by `tidy(model)`. Note: you may have to load the
#'   `broom` or `broom.mixed` package before executing `tidy(model)`.
#' @examples
#' \dontrun{
#'
#'
#' }
#' @export
modelsummary_wide <- function(model,
  output = "default",
  fmt = '%.3f',
  statistic = 'std.error',
  statistic_override = NULL,
  statistic_vertical = TRUE,
  conf_level = 0.95,
  stars = FALSE,
  coef_group = NULL,
  coef_map = NULL,
  coef_omit = NULL,
  coef_rename = NULL,
  gof_map = NULL,
  gof_omit = NULL,
  add_rows = NULL,
  title = NULL,
  notes = NULL,
  estimate = 'estimate',
  ...) {

  # local helpers split a single model into a list of models

  coef_groups <- function(model, coef_group, statistic, conf_level, ...) {

    # tidy
    if (statistic == "conf.int") {
      out <- generics::tidy(model, conf.int=TRUE, conf.level=conf_level, ...)
    } else {
      out <- generics::tidy(model, conf.int=TRUE, conf.level=conf_level, ...)
    }

    # guess coef_group
    if (is.null(coef_group)) {
      if ("y.level" %in% colnames(out)) {
        coef_group <- "y.level"
      } else if ("group" %in% colnames(out)) {
        coef_group <- "group"
      } else {
        stop("You must specify a valid character value for the `coef_group` argument. To find this value for your type of model, load the `broom` and/or `broom.mixed` libraries, then call `tidy(model)` on your model object. The `coef_group` value must be a column in the resulting data.frame. This column includes identifiers which determine which coefficients appear in which columns of your table.") 
      }
    }

    # glance
    gl <- gl_empty <- generics::glance(model, ...)
    for (i in seq_along(gl_empty)) {
      gl_empty[[i]] <- ""
    }

    # split and create placeholder lists extractable by the *.coef_groups methods
    out <- split(out, out[[coef_group]])
    for (i in seq_along(out)) {
      if (i == 1) {
        out[[i]] <- list(glance = gl, tidy = out[[i]])
      } else {
        out[[i]] <- list(glance = gl_empty, tidy = out[[i]])
      }
      class(out[[i]]) <- c("modelsummary_internal_coef_groups", class(out[[i]]))
    }
    out
  }

  models <- coef_groups(
    model=model, 
    coef_group=coef_group, 
    conf_level=conf_level,
    statistic=statistic,
    ...)

  modelsummary(models,
    output = output,
    fmt = fmt,
    statistic = statistic,
    statistic_override = statistic_override,
    statistic_vertical = statistic_vertical,
    conf_level = conf_level,
    stars = stars,
    coef_map = coef_map,
    coef_omit = coef_omit,
    coef_rename = coef_rename,
    gof_map = gof_map,
    gof_omit = gof_omit,
    add_rows = add_rows,
    title = title,
    notes = notes,
    estimate = estimate,
    ...) 

}
