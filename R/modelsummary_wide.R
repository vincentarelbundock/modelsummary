#' Summary Tables for Models with Grouped Coefficients
#'
#' `modelsummary_wide` summarizes models with grouped coefficients. For
#' example, these groups could correspond to levels of a multinomial logit
#' outcome variable, or to parameters of a GAMLSS model. This function's
#' arguments are the same as in `modelsummary`, except for the `coef_group` and
#' the `stacking` arguments.
#'
#' @param coef_group the name of the coefficient groups to use as columns (NULL
#' or character). If `coef_group` is NULL, `modelsummary` tries to guess the
#' correct coefficient group identifier. To be valid, this identifier must
#' be a column in the data.frame produced by `get_estimates(model)`.
#' @param stacking direction in which models are stacked: "horizontal" or
#' "vertical"
#' @inheritParams modelsummary
#' @return a regression table in a format determined by the `output` argument.
#' @export
modelsummary_wide <- function(
  models,
  output      = "default",
  fmt         = 3,
  estimate    = "estimate",
  statistic   = "std.error",
  vcov        = NULL,
  conf_level  = 0.95,
  stars       = FALSE,
  coef_group  = NULL,
  coef_map    = NULL,
  coef_omit   = NULL,
  coef_rename = NULL,
  gof_map     = NULL,
  gof_omit    = NULL,
  add_rows    = NULL,
  align       = NULL,
  notes       = NULL,
  title       = NULL,
  stacking    = "horizontal",
  ...) {

  checkmate::assert_character(stacking, pattern = "^horizontal$|^vertical$")

  # models must be a list of models
  if (!'list' %in% class(models)) {
    models <- list(models)
  }

  # model names
  if (is.null(names(models))) {
    model_names <- paste('Model', 1:length(models))
  } else {
    model_names <- names(models)
  }
  model_names <- pad(model_names)

  # tidy
  if (!is.null(statistic) && statistic == "conf.int") {
    ti <- lapply(models, function(x)
                 get_estimates(x, conf_level = conf_level, ...))
  } else {
    ti <- lapply(models, function(x)
                 get_estimates(x, ...))
  }

  # glance
  gl <- lapply(models, get_gof)
  gl_wide <- gl

  # insert model names in glance and tidy frames
  for (i in seq_along(model_names)) {
    ti[[i]]$model <- model_names[i]
    gl[[i]]$model <- model_names[i]
    if (length(model_names) > 1) {
      colnames(gl_wide[[i]]) <- paste(model_names[i], colnames(gl_wide[[i]]))
    }
  }
  ti <- bind_rows(ti)
  gl <- bind_rows(gl)
  gl_wide <- bind_cols(gl_wide)

  # guess coef_group
  if (is.null(coef_group)) {
    coef_group <- intersect(c("y.level", "response", "group"), colnames(ti))[1]
    if (is.na(coef_group)) {
      stop("You must specify a valid character value for the `coef_group` argument. To find this value for your type of model, load the `broom` and/or `broom.mixed` libraries, then call `tidy(model)` on your model object. The `coef_group` value must be a column in the resulting data.frame. This column includes identifiers which determine which coefficients appear in which columns of your table.")
    }
  }

  # unique group names
  group_names <- unique(ti[[coef_group]])
  group_names <- as.character(group_names) # weird bug otherwise when groups are integers

  # vertical stacking: model_names are groups
  if (stacking == "vertical") {
    results <- list()
    for (g in group_names) {
      results[[g]] <- list()
      results[[g]]$tidy <- ti[ti[[coef_group]] == g, , drop = FALSE]
      results[[g]]$tidy$term <- paste(results[[g]]$tidy$model,
                                      results[[g]]$tidy$term)
      if (g == group_names[1]) {
        results[[g]]$glance <- gl_wide
      }
      class(results[[g]]) <- c("modelsummary_list", "list")
    }
  }

  # horizontal stacking: model_names are model/group combinations
  if (stacking == "horizontal") {
    results <- list()
    for (m in model_names) {
      first <- TRUE
      for (g in group_names) {

        # do not include model label in columns for single model tables, only
        # the response.
        if (length(model_names) > 1) {
          idx <- paste(m, g)
        } else {
          idx <- g
        }

        tmp_ti <- ti[ti[[coef_group]] == g & ti$model == m, , drop = FALSE]
        tmp_gl <- gl[gl$model == m, , drop = FALSE]
        tmp_gl$model <- NULL

        # skip missing response levels
        if (nrow(tmp_ti) > 0) {
          results[[idx]] <- list()
          results[[idx]]$tidy <- tmp_ti
          if (first) {
            results[[idx]]$glance <- tmp_gl
            first <- FALSE
          }
          class(results[[idx]]) <- c("modelsummary_list", "list")
        }
      }
    }
  }

  # output
  modelsummary(results,
    output      = output,
    fmt         = fmt,
    conf_level  = conf_level,
    stars       = stars,
    coef_map    = coef_map,
    coef_omit   = coef_omit,
    coef_rename = coef_rename,
    gof_map     = gof_map,
    gof_omit    = gof_omit,
    statistic   = statistic,
    vcov        = vcov,
    add_rows    = add_rows,
    title       = title,
    notes       = notes,
    estimate    = estimate,
    align       = align,
    ...)

}
