#' Extract goodness-of-fit statistics a tidy format.
#'
#' A unified approach to extract results from a wide variety of models. 
#'
#' @param vcov_type string vcov type to add at the bottom of the table
#' @inheritParams get_estimates
#' @inheritParams modelsummary
#' @returns A dataframe with the goodness-o-fit statistics.
#'          The `backend` attribute indicates the backend used to extract them.
#'          Moreover, for some models `get_gof` attaches useful attributes to the output. 
#'          You can access this information by calling the `attributes` function `attributes(get_estimates(model))`.
#' @export
get_gof <- function(model, gof_function = NULL, vcov_type = NULL, ...) {
  # secret argument passed internally
  # gof_map = NULL: no value supplied by the user
  # gof_map = NA: the user explicitly wants to exclude everything
  dots <- list(...)
  if (isTRUE(is.na(dots$gof_map))) return(NULL)

  checkmate::assert_function(gof_function, null.ok = TRUE)

  # priority
  get_priority <- getOption("modelsummary_get", default = "easystats")
  checkmate::assert_choice(
    get_priority,
    choices = c("broom", "easystats", "parameters", "performance", "all")
  )

  if (get_priority %in% c("all", "broom")) {
    funs <- list("broom"=get_gof_broom, "parameters"=get_gof_parameters)
  } else {
    funs <- list("parameters"=get_gof_parameters, "broom"=get_gof_broom)
  }

  warning_msg <- NULL

  gof <- NULL

  for (f in names(funs)) {
    if (get_priority == "all") {
      tmp <- funs[[f]](model, ...)
      if (!is.null(tmp)) {
        attr(tmp, "backend") <- f
      }
      if (
        inherits(tmp, "data.frame") &&
          inherits(gof, "data.frame")
      ) {
        idx <- !tolower(colnames(tmp)) %in% tolower(colnames(gof))
        tmp <- tmp[, idx, drop = FALSE]
        if (ncol(tmp) > 0) {
          gof <- bind_cols(gof, tmp)
        }
      } else if (inherits(tmp, "data.frame")) {
        gof <- tmp
      } else {
        warning_msg <- c(warning_msg, tmp)
      }
    } else {
      if (!inherits(gof, "data.frame")) {
        gof <- funs[[f]](model, ...)
        if (!is.null(gof)) {
          attr(gof, "backend") <- f
        }
      }
    }
  }

  # vcov_type: nothing if unnamed matrix, vector, or function
  if (
    is.character(vcov_type) && !vcov_type %in% c("matrix", "vector", "function")
  ) {
    gof$vcov.type <- vcov_type
  }

  # internal customization by modelsummary
  gof_custom_df <- glance_custom_internal(
    model,
    vcov_type = vcov_type,
    gof = gof,
    gof_function = gof_function
  )
  if (!is.null(gof_custom_df) && is.data.frame(gof)) {
    for (n in colnames(gof_custom_df)) {
      # modelsummary's vcov argument has precedence
      # mainly useful to avoid collision with `fixest::glance_custom`
      overwriteable <- c("IID", "Default", "")
      if (
        is.null(vcov_type) ||
          n != "vcov.type" ||
          gof[["vcov.type"]] %in% overwriteable
      ) {
        gof[[n]] <- gof_custom_df[[n]]
      }
    }
  }

  # glance_custom (vcov_type arg is needed for glance_custom.fixest)
  gof_custom_df <- glance_custom(model)
  if (!is.null(gof_custom_df) && is.data.frame(gof)) {
    for (n in colnames(gof_custom_df)) {
      # modelsummary's vcov argument has precedence
      # mainly useful to avoid collision with `fixest::glance_custom`
      if (is.null(vcov_type) || n != "vcov.type") {
        gof[[n]] <- gof_custom_df[[n]]
      }
    }
  }

  # gof_function function supplied directly by the user
  if (is.function(gof_function)) {
    if (!"model" %in% names(formals(gof_function))) {
      msg <- "`gof_function` must accept an argument named 'model'."
      insight::format_error(msg)
    }
    tmp <- try(gof_function(model = model))
    if (
      !isTRUE(checkmate::check_data_frame(tmp, nrows = 1, col.names = "unique"))
    ) {
      msg <- "`gof_function` must be a function which accepts a model and returns a 1-row data frame with unique column names."
      insight::format_error(msg)
    } else {
      for (n in names(tmp)) {
        gof[[n]] <- tmp[[n]]
      }
    }
  }

  # drop NA gof: this allows us to drop them with glance_custom
  for (i in rev(seq_along(gof))) {
    if (isTRUE(is.na(gof[[i]]))) {
      gof[[i]] <- NULL
    }
  }

  # uniform types. Important for `mice` compatibility
  for (col in colnames(gof)) {
    if (inherits(gof[[col]], "logLik")) {
      gof[[col]] <- as.numeric(gof[[col]])
    }
  }

  if (inherits(gof, "data.frame")) {
    return(gof)
  }

  warning(
    sprintf(
      '`modelsummary could not extract goodness-of-fit statistics from a model
of class "%s". The package tried a sequence of 2 helper functions:

performance::model_performance(model)
broom::glance(model)

One of these functions must return a one-row `data.frame`. The `modelsummary` website explains how to summarize unsupported models or add support for new models yourself:

https://modelsummary.com/vignettes/modelsummary.html',
      class(model)[1]
    ),
    call. = FALSE
  )
}


#' Extract goodness-of-fit statistics from a single model using the
#' `broom` package or another package with package which supplies a
#' method for the `generics::glance` generic.
#'
#' @keywords internal
get_gof_broom <- function(model, ...) {
  insight::check_if_installed("broom")

  out <- suppressWarnings(try(
    broom::glance(model, ...),
    silent = TRUE
  ))

  if (!inherits(out, "data.frame")) {
    return("`broom::glance(model)` did not return a data.frame.")
  }

  if (nrow(out) > 1) {
    return("`broom::glance(model)` returned a data.frame with more than 1 row.")
  }

  return(out)
}


#' Extract goodness-of-fit statistics from a single model using
#' the `performance` package
#'
#' @keywords internal
get_gof_parameters <- function(model, ...) {
  dots <- list(...)

  mi <- hush(tryCatch(
    insight::model_info(model),
    error = function(e) NULL,
    warning = function(e) NULL
  ))

  if (isTRUE(dots[["metrics"]] == "none")) {
    return(NULL)
  }

  args <- c(list(model, verbose = FALSE), dots)

  if (!"metrics" %in% names(dots)) {
    if (isTRUE(mi[["is_bayesian"]])) {
      # this is the list of "common" metrics in `performance`
      # documentation, but their code includes R2_adj, which produces
      # a two-row glance and gives us issues.
      msg <- format_msg(
        '`modelsummary` uses the `performance` package to extract goodness-of-fit
            statistics from models of this class. You can specify the statistics you wish
            to compute by supplying a `metrics` argument to `modelsummary`, which will then
            push it forward to `performance`. Acceptable values are: "all", "common",
            "none", or a character vector of metrics names. For example: `modelsummary(mod,
            metrics = c("RMSE", "R2")` Note that some metrics are computationally
            expensive. See `?performance::performance` for details.'
      )
      warn_once(msg, "performance_gof_expensive")
      metrics <- c("RMSE", "LOOIC", "WAIC")
    } else if (
      inherits(model, "fixest") &&
        isTRUE(utils::packageVersion("insight") < "0.17.1.7")
    ) {
      args[["metrics"]] <- c("RMSE", "R2", "R2_adj")
    } else {
      args[["metrics"]] <- "common"
    }
  }

  fun <- performance::model_performance
  out <- hush(tryCatch(do.call("fun", args), error = function(e) NULL))

  # sanity
  if (!inherits(out, "data.frame") && isTRUE(dots$metrics != "none")) {
    return(
      "`performance::model_performance(model)` did not return a data.frame."
    )
  }

  if (inherits(out, "data.frame") && nrow(out) > 1) {
    return(
      "`performance::model_performance(model)` returned a data.frame with more than 1 row."
    )
  }

  # cleanup
  out <- hush(insight::standardize_names(out, style = "broom"))

  # nobs
  n_obs <- hush(tryCatch(insight::n_obs(model)[1], error = function(e) NULL))
  out[["nobs"]] <- n_obs

  return(out)
}
