#' Extract model gof A mostly internal function with some potential uses
#' outside.
#'
#' @inheritParams get_estimates
#' @param vcov_type string vcov type to add at the bottom of the table
#' @export
get_gof <- function(model, vcov_type = NULL, ...) {

    # secret argument passed internally
    # gof_map = NULL: no value supplied by the user
    # gof_map = NA: the user explicitly wants to exclude everything
    dots <- list(...)
    if (isTRUE(is.na(dots$gof_map))) return(NULL)

    # priority
    get_priority <- getOption("modelsummary_get", default = "easystats")
    checkmate::assert_choice(
      get_priority,
      choices = c("broom", "easystats", "parameters", "performance", "all"))

    if (get_priority %in% c("all", "broom")) {
        funs <- list(get_gof_broom, get_gof_parameters)
    } else {
        funs <- list(get_gof_parameters, get_gof_broom)
    }

    warning_msg <- NULL

    gof <- NULL

    for (f in funs) {
        if (get_priority == "all") {
            tmp <- f(model, ...)
            if (inherits(tmp, "data.frame") &&
                inherits(gof, "data.frame")) {
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
                gof <- f(model, ...)
            }
        }
    }

    # vcov_type: nothing if unnamed matrix, vector, or function
    if (is.character(vcov_type) && !vcov_type %in% c("matrix", "vector", "function")) {
        gof$vcov.type <- vcov_type
    }

    # internal customization by modelsummary
    gof_custom <- glance_custom_internal(model, vcov_type = vcov_type, gof = gof)
    if (!is.null(gof_custom) && is.data.frame(gof)) {
        for (n in colnames(gof_custom)) {
            # modelsummary's vcov argument has precedence
            # mainly useful to avoid collision with `fixest::glance_custom`
            if (is.null(vcov_type) || n != "vcov.type") {
                gof[[n]] <- gof_custom[[n]]
            }
        }
    }

    # glance_custom (vcov_type arg is needed for glance_custom.fixest)
    gof_custom <- glance_custom(model)
    if (!is.null(gof_custom) && is.data.frame(gof)) {
        for (n in colnames(gof_custom)) {
            # modelsummary's vcov argument has precedence
            # mainly useful to avoid collision with `fixest::glance_custom`
            if (is.null(vcov_type) || n != "vcov.type") {
                gof[[n]] <- gof_custom[[n]]
            }
        }
    }

    if (inherits(gof, "data.frame")) {
        return(gof)
    }

    warning(sprintf(
'`modelsummary could not extract goodness-of-fit statistics from a model
of class "%s". The package tried a sequence of 2 helper functions:

performance::model_performance(model)
broom::glance(model)

One of these functions must return a one-row `data.frame`. The `modelsummary` website explains how to summarize unsupported models or add support for new models yourself:

https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html',
class(model)[1]),
            call. = FALSE)
}


#' Extract goodness-of-fit statistics from a single model using the
#' `broom` package or another package with package which supplies a
#' method for the `generics::glance` generic.
#'
#' @keywords internal
get_gof_broom <- function(model, ...) {

  out <- suppressWarnings(try(
    broom::glance(model, ...),
    silent = TRUE))

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

  # user explicitly supplies a "metrics" argument
  if ("metrics" %in% names(list(...))) {
    # "none" is a custom option here
    if (isTRUE(dots$metrics == "none")) {
      out <- NULL

    } else {
      out <- suppressMessages(suppressWarnings(try(
        performance::model_performance(model, verbose = FALSE, ...))))
    }

  # defaults for stan models: exclude r2_adjusted because veeeery slow
  } else {
    if (inherits(model, "stanreg") ||
        inherits(model, "brmsfit") ||
        inherits(model, "stanmvreg") ||
        inherits(model, "merMod")) {
      # this is the list of "common" metrics in `performance`
      # documentation, but their code includes R2_adj, which produces
      # a two-row glance and gives us issues.
      msg <- '`modelsummary` uses the `performance` package to extract goodness-of-fit statistics from models of this class. You can specify the statistics you wish to compute by supplying a `metrics` argument to `modelsummary`, which will then push it forward to `performance`. Acceptable values are: "all", "common", "none", or a character vector of metrics names. For example: `modelsummary(mod, metrics = c("RMSE", "R2")` Note that some metrics are computationally expensive. See `?performance::performance` for details.'
      warn_once(msg, "performance_gof_expensive")
      metrics <- c("RMSE", "LOOIC", "WAIC")

    } else if (inherits(model, "fixest") && isTRUE(utils::packageVersion("insight") < "0.17.1.7")) {
      metrics <- c("RMSE", "R2", "R2_adj")

    } else {
      metrics <- "common"
    }
    out <- suppressMessages(suppressWarnings(try(
      performance::model_performance(model, verbose = FALSE, metrics = metrics, ...), silent = TRUE)))
  }

  # sanity
  if (!inherits(out, "data.frame") && isTRUE(dots$metrics == "none")) {
    return("`performance::model_performance(model)` did not return a data.frame.")
  }

  if (is.null(out)) return(out)

  if (inherits(out, "data.frame") && nrow(out) > 1) {
    return("`performance::model_performance(model)` returned a data.frame with more than 1 row.")
  }

  # cleanup
  out <- insight::standardize_names(out, style = "broom")

  # nobs
  if (inherits(out, "data.frame")) {
    mi <- try(insight::model_info(model), silent = TRUE)
    if (isTRUE("n_obs" %in% names(mi))) {
      out$nobs <- mi$n_obs
    }
  }

  return(out)
}
