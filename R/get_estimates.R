#' Extract model estimates in a tidy format. 
#'
#' This is a mostly internal function which could be useful to users who want
#' a unified approach to extract results from a wide variety of models. For
#' some models `get_estimates` attaches useful attributes to the output. You
#' can access this information by calling the `attributes` function:
#' `attributes(get_estimates(model))`
#'
#' @inheritParams modelsummary
#' @param model a single model object
#' 
#' @export
get_estimates <- function(model, conf_level = .95, vcov = NULL, ...) {

    if (is.null(conf_level)) {
        conf_int <- FALSE
    } else {
        conf_int <- TRUE
    }

    # priority
    get_priority <- getOption("modelsummary_get", default = "easystats")
    checkmate::assert_choice(
      get_priority,
      choices = c("broom", "easystats", "parameters", "performance", "all"))

    if (get_priority %in% c("easystats", "parameters", "performance")) {
        funs <- list(get_estimates_parameters, get_estimates_broom)
    } else {
        funs <- list(get_estimates_broom, get_estimates_parameters)
    }

    warning_msg <- NULL
    out <- NULL

    for (f in funs) {
        if (!inherits(out, "data.frame") || nrow(out) == 0) {
            out <- f(model,
                    conf_int = conf_int,
                    conf_level = conf_level,
                    ...)
            if (is.character(out)) {
                warning_msg <- c(warning_msg, out)
            }
        }
    }

    if (!inherits(out, "data.frame")) {
      stop(sprintf(
        '`modelsummary could not extract the required information from a model
of class "%s". The package tried a sequence of 2 helper functions to extract
estimates:

parameters::parameters(model)
broom::tidy(model)

To draw a table, one of these commands must return a `data.frame` with a
column named "term". The `modelsummary` website explains how to summarize
unsupported models or add support for new models yourself:

https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html

These errors messages were generated during extraction:
%s',
        class(model)[1], paste(warning_msg, collapse = "\n")
      ))
    }

    # tidy_custom_internal (modelsummary customization avoids name conflict)
    out_custom <- tidy_custom_internal(model)
    if (inherits(out_custom, "data.frame") && nrow(out_custom) > 0) {
        if (!any(out_custom$term %in% out$term)) {
            warning('Elements of the "term" column produced by `tidy_custom` must match model terms. `tidy_custom` was ignored.',
                    call. = FALSE)
        } else {
            # R 3.6 doesn't deal well with factors
            out_custom$term <- as.character(out_custom$term)
            out$term <- as.character(out$term)
            out_custom <- out_custom[out_custom$term %in% out$term, , drop = FALSE]
            idx <- match(out_custom$term, out$term)
            for (n in colnames(out_custom)) {
                out[[n]][idx] <- out_custom[[n]]
            }
        }
    }

    # tidy_custom
    out_custom <- tidy_custom(model)
    if (inherits(out_custom, "data.frame") && nrow(out_custom) > 0) {
        if (!any(out_custom$term %in% out$term)) {
            warning('Elements of the "term" column produced by `tidy_custom` must match model terms. `tidy_custom` was ignored.',
                    call. = FALSE)
        } else {
            # R 3.6 doesn't deal well with factors
            out_custom$term <- as.character(out_custom$term)
            out$term <- as.character(out$term)
            out_custom <- out_custom[out_custom$term %in% out$term, , drop = FALSE]
            idx <- match(out_custom$term, out$term)
            for (n in colnames(out_custom)) {
                if (!n %in% colnames(out)) {
                    out[[n]] <- NA
                }
                out[[n]][idx] <- out_custom[[n]]
            }
        }
    }

    # fixest mods
    fixest_mod = inherits(model, "fixest") || inherits(model, "fixest_multi")

    # vcov override
    flag1 <- !is.null(vcov)
    flag2 <- isFALSE(all.equal(vcov, stats::vcov))
    flag3 <- !is.character(vcov)
    flag4 <- is.character(vcov) && length(vcov) == 1 &&
      (!vcov %in% c("classical", "iid", "constant") || fixest_mod)
    flag5 <- is.character(vcov) && length(vcov) > 1

    if (flag1 && (flag2 || flag3 || flag4 || flag5)) {

      # extract overridden estimates
      so <- get_vcov(
        model,
        vcov = vcov,
        conf_level = conf_level,
        ...)

      if (!is.null(so) && nrow(out) == nrow(so)) {
        # so overrides out, so we drop columns first
        idx <- c("group", "term", "response")
        good <- setdiff(colnames(out), colnames(so))
        good <- intersect(colnames(out), c(good, idx))
        out <- out[, good, drop = FALSE]
        # merge vcov and estimates
        idx <- Reduce("intersect", list(colnames(out), colnames(so), idx)) 
        out <- merge(out, so, by = idx, sort = FALSE)
      }
    }

    # term must be a character (not rounded with decimals when integer)
    out$term <- as.character(out$term)


    if (inherits(out, "data.frame")) {
        return(out)
    }
}


get_estimates_broom <- function(model, conf_int, conf_level, ...) {

    if (isTRUE(conf_int)) {
        out <- suppressWarnings(try(
            broom::tidy(model, conf.int = conf_int, conf.level = conf_level, ...),
            silent = TRUE))
    } else {
        out <- suppressWarnings(try(
            broom::tidy(model, conf.int = conf_int, ...),
            silent = TRUE))
    }

    if (!inherits(out, "data.frame") || nrow(out) < 1) {
        return("`broom::tidy(model)` did not return a valid data.frame.")
    }

    if (!"term" %in% colnames(out)) {
        return("`broom::tidy(model)` did not return a data.frame with a `term` column.")
    }

    return(out)

}


get_estimates_parameters <- function(model,
                                     conf_int,
                                     conf_level,
                                     effects = "all", ...) {

    if (inherits(model, "marginaleffects") ||
        inherits(model, "comparisons") ||
        inherits(model, "marginalmeans")) {
        return("`parameters` does not support marginaleffects yet.")
    }

    f <- tidy_easystats <- function(x, ...) {
        out <- parameters::parameters(x, verbose = FALSE, ...)
        out <- parameters::standardize_names(out, style = "broom")
    }

    if (isTRUE(conf_int)) {
        out <- suppressMessages(suppressWarnings(try(
            f(model, ci = conf_level, effects = effects, ...),
            silent = TRUE)))
    } else {
        out <- suppressMessages(suppressWarnings(try(
            f(model, effects = effects, ...),
            silent = TRUE)))
    }

    if (!inherits(out, "data.frame") || nrow(out) < 1) {
        return("`parameters::parameters(model)` did not return a valid data.frame.")
    }

    if (!"term" %in% colnames(out)) {
        return("`parameters::parameters(model)` did not return a data.frame with a `term` column.")
    }

    return(out)
}
