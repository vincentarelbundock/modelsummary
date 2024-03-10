#' Extract model estimates in a tidy format. 
#'
#' A unified approach to extract results from a wide variety of models. For
#' some models `get_estimates` attaches useful attributes to the output. You
#' can access this information by calling the `attributes` function:
#' `attributes(get_estimates(model))`
#'
#' @inheritParams modelsummary
#' @param model a single model object
#' 
#' @export
get_estimates <- function(model, conf_level = .95, vcov = NULL, shape = NULL, coef_rename = FALSE, ...) {

    if (is.null(conf_level)) {
        conf_int <- FALSE
    } else {
        conf_int <- TRUE
    }

    if (inherits(model, "modelsummary_list") && "tidy" %in% names(model)) {
        return(model[["tidy"]])
    }

    # this is usually done in `modelsummary`, but some users may call
    # `get_estimates(mod, vcov = "stata")`
    if (isTRUE(checkmate::check_string(vcov)) || isTRUE(checkmate::check_formula(vcov))) {
        vcov <- sanitize_vcov(list(vcov), list(model), ...)[[1]]
    }

    args <- append(list(model, "vcov" = vcov), list(...))

    vcov <- do.call("get_vcov", args)

    # priority: {parameters} messes up {marginaleffects}, whereas VAB controls `tidy()` exactly
    if (inherits(model, c("comparisons", "marginaleffects", "predictions", "marginalmeans"))) {
        funs <- list(get_estimates_broom)
    } else {
        get_priority <- getOption("modelsummary_get", default = "easystats")
        checkmate::assert_choice(
            get_priority,
            choices = c("broom", "easystats", "parameters", "performance", "all"))
        if (get_priority %in% c("easystats", "parameters", "performance")) {
            funs <- list(get_estimates_parameters, get_estimates_broom)
        } else {
            funs <- list(get_estimates_broom, get_estimates_parameters)
        }
    }

    warning_msg <- NULL
    out <- NULL

    for (f in funs) {
        if (!inherits(out, "data.frame") || nrow(out) == 0) {
            if (is.matrix(vcov)) {
                V <- vcov
            } else {
                V <- NULL
            }
            out <- f(
                model,
                conf_int = conf_int,
                conf_level = conf_level,
                vcov = V,
                coef_rename = coef_rename,
                ...)
            if (is.character(out)) {
                warning_msg <- c(warning_msg, out)
            }
        }
    }

    if (!inherits(out, "data.frame")) {
      msg <- c(
        sprintf('`modelsummary could not extract the required information from a model of class "%s". The package tried a sequence of 2 helper functions to extract estimates:', class(model)[1]),
        '',
        'parameters::parameters(model)',
        'broom::tidy(model)',
        '',
        'To draw a table, one of these commands must return a `data.frame` with a column named "term". The `modelsummary` website explains how to summarize unsupported models or add support for new models yourself: https://modelsummary.com/articles/modelsummary.html',
        '',
        'These errors messages were generated during extraction:',
        '', '')
      msg <- insight::format_message(msg)
      msg <- paste0(msg, paste(warning_msg, collapse = "\n"))
      stop(msg, call. = FALSE)
    }

    override <- function(old, new, columns) {
        columns <- setdiff(columns, c("term", shape$group_name))
        if (!inherits(new, "data.frame") || nrow(new) == 0 || !"term" %in% colnames(new)) {
            return(old)
        }
        if (is.null(shape$group_name)) {
            def <- old[["term"]]
            cus <- new[["term"]]
        } else {
            def <- do.call("paste", as.list(old[, c("term", shape$group_name)]))
            cus <- do.call("paste", as.list(new[, c("term", shape$group_name)]))
        }
        idx <- match(def, cus)
        if (all(is.na(idx))) {
            warning(insight::format_message("Term name mismatch. Make sure all `tidy_custom` method returns a data frame with proper and matching term names."),
                    call. = FALSE)
            return(old)
        }
        for (n in columns) {
            old[[n]] <- ifelse(is.na(idx), old[[n]], new[[n]][idx])
        }
        return(old)
    }

    # override standard errors if `vcov` is a named vector
    out <- override(old = out, new = vcov, columns = "std.error")

    # tidy_custom_internal (modelsummary customization avoids name conflict)
    out_custom <- tidy_custom_internal(model)
    out <- override(old = out, new = out_custom, columns = colnames(out_custom))

    # tidy_custom
    out_custom <- tidy_custom(model)
    out <- override(old = out, new = out_custom, columns = colnames(out_custom))

    # combine columns if requested in `shape` argument using an : interaction
    for (x in shape$combine) {
        vars <- strsplit(x, ":")[[1]]
        out[[vars[1]]] <- paste(out[[vars[1]]], out[[vars[2]]])
    }

    # term must be a character (not rounded with decimals when integer)
    out$term <- as.character(out$term)


    # standard columns may be missing, but a blank space is better than an
    # error, especially for mix of brms::brm() and lm(), for example
    for (col in c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")) {
        if (!col %in% colnames(out)) {
            out[[col]] <- NA_real_
        }
    }

    if (inherits(out, "data.frame")) {
        return(out)
    }
}


get_estimates_broom <- function(model, conf_int, conf_level, ...) {
    insight::check_if_installed("broom")

    if (isTRUE(conf_int) && !is.null(conf_level)) {
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
                                     vcov,
                                     coef_rename,
                                     ...) {

    dots <- list(...)
    args <- c(list("model" = model), dots)
    args[["verbose"]] <- FALSE

    mi <- tryCatch(
        suppressMessages(suppressWarnings(insight::model_info(model))),
        error = function(e) NULL,
        warning = function(e) NULL)

    # extract everything by default
    if (!"effects" %in% names(dots)) args[["effects"]] <- "all"

    # confidence intervals
    if (isTRUE(conf_int)) {
        args[["ci"]] <- conf_level
    } else if (!"ci" %in% names(dots)) { # faster
        args <- c(args, list(ci = NULL))
        args[["ci_random"]] <- FALSE # do not append to avoid duplicate arg
    }

    # bayes: diagnostics can be very expensive
    if (isTRUE(mi[["is_bayesian"]])) {
        if (!"test" %in% names(dots)) args <- c(args, list("test" = NULL))
        if (!"diagnostic" %in% names(dots)) args <- c(args, list("diagnostic" = NULL))
        if (!"dispersion" %in% names(dots)) args <- c(args, list("dispersion" = TRUE))
    }

    # main call
    tidy_easystats <- function(...) {
        dots <- list(...)
        # ci_method="profile" in parameters() does not respect vcov argument
        if ("vcov" %in% names(dots) && !"ci_method" %in% names(dots)) {
            dots[["ci_method"]] <- "wald"
        }
        # bug in `parameters`
        if (isTRUE(dots$coef_rename)) {
            dots[["pretty_names"]] <- "labels"
        }
        inner <- parameters::parameters
        out <- do.call("inner", dots)
        out <- insight::standardize_names(out, style = "broom")

        # S-value (Greenland 2019)
        if ("p.value" %in% colnames(out)) {
            out$s.value <- sprintf("%.1f", -log2(out$p.value))
        }
        return(out)
    }


    if (is.character(vcov) || is.matrix(vcov)) {
      args[["vcov"]] <- vcov
    }
    out <- hush(tryCatch(do.call("tidy_easystats", args), error = function(e) NULL))

    if (isTRUE(coef_rename)) {
        labs <- attr(out, "pretty_labels")
        labs <- gsub("\\*", "\u00d7", labs)
        out$term <- replace_dict(out$term, labs)
        out$term <- gsub("\\*", "\u00d7", out$term)
    }

    # errors and warnings: before processing the data frame term names
    if (!inherits(out, "data.frame") || nrow(out) < 1) {
        return("`parameters::parameters(model)` did not return a valid data.frame.")
    }

    # term names: lavaan
    # before check if there is a `term` name column
    if (inherits(model, "lavaan") && all(c("to", "operator", "from") %in% colnames(out))) {
        out$term <- paste(out$to, out$operator, out$from)
        out$to <- out$operator <- out$from <- NULL
    }

    if (!"term" %in% colnames(out)) {
        return("`parameters::parameters(model)` did not return a data.frame with a `term` column.")
    }

    # term names: mixed-effects
    if (isTRUE(mi[["is_mixed"]]) && isTRUE("group" %in% colnames(out))) {
        idx <- out$term != "SD (Observations)" &
               out$group != "" &
               !grepl(":", out$term) &
               grepl("\\)$", out$term)
        out$term <- ifelse(
            idx,
            sprintf("%s: %s)", gsub("\\)$", "", out$term), out$group),
            out$term)
        # otherwise gets converted to x
        out$term <- ifelse(idx, gsub(":", "", out$term), out$term)
    }

    # "group" column is required to merge lm() and lme4::lmer(), and other grouped and non-grouped models.
    if (!"group" %in% colnames(out)) {
        out[["group"]] <- ""
    }

    return(out)
}


