#' sanity check
#'
#' @noRd
sanitize_vcov <- function(vcov, models, ...) {

  ellip <- list(...)
  number_of_models <- length(models)

  if (is.null(vcov)) {
    vcov <- list(vcov)
    names(vcov) <- ""
  }

  # corner case: `vcov = list(NULL)`
  if (isTRUE(checkmate::check_list(vcov, len = 1)) && is.null(vcov[[1]])) {
    lab <- names(vcov) # user-supplied label
    vcov <- rep(list(NULL), number_of_models)
    names(vcov) <- rep(lab, number_of_models)
  }

  # recycling vcov for multiple models
  # single string/formulas/matrices/functions: apply to every model
  if (!isTRUE(checkmate::check_list(vcov))) {
    if (is.null(vcov) ||
        isTRUE(checkmate::check_formula(vcov)) ||
        isTRUE(checkmate::check_matrix(vcov))  ||
        isTRUE(checkmate::check_function(vcov)) ||
        # checkmate oddity for list(NULL) which flags as numeric
        isTRUE(checkmate::check_numeric(vcov)) ||
        isTRUE(checkmate::check_character(vcov, len = 1))) {
      vcov <- rep(list(vcov), number_of_models)
    # character vector to list (can be any length for single model many vcov)
    } else if (isTRUE(checkmate::check_character(vcov))) {
      vcov <- as.list(vcov)
    # check first class because some models inherit from "list"
    } else if (class(vcov)[1] != "list") {
      stop("The value of the `vcov` argument is invalid. Please refer to the documentation.")
    }
  }

  labels <- names(vcov)

  checkmate::check_true(length(vcov) == number_of_models)

  sandwich_types <- c(
    "Andrews",
    "bootstrap",
    "classical",
    "constant",
    "CR",
    "CR0",
    "CR1",
    "CR1p",
    "CR2",
    "CR3",
    "HAC",
    "HC",
    "HC0",
    "HC1",
    "HC2",
    "HC3",
    "HC4",
    "HC4m",
    "HC5",
    "iid",
    "kenward-roger",
    "mammen",
    "NeweyWest",
    "outer-product",
    "panel-corrected",
    "PL",
    "robust",
    "stata",
    "weave",
    "webb",
    "wild",
    "xy")

  for (i in seq_along(vcov)) {

    # lme4::lmer
    if (inherits(vcov[[i]], "dpoMatrix") || inherits(vcov[[i]], "vcovCR")) {
      vcov[[i]] <- as.matrix(vcov[[i]])
    }

    j <- ifelse(length(models) == 1, 1, i)
    # fixest does its own checks

    if (!inherits(models[[j]], "fixest")) {
      checkmate::assert(
          checkmate::check_null(vcov[[i]]),
          checkmate::check_formula(vcov[[i]]),
          checkmate::check_function(vcov[[i]]),
          checkmate::check_matrix(vcov[[i]]),
          checkmate::check_numeric(vcov[[i]]),
          checkmate::check_character(vcov[[i]], min.len = 2),
          checkmate::check_choice(tolower(vcov[[i]]), choices = tolower(sandwich_types)),
          combine = "or")

    # fixest is weird
    } else {
      insight::check_if_installed("fixest")
      if (identical(vcov[[i]], "robust")) {
        vcov[[i]] <- "HC1"
      } else if (identical(vcov[[i]], "HC3")) {
        insight::check_if_installed("sandwich")
        vcov[[i]] <- tryCatch(sandwich::vcovHC(models[[j]]), error = function(e) "HC1")
        names(vcov)[i] <- "HC3"
      } else if (isTRUE(checkmate::check_formula(vcov[[i]]))) {
        lab <- paste("by:", gsub("\\+", "\\&", gsub(":", "\\ & ", as.character(vcov[[i]])[2])))
        vcov[[i]] <- stats::vcov(models[[j]], vcov = vcov[[i]])
        names(vcov)[i] <- lab
      }
    }

    if (isTRUE(checkmate::check_formula(vcov[[i]]))) {
      names(vcov)[i] <- paste("by:", gsub("\\+", "\\&", gsub(":", "\\ & ", as.character(vcov[[i]])[2])))
    }

    if ((is.null(names(vcov)[i]) || is.na(names(vcov)[i])) && (
        isTRUE(checkmate::check_function(vcov[[i]])) ||
        isTRUE(checkmate::check_matrix(vcov[[i]])) ||
        isTRUE(checkmate::check_numeric(vcov[[i]])) ||
        isTRUE(checkmate::check_character(vcov[[i]])))) {
      names(vcov)[i] <- "Custom"
    }

    # case-insensitive sandwich types
    if (isTRUE(checkmate::check_character(vcov[[i]], len = 1))) {

      # modelsummary-specific shortcuts
      vcov[[i]] <- switch(tolower(vcov[[i]]),
        "stata" = "hc1",
        "robust" = "hc3",
        vcov[[i]])

      # case normalization
      vcov[[i]] <- sandwich_types[match(tolower(vcov[[i]]), tolower(sandwich_types))]
      names(vcov)[i] <- vcov[[i]]

      if (isTRUE(tolower(vcov[[i]]) %in% c("classical", "constant", "iid", "default"))) {
        vcov[i] <- list(NULL)
        if (isTRUE(tolower(vcov[[i]]) %in% c("classical", "constant", "default"))) {
          names(vcov)[i] <- tools::toTitleCase("vcov[[i]]")
        } else {
          names(vcov)[i] <- "IID"
        }
        j <- ifelse(length(models) == 1, 1, i)
        if (inherits(models[[j]], c("lm_robust", "iv_robust", "felm"))) {
          msg <- insight::format_message(sprintf('When the `vcov` argument is set to "iid", "classical", or "constant", `modelsummary` extracts the default variance-covariance matrix from the model object. For objects of class `%s`, the default vcov is not always IID. Please make sure that the standard error label matches the numeric results in the table. Note that the `vcov` argument accepts a named list for users who want to customize the standard error labels in their regression tables.', class(models[[j]])[1]))
          warning(msg, call. = FALSE)
        }
      }

      # after case normalization
      if (identical(tolower(vcov[[i]]), "weave")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::weave
        names(vcov)[i] <- "Weave"

      } else if (identical(tolower(vcov[[i]]), "NeweyWest")) {
        names(vcov)[i] <- "Newey-West"

      } else if (identical(tolower(vcov[[i]]), "andrews")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::kernHAC
        names(vcov)[i] <- "Andrews"

      } else if (identical(tolower(vcov[[i]]), "bootstrap")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::vcovBS
        names(vcov)[i] <- "Bootstrap"

      } else if (identical(tolower(vcov[[i]]), "panel-corrected")) {
        assert_dependency("sandwich")
        if (!"cluster" %in% names(ellip)) {
          msg <- 'You must specify a `cluster` argument when using "panel-corrected" standard errors.'
          stop(insight::format_message(msg), call. = FALSE)
        }
        vcov[[i]] <- sandwich::vcovPC
        names(vcov)[i] <- "Panel-corrected"

      } else if (identical(tolower(vcov[[i]]), "outer-product")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::vcovOPG
        names(vcov)[i] <- "Outer product"
      }
    }
  }

  # user-supplied labels
  if (!is.null(labels)) {
    names(vcov) <- labels
  }

  for (i in seq_along(vcov)) {
    if (!is.null(names(vcov)) && is.na(names(vcov)[i])) {
      names(vcov)[i] <- ""
    }
  }

  if (all(names(vcov) == "")) names(vcov) <- NULL

  return(vcov)
}
