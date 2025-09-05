#' Get default sandwich types
#' @noRd
get_sandwich_types <- function() {
  c(
    "Andrews", "BS", "classical", "constant", "CR", "CR0", "CR1", "CR1p",
    "CR2", "CR3", "HAC", "HC", "HC0", "HC1", "HC2", "HC3", "HC4",
    "HC4m", "HC5", "iid", "kenward-roger", "mammen", "NeweyWest",
    "outer-product", "panel-corrected", "PL", "bootstrap", "residual",
    "robust", "stata", "weave", "webb", "wild", "xy"
  )
}

#' Classify and validate vcov type
#' @noRd
classify_vcov <- function(vcov_item, sandwich_types = NULL) {
  if (is.null(sandwich_types)) {
    sandwich_types <- get_sandwich_types()
  }
  
  if (is.null(vcov_item)) return("null")
  if (isTRUE(checkmate::check_formula(vcov_item))) return("formula")
  if (isTRUE(checkmate::check_function(vcov_item))) return("function")
  if (isTRUE(checkmate::check_matrix(vcov_item))) return("matrix")
  if (isTRUE(checkmate::check_numeric(vcov_item))) return("numeric")
  if (isTRUE(checkmate::check_character(vcov_item, min.len = 2))) return("character_vector")
  if (isTRUE(checkmate::check_character(vcov_item, len = 1))) {
    if (tolower(vcov_item) %in% tolower(sandwich_types)) return("character_single")
  }
  
  stop("The value of the `vcov` argument is invalid. Please refer to the documentation.", call. = FALSE)
}

#' Recycle vcov for multiple models
#' @noRd
recycle_vcov <- function(vcov, number_of_models) {
  if (is.null(vcov)) {
    vcov <- list(vcov)
    names(vcov) <- ""
  }

  # corner case: `vcov = list(NULL)`
  if (isTRUE(checkmate::check_list(vcov, len = 1)) && is.null(vcov[[1]])) {
    lab <- names(vcov) # user-supplied label
    vcov <- rep(list(NULL), number_of_models)
    names(vcov) <- rep(lab, number_of_models)
    return(vcov)
  }

  # recycling vcov for multiple models
  if (!isTRUE(checkmate::check_list(vcov))) {
    if (isTRUE(checkmate::check_character(vcov)) && length(vcov) > 1) {
      vcov <- as.list(vcov)
    } else if (class(vcov)[1] != "list") {
      vcov <- rep(list(vcov), number_of_models)
    } else {
      stop(
        "The value of the `vcov` argument is invalid. Please refer to the documentation.",
        call. = FALSE
      )
    }
  }

  return(vcov)
}

#' Process fixest-specific vcov handling
#' @noRd
process_fixest_vcov <- function(vcov_item, model, vcov, i) {
  insight::check_if_installed("fixest")

  if (identical(vcov_item, "robust")) {
    vcov[[i]] <- "HC1"
    return(vcov)
  }

  if (identical(vcov_item, "HC3")) {
    insight::check_if_installed("sandwich")
    vcov[[i]] <- tryCatch(
      sandwich::vcovHC(model),
      error = function(e) "HC1"
    )
    names(vcov)[i] <- "HC3"
    return(vcov)
  }

  if (isTRUE(checkmate::check_formula(vcov_item))) {
    names(vcov)[i] <- paste(
      "by:",
      gsub("\\+", "\\&", gsub(":", "\\ & ", as.character(vcov_item)[2]))
    )
    vcov[[i]] <- stats::vcov(model, vcov = vcov_item)
    return(vcov)
  }

  return(vcov)
}

#' Process character vcov types (shortcuts, normalization, special functions)
#' @noRd
process_character_vcov <- function(vcov_item, model, ..., sandwich_types = NULL) {
  if (is.null(sandwich_types)) {
    sandwich_types <- get_sandwich_types()
  }
  if (!isTRUE(checkmate::check_character(vcov_item, len = 1))) {
    return(list(vcov = vcov_item, name = NULL))
  }

  # Apply shortcuts and normalize case
  vcov_lower <- tolower(vcov_item)
  vcov_lower <- switch(vcov_lower,
    "stata" = "hc1",
    "robust" = "hc3",
    vcov_lower
  )
  normalized <- sandwich_types[match(vcov_lower, tolower(sandwich_types))]

  # Handle default types that become NULL
  if (vcov_lower %in% c("classical", "constant", "iid", "default")) {
    name <- switch(vcov_lower,
      "iid" = "IID",
      "classical" = "Classical",
      "constant" = "Constant", 
      "default" = "Default"
    )

    if (inherits(model, c("lm_robust", "iv_robust", "felm"))) {
      warning(sprintf(
        'When the `vcov` argument is set to "iid", "classical", or "constant", `modelsummary` extracts the default variance-covariance matrix from the model object. For objects of class `%s`, the default vcov is not always IID. Please make sure that the standard error label matches the numeric results in the table. Note that the `vcov` argument accepts a named list for users who want to customize the standard error labels in their regression tables.',
        class(model)[1]
      ), call. = FALSE)
    }
    return(list(vcov = NULL, name = name))
  }

  # Handle special functions with lookup
  special_functions <- list(
    "weave" = list(vcov = sandwich::weave, name = "Weave"),
    "neweywest" = list(vcov = normalized, name = "Newey-West"),
    "andrews" = list(vcov = sandwich::kernHAC, name = "Andrews"),
    "bootstrap" = list(vcov = normalized, name = "Bootstrap"),
    "panel-corrected" = list(vcov = sandwich::vcovPC, name = "Panel-corrected"),
    "outer-product" = list(vcov = sandwich::vcovOPG, name = "Outer product")
  )

  if (vcov_lower %in% names(special_functions)) {
    spec <- special_functions[[vcov_lower]]

    # Check sandwich dependency for functions that need it
    sand <- c("weave", "andrews", "bootstrap", "panel-corrected", "outer-product")
    if (vcov_lower %in% sand) {
      assert_dependency("sandwich")
    }

    if (vcov_lower == "panel-corrected" && !"cluster" %in% ...names()) {
      stop('You must specify a `cluster` argument when using "panel-corrected" standard errors.', call. = FALSE)
    }

    return(list(vcov = spec$vcov, name = spec$name))
  }

  return(list(vcov = normalized, name = normalized))
}

#' Sanitize vcov argument
#' @noRd
sanitize_vcov <- function(vcov, models, ...) {
  number_of_models <- length(models)

  sandwich_types <- get_sandwich_types()

  vcov <- recycle_vcov(vcov, number_of_models)
  labels <- names(vcov)

  checkmate::check_true(length(vcov) == number_of_models)

  for (i in seq_along(vcov)) {
    # Convert special matrix types to standard matrices
    if (inherits(vcov[[i]], "dpoMatrix") || inherits(vcov[[i]], "vcovCR")) {
      vcov[[i]] <- as.matrix(vcov[[i]])
    }

    j <- if (length(models) == 1) 1 else i

    # Classify and validate vcov type
    vcov_type <- classify_vcov(vcov[[i]], sandwich_types)
    
    # Handle fixest vs non-fixest models  
    if (inherits(models[[j]], "fixest")) {
      vcov <- process_fixest_vcov(vcov[[i]], models[[j]], vcov, i)
    }

    # Generate formula labels (skip for fixest as it's already handled)
    if (vcov_type == "formula" && !inherits(models[[j]], "fixest")) {
      names(vcov)[i] <- paste(
        "by:",
        gsub("\\+", "\\&", gsub(":", "\\ & ", as.character(vcov[[i]])[2]))
      )
    }

    # Set default labels for custom types
    if (
      (is.null(names(vcov)[i]) || is.na(names(vcov)[i])) &&
        vcov_type %in% c("function", "matrix", "numeric", "character_vector", "character_single")
    ) {
      names(vcov)[i] <- "Custom"
    }

    # Process character vcov types
    char_result <- process_character_vcov(vcov[[i]], models[[j]], ..., sandwich_types)
    if (!is.null(char_result$vcov)) {
      vcov[[i]] <- char_result$vcov
    } else {
      vcov[i] <- list(NULL)
    }
    if (!is.null(char_result$name)) {
      names(vcov)[i] <- char_result$name
    }
  }

  # Restore user-supplied labels
  if (!is.null(labels)) {
    names(vcov) <- labels
  }

  # Clean up names
  for (i in seq_along(vcov)) {
    if (!is.null(names(vcov)) && is.na(names(vcov)[i])) {
      names(vcov)[i] <- ""
    }
  }

  if (all(names(vcov) == "")) names(vcov) <- NULL

  return(vcov)
}
