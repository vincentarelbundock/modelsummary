#' sanity check
#'
#' @noRd
sanitize_vcov <- function(vcov, number_of_models, ...) {

  ellip <- list(...)

  # sanity_ellipsis must be called before sanity_vcov
  if ("statistic_override" %in% names(ellip)) {
    vcov <- ellip[["statistic_override"]]
  }

  if (is.null(vcov)) {
    vcov <- list(vcov)
  }

  # corner case: `vcov = list(NULL)`
  if (isTRUE(checkmate::check_list(vcov, len = 1)) && is.null(vcov[[1]])) {
    vcov <- rep(list(NULL), number_of_models)
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
            browser()
      vcov <- as.list(vcov)
    # check first class because some models inherit from "list"
    } else if (class(vcov)[1] != "list") {
      stop("The value of the `vcov` argument is invalid. Please refer to the documentation.")
    }
  }

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
    checkmate::assert(
      checkmate::check_null(vcov[[i]]),
      checkmate::check_formula(vcov[[i]]),
      checkmate::check_function(vcov[[i]]),
      checkmate::check_matrix(vcov[[i]]),
      checkmate::check_numeric(vcov[[i]]),
      checkmate::check_character(vcov[[i]], min.len = 2),
      checkmate::check_choice(tolower(vcov[[i]]), choices = tolower(sandwich_types)),
      combine = "or"
    )

    # case-insensitive sandwich types
    if (isTRUE(checkmate::check_character(vcov[[i]], len = 1))) {

      # modelsummary-specific shortcuts
      vcov[[i]] <- switch(tolower(vcov[[i]]),
        "stata" = "hc1",
        "robust" = "hc3",
        vcov[[i]])

      # case normalization
      vcov[[i]] <- sandwich_types[match(tolower(vcov[[i]]), tolower(sandwich_types))]

      if (tolower(vcov[[i]]) %in% c("classical", "constant", "iid", "default")) {
        lab <- tools::toTitleCase(vcov[[i]])
        vcov[i] <- list(NULL)
      }

      # after case normalization
      if (identical(tolower(vcov[[i]]), "weave")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::weave

      } else if (identical(tolower(vcov[[i]]), "andrews")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::kernHAC

      } else if (identical(tolower(vcov[[i]]), "bootstrap")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::vcovBS

      } else if (identical(tolower(vcov[[i]]), "panel-corrected")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::vcovPC

      } else if (identical(tolower(vcov[[i]]), "outer-product")) {
        assert_dependency("sandwich")
        vcov[[i]] <- sandwich::vcovOPG
      }
    }
  }

  return(vcov)
}
