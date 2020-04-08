#' Allow users to override uncertainty estimates
#' @importFrom broom tidy
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @keywords internal
#' @return a numeric vector of test statistics
extract_statistic_override <- function(model, statistic_override, statistic = 'std.error') {
    out <- NULL

    # lmtest is installed
    lmtest_installed <- try(base::find.package('lmtest'), silent = TRUE)
    lmtest_installed <- !'try-error' %in% class(lmtest_installed)

    if (lmtest_installed & (is.matrix(statistic_override) | is.function(statistic_override))) {
       out <- try(statistic_override_lmtest(model, statistic_override), silent = TRUE)
    } 

    # lmtest is not installed or errored
    if (!lmtest_installed | ('try-error' %in% class(out)) | is.null(out)) {

        # function
        if (is.function(statistic_override)) {
            out <- try(statistic_override_function(model, statistic_override), silent = TRUE)
            if ('try-error' %in% class(out)) {
                stop('The function supplied to `statistic_override` cannot recover a variance-covariance matrix or standard error vector')
            }

        # matrix
        } else if (is.matrix(statistic_override)) {
            out <- try(statistic_override_matrix(model, statistic_override), silent = TRUE)
            if ('try-error' %in% class(out)) {
                stop('The matrix supplied to `statistic_override` cannot be used to recover standard errors.')
            }

        # vector
        } else if (is.vector(statistic_override)) {
            out <- try(statistic_override_vector(model, statistic_override, statistic = statistic), silent = TRUE)
        }
    }

    return(out)
}

#' Use the lmtest::coeftest function to extract uncertainty estimates
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibble
#' @keywords internal
statistic_override_lmtest <- function(model, statistic_override) {
    out <- lmtest::coeftest(model, statistic_override) %>%
           generics::tidy()
    return(out)
}

#' Use the statistic_override function to extract std.error
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibble
#' @keywords internal
statistic_override_function <- function(model, statistic_override) {
    out <- statistic_override(model) %>%
           base::diag() %>%
           tibble::tibble('term' = names(.),
                          'std.error' = .)
    return(out)
}

#' Use the statistic_override matrix to extract std.error
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibble
#' @keywords internal
statistic_override_matrix <- function(model, statistic_override) {
    if (is.null(names(statistic_override))) {
        stop('The colnames and row.names of the `statistic_override` matrix must correspond to term/coefficient names.')
    }
    out <- statistic_override %>%
           base::diag() %>%
           tibble::tibble('term' = names(.),
                          'std.error' = .)
    return(out)
}

#' Use the statistic_override vector to extract std.error/p.value/statistic
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return tibble
#' @keywords internal
statistic_override_vector <- function(model, statistic_override, statistic) {
    if (is.null(names(statistic_override))) {
        stop('The names of the `statistic_override` vector must correspond to term/coefficient names.')
    }
    out <- statistic_override %>%
           tibble::tibble(term = names(.), statistic = .) %>%
           stats::setNames(c('term', statistic))
    return(out)
}
