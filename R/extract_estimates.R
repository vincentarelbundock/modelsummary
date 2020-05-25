#' Extract estimates and statistics from a single model
#' @importFrom generics tidy
#' @param model object type with an available `tidy` method.
#' @inheritParams modelsummary
#' @return data.frame with side-by-side model summaries
#' @keywords internal
extract_estimates <- function(model,
                              statistic = 'std.error',
                              statistic_override = NULL,
                              statistic_vertical = TRUE,
                              conf_level = .95,
                              fmt = '%.3f',
                              stars = FALSE,
                              ...) {

    # statistic override
    if (!is.null(statistic_override)) {

        # extract overriden statistics
        so <- extract_statistic_override(model,
                                         statistic = statistic,
                                         statistic_override = statistic_override)

        if (!statistic %in% colnames(so)) {
            stop(paste0(statistic, " cannot be extracted through the
                        `statistic_override` argument. You might want to look
                        at the `modelsummary:::extract_statistic_override`
                        function to diagnose the problem."))
        }

        # extract estimates, but keep only columns that do not appear in so
        est <- tidy(model, ...)
        est <- est[, c('term', base::setdiff(colnames(est), colnames(so)))]
        est <- suppressWarnings(dplyr::left_join(est, so, by = 'term'))

    } else { # if statistic_override is not used

        # extract estimates
        if ('conf.int' %in% statistic) {
            est <- tidy(model, conf.int = TRUE, conf.level = conf_level, ...)
        } else {
            est <- tidy(model, ...)
        }
    }

    # round estimates
    est$estimate <- rounding(est$estimate, fmt)

    # extract statistics
    for (i in seq_along(statistic)) {

        s <- statistic[i]

        # extract confidence intervals
        if (s == 'conf.int') {
            if (!all(c('conf.low', 'conf.high') %in% colnames(est))) {
                stop('tidy cannot not extract confidence intervals for a model of this class.')
            }

            # rounding and brackets
            est$conf.high <- rounding(est$conf.high, fmt)
            est$conf.low <- rounding(est$conf.low, fmt)
            est[[paste0('statistic', i)]] <- paste0('[', est$conf.low, ', ', est$conf.high, ']')

        # extract other types of statistics
        } else {

            # rounding non-character values and parentheses
            if (!is.character(est[[s]])) {
                est[[s]] <- rounding(est[[s]], fmt)
                est[[s]] <- ifelse(est[[s]] != '',  # avoid empty parentheses for NAs
                                   paste0('(', est[[s]], ')'),
                                   est[[s]])
                est[[paste0('statistic', i)]] <- est[[s]]
            } else if (is.character(est[[s]])) {
                # renaming character columns for later subsetting
                est[[paste0('statistic', i)]] <- est[[s]]
            }

        }
    }

    # stars
    stars <- clean_stars(stars)

    if (!is.null(stars)) {
        if (!'p.value' %in% colnames(est)) {
            stop('To use the `stars` argument, the `tidy` function must produce a column called "p.value"')
        }
        est$stars <- ''
        for (n in names(stars)) {
            est$stars <- ifelse(est$p.value < stars[n], n, est$stars)
        }
        est$estimate <- paste0(est$estimate, est$stars)
    }

    # subset columns
    cols <- c('term', 'estimate', paste0('statistic', seq_along(statistic)))
    est <- est[, cols]

    # reshape to vertical
    if (statistic_vertical) {
        est <- suppressWarnings(tidyr::pivot_longer(est, -term, names_to='statistic'))
        est <- est %>% 
               dplyr::arrange(term, statistic)
    } else {
        est$statistic <- 'estimate'
        est$value <- paste(est$estimate, est$statistic1)
        est$estimate <- est$statistic1 <- NULL
    }

    # output
    return(est)
}
