#' Extract estimates and statistics from a single model
#' @importFrom broom tidy
#' @param model object type with an available `tidy` method.
#' @return data.frame with side-by-side model summaries
#' @inheritParams modelsummary 
extract_estimates <- function(model,
                              statistic = 'std.error',
                              statistic_override = NULL,
                              conf_level = .95,
                              fmt = '%.3f',
                              stars = FALSE) {

    # extract estimates
    if (statistic == 'conf.int') {
        est <- generics::tidy(model, conf.int = TRUE, conf.level = conf_level)
    } else {
        est <- generics::tidy(model)
    }

    # statistic override
    if (!is.null(statistic_override)) {
        so <- extract_statistic_override(model, 
                                         statistic = statistic,
                                         statistic_override = statistic_override)
        if (!statistic %in% colnames(so)) {
            stop(paste0(statistic, " cannot be extracted through the `statistic_override` argument. You might want to look at the `modelsummary:::extract_statistic_override` function to diagnose the problem."))
        }
        idx <- base::intersect(colnames(est), colnames(so))
        idx <- idx[idx != 'term']
        est <- est[, !colnames(est) %in% idx] 
        est <- dplyr::left_join(est, so, by = 'term')
    }

    # extract estimates with confidence intervals
    if (statistic == 'conf.int') {
        if (!all(c('conf.low', 'conf.high') %in% colnames(est))) {
            stop('broom::tidy cannot not extract confidence intervals for a model of this class.')
        }
        est <- est %>%
              # rounding
              dplyr::mutate(estimate = rounding(estimate, fmt),
                            conf.low = rounding(conf.low, fmt),
                            conf.high = rounding(conf.high, fmt),
                            statistic = paste0('[', conf.low, ', ', conf.high, ']'))

    # extract estimates with another statistic
    } else {
        if (!statistic %in% colnames(est)) {
            stop('argument statistic must be `conf.int` or match a column name in the output of broom::tidy(model). Typical values are: `std.error`, `p.value`, `statistic`.')
        }
        est$statistic <- est[[statistic]]
              # rounding
        est <- est %>%
               dplyr::mutate(estimate = rounding(estimate, fmt),
                             statistic = rounding(statistic, fmt),
                             statistic = paste0('(', statistic, ')'))
    }

    # stars
    if (is.numeric(stars)) {
        if (!'p.value' %in% colnames(est)) {
            stop('To use the `stars` argument, the `broom::tidy` function must produce a column called "p.value"')
        }
        est$stars <- ''
        for (n in names(stars)) {
            est$stars <- ifelse(est$p.value < stars[n], n, est$stars)
        }
        est$estimate <- paste0(est$estimate, est$stars)
    }

    # prune columns
    est <- est %>% dplyr::select(term, estimate, statistic)

    # reshape
    est <- est %>%
           tidyr::gather(statistic, value, -term) %>%
           dplyr::arrange(term, statistic)


    # output
    return(est)
}
