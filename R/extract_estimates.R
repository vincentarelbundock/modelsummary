#' Extract estimates and statistics from a single model
extract_estimates <- function(model, 
                             statistic = 'std.error', 
                             conf_level = .95,
                             fmt = '%.3f', 
                             stars = NULL, 
                             ...) {
    # extract estimates with confidence intervals
    if (statistic == 'conf.int') {
        est <- broom::tidy(model, conf.int = TRUE, conf.level = conf_level) 
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
        est <- broom::tidy(model)
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
    if (!is.null(stars)) {
        if (!'p.value' %in% colnames(est)) {
            stop('To use the `stars` argument, the `broom::tidy` function must produce a column called "p.value"')
        }
        if (is.logical(stars)) {
            if (stars) {
                stars <- c('*' = .1, '**' = .05, '***' = .01)
            } 
        }
        stars <- sort(stars, decreasing = TRUE)
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
