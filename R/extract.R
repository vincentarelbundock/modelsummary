#' Extract and combine estimates and goodness-of-fit statistics from several
#' statistical models.
#'
#' @param models named list of models to summarize
#' @param statistic string name of the statistic to include in parentheses
#' below estimates. Must be either "conf.int", or one of the column names
#' produced by the `broom::tidy` function. Typical values include: "std.error",
#' "conf.int", "statistic", "p.value".
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `stringr::str_detect`).
#' @param coef_map named string vector. Names refer to the original variable
#' names. Values refer to the variable names that will appear in the table.
#' Coefficients which are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param gof string regular expression. Omits all matching gof statistics from
#' the table (using `stringr::str_detect`).
#' @param gof_map data.frame in the same format as `gtsummary::gof_map`
#' @param add_rows list of character vectors, each of length equal to the number
#' of models + 1.
#' @param fmt passed to the `sprintf` function to format numeric values into
#' strings, with rounding, etc. See `?sprintf` for options.
#'
#' @export
extract <- function(models,
                    statistic = 'std.error',
                    conf_level = 0.95,
                    coef_map = NULL,
                    coef_omit = NULL,
                    gof_map = NULL,
                    gof_omit = NULL,
                    add_rows = NULL,
                    stars = NULL,
                    fmt = '%.3f') {
    # models must be a list of models or a single model
    if (class(models) != 'list') {
        models <- list(models)
    }
    # model names
    if (is.null(names(models))) {
        model_names <- paste('Model', 1:length(models))
    } else {
        if (any(names(models) == '')) {
            stop('Model names cannot include empty strings. Please make sure that every object in the `models` list has a unique, non-empty name. If the `models` list has no names at all, `gtsummary` will create some automatically.')
        }
        model_names <- names(models)
    }
    # extract and combine estimates
    est <- models %>%
           purrr::map(extract_estimates, fmt = fmt, statistic = statistic, conf_level = conf_level, stars = stars) %>%
           purrr::reduce(dplyr::full_join, by = c('term', 'statistic'))  %>%
           setNames(c('term', 'statistic', model_names)) %>%
           dplyr::mutate(group = 'estimates') %>%
           dplyr::select(group, term, statistic, names(.))
    # extract and combine gof
    gof <- models %>%
           purrr::map(extract_gof, fmt = fmt, gof_map = gof_map)  %>%
           purrr::reduce(dplyr::full_join, by = 'term') %>%
           setNames(c('term', model_names))
    # add_rows to bottom of gof
    if (!is.null(add_rows)) {
        bad <- any(sapply(add_rows, length) != length(models) + 1)
        if (bad) {
            stop('All string vectors in the `add_rows` list must include one more element (i.e. the row name) than the number of models in the table.')
        }
        add_rows <- lapply(add_rows, as.character) %>%  # TODO: remove once sanity checks are complete
                    do.call('rbind', .) %>%
                    data.frame(stringsAsFactors = FALSE) %>%
                    setNames(names(gof))
        gof <- dplyr::bind_rows(gof, add_rows)
    }
    # add gof row identifier
    gof <- gof %>%
           dplyr::mutate(group = 'gof') %>%
           dplyr::select(group, term, names(.))
    # omit using regex
    if (!is.null(coef_omit)) {
        est <- est %>%
               dplyr::filter(!stringr::str_detect(term, coef_omit))
    }
    if (!is.null(gof_omit)) {
        gof <- gof %>%
               dplyr::filter(!stringr::str_detect(term, gof_omit))
    }
    # reorder estimates using coef_map
    if (!is.null(coef_map)) {
        est <- est[est$term %in% names(coef_map),] # subset
        idx <- match(est$term, names(coef_map))
        est$term <- coef_map[idx] # rename
        est <- est[order(idx, est$statistic),] # reorder
    }
    # output
    out <- dplyr::bind_rows(est, gof)
    out[is.na(out)] <- ''
    return(out)
}
