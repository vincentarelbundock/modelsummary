#' Data.frame used to clean up and format goodness-of-fit statistics
#'
#' @docType data
#' @keywords datasets
#' @name gof_map
#' @format data.frame with 3 columns of character data: raw, clean, fmt

#' Internal function to check the sanity of various user inputs
#'
sanity_check <- function(gof_map = NULL) {
    if (!is.null(gof_map)) {
        if (!'data.frame' %in% class(gof_map)) {
            stop('`gof_map` must be a data.frame or a tibble.')
        }
        if (!all(c('raw', 'clean', 'fmt') %in% colnames(gof_map))) {
            stop('`gof_map` must have 3 columns named "raw", "clean", and "fmt".')
        }
    }
}

#' Convert numeric values to strings using the `sprintf` function. NA, NaN,
#' -Inf, and Inf are replaced by an empty string.
#' @param x a numeric vector to be converted to string
#' @param fmt a character vector of format strings which will be fed to the
#' `sprintf` function. See ?sprintf for details.
#'
rounding <- function(x, fmt = '%.3f') {
    out <- sprintf(fmt, x)
    out <- stringr::str_replace(out, 'NA|NaN|-Inf|Inf', '')
    return(out)
}

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

#' Extract goodness-of-fit statistics from a single model
#'
extract_gof <- function(model, fmt = '%.3f', gof_map = NULL, ...) {
    if (is.null(gof_map)) {
        gof_map <- gtsummary::gof_map
    }
    # sanity checks
    sanity_check(gof_map = gof_map)
    # extract gof from model object
    gof <- broom::glance(model) 
    # round numeric values and rename 
    for (column in colnames(gof)) {
        # is gof in gof_map?
        idx <- match(column, gof_map$raw)
        if (!is.na(idx)) { # yes
            if (class(gof[[column]]) %in% c('numeric', 'integer')) {
                gof[[column]] <- rounding(gof[[column]], gof_map$fmt[idx]) 
            } else {
                gof[[column]] <- as.character(gof[[column]])
            }
            colnames(gof)[colnames(gof) == column] <- gof_map$clean[idx] 
        } else { # no
            if (class(gof[[column]]) %in% c('numeric', 'integer')) {
                gof[[column]] <- rounding(gof[[column]], fmt)
            } else {
                gof[[column]] <- as.character(gof[[column]])
            }
        }
    }
    # reshape
    gof <- gof %>% 
           tidyr::gather(term, value)
    # output
    return(gof)
}

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
           setNames(c('term', model_names)) %>%
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
    # reorder estimates using map
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

#' Beautiful, customizable summaries of statistical models
#'
#' @param models named list of models to summarize
#' @param statistic string name of the statistic to include in parentheses
#' below estimates. Must be either "conf.int", or one of the column names
#' produced by the `broom::tidy` function. Typical values include: "std.error",
#' "conf.int", "statistic", "p.value".
#' @param conf_level confidence level to use for confidence intervals
#' @param coef_omit string regular expression. Omits all matching coefficients
#' from the table (using `stringr::str_detect`).
#' @param coef_map named character vector. Names refer to the original variable
#' names. Values refer to the variable names that will appear in the table.
#' Coefficients which are omitted from this vector will be omitted from the
#' table. The table will be ordered in the same order as this vector.
#' @param gof string regular expression. Omits all matching gof statistics from
#' the table (using `stringr::str_detect`).
#' @param gof_map data.frame in the same format as `gtsummary::gof_map`
#' @param fmt passed to the `sprintf` function to format numeric values into
#' strings, with rounding, etc. See `?sprintf` for options.
#' @param notes a character vector of notes to append to the bottom of the table.
#'
#' @export
gtsummary <- function(models, 
                      statistic = 'std.error',
                      conf_level = 0.95,
                      coef_map = NULL,
                      coef_omit = NULL,
                      gof_map = NULL,
                      gof_omit = NULL,
                      fmt = '%.3f', 
                      stars = NULL,
                      title = NULL,
                      subtitle = NULL,
                      notes = NULL,
                      filename = NULL) {
    dat <- extract(models, 
                   statistic = statistic,
                   conf_level = conf_level,
                   coef_map = coef_map,
                   coef_omit = coef_omit,
                   gof_map = gof_map,
                   gof_omit = gof_omit,
                   stars = stars,
                   fmt = fmt)
    tab <- dat %>%
           # remove duplicate term labels
           dplyr::mutate(term = ifelse(statistic == 'statistic', '', term)) 
    idx <- (1:nrow(tab))[tab$group == 'estimates']
    tab <- tab %>%
           # remove columns not fit for printing
           dplyr::select(-statistic, -group) %>%
           ## group statistics (alternate mechanism. probably better, but I
           ## can't find a way to suppress group labels)
           #dplyr::group_by(group) %>%
           # gt object
           gt::gt(rowname_col = 'term') %>%
           # group statistics
           gt::tab_row_group(group = '', rows = idx)
    # titles
    if (!is.null(title)) {
        if (!is.null(subtitle)) {
            tab <- tab %>% gt::tab_header(title = title, subtitle = subtitle)
        } else {
            tab <- tab %>% gt::tab_header(title = title)
        }
    }
    # stars
    if (!is.null(stars)) {
        if (is.logical(stars)) {
            if (stars) {
                stars <- c('*' = .1, '**' = .05, '***' = .01)
            }
        }
        stars <- sort(stars, decreasing = TRUE)
        stars_note <- paste0(names(stars), ' p < ', stars)
        stars_note <- paste(stars_note, collapse = ', ')
        tab = tab %>% gt::tab_source_note(source_note = stars_note)
    }
    # notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% gt::tab_source_note(source_note = n)
        }
    }
    # write to file 
    if (!is.null(filename)) {
        # guess output format based on filename extension
        ext <- tools::file_ext(filename)
        if (ext == 'html') {
            tab %>% gt::as_raw_html() %>%
                    cat(file = filename)
        } else if (ext == 'rtf') {
            tab %>% gt::as_rtf() %>%
                    cat(file = filename)
        } else if (ext == 'tex') {
            tab %>% gt::as_latex() %>%
                    cat(file = filename)
        } else {
            stop('filename must have one of the following extensions: .html, .rtf, .tex')
        }
    } else {
        return(tab)
    }
}
