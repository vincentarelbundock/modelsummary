#' Extract and combine estimates and goodness-of-fit statistics from several
#' statistical models.
#'
#' @inheritParams modelsummary
#' @return tibble
#' @examples
#' library(modelsummary)
#' data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#' extract(models)
#'
#' @export
extract <- function(models,
                    statistic = 'std.error',
                    statistic_override = NULL,
                    statistic_vertical = TRUE,
                    conf_level = 0.95,
                    coef_map = NULL,
                    coef_omit = NULL,
                    gof_map = modelsummary::gof_map,
                    gof_omit = NULL,
                    add_rows = NULL,
                    add_rows_location = NULL,
                    stars = FALSE,
                    fmt = '%.3f',
                    estimate = 'estimate',
                    ...) {

    # models must be a list of models
    if (!'list' %in% class(models)) {
        models <- list(models)
    }

    # sanity check functions are hosted in R/sanity_checks.R
    sanity_statistic(statistic, statistic_override, statistic_vertical, models)
    sanity_conf_level(conf_level)
    sanity_coef_map(coef_map)
    sanity_coef_omit(coef_omit)
    sanity_gof_map(gof_map)
    sanity_gof_omit(gof_omit)
    sanity_add_rows(add_rows, add_rows_location, models)
    sanity_stars(stars)
    sanity_fmt(fmt)
    sanity_estimate(estimate)

    # model names
    if (is.null(names(models))) {
        model_names <- paste('Model', 1:length(models))
    } else {
        if (any(names(models) == '')) {
            stop('Model names cannot include empty strings. Please make sure that every object in the `models` list has a unique, non-empty name. If the `models` list has no names at all, `modelsummary` will create some automatically.')
        }
        model_names <- names(models)
    }

    # if statistics_override is a single function, repeat it in a list to allow map
    if (is.function(statistic_override)) {
        statistic_override <- rep(list(statistic_override), length(models))
    }

    # extract and combine estimates
    est <- list()
    for (i in seq_along(models)) {
        est[[i]] <- extract_estimates(model = models[[i]],
                                      fmt = fmt,
                                      estimate = estimate,
                                      statistic = statistic,
                                      statistic_override = statistic_override[[i]],
                                      statistic_vertical = statistic_vertical,
                                      conf_level = conf_level,
                                      stars = stars,
                                      ...)

        # coef_map: omit, rename (must be done before join to collapse rows)
        if (!is.null(coef_map)) {
            est[[i]] <- est[[i]][est[[i]]$term %in% names(coef_map),] # omit (white list)
            idx <- match(est[[i]]$term, names(coef_map))
            est[[i]]$term <- coef_map[idx] # rename

            # defensive programming
            if (any(table(est[[i]]$term) > 2)) {
                msg <- paste('You are trying to assign the same name to two different variables in model', i)
                stop(msg)
            }
        }
        # coef_omit
        if (!is.null(coef_omit)) {
            est[[i]] <- est[[i]] %>%
                        dplyr::filter(!stringr::str_detect(term, coef_omit))
        }

        # set model name
        colnames(est[[i]])[3] <- model_names[i]
    }

    est <- est %>% 
           purrr::reduce(dplyr::full_join, by = c('term', 'statistic'))  %>%
           dplyr::mutate(group = 'estimates') %>%
           dplyr::select(group, term, statistic, names(.))

    # reorder estimates (must be done after join
    if (!is.null(coef_map)) {
        idx <- match(est[['term']], coef_map)
        est <- est[order(idx, est[['statistic']]),]
    }

    # extract and combine gof
    gof <- models %>%
           purrr::map(extract_gof, fmt = fmt, gof_map = gof_map)  %>%
           purrr::reduce(dplyr::full_join, by = 'term') %>%
           stats::setNames(c('term', model_names))

    # add gof row identifier
    gof <- gof %>%
           dplyr::mutate(group = 'gof') %>%
           dplyr::select(group, term, names(.))

    # omit gof using regex
    if (!is.null(gof_omit)) {
        gof <- gof %>%
               dplyr::filter(!stringr::str_detect(term, gof_omit))
    }

    # gof_map: omit, reorder, rename
    gof <- gof[!gof$term %in% gof_map$raw[gof_map$omit],] # omit (black list)
	gof_names <- gof_map$clean[match(gof$term, gof_map$raw)] # rename
	gof_names[is.na(gof_names)] <- gof$term[is.na(gof_names)]
    gof$term <- gof_names
	idx <- match(gof$term, gof_map$clean) # reorder
    gof <- gof[order(idx, gof$term),] 

    # add_rows: this needs to be done after sorting and combining to preserve
    # user-selected row order
    if (!is.null(add_rows)) {
        add_rows <- lapply(add_rows, as.character) %>%  # TODO: remove once sanity checks are complete
                    do.call('rbind', .) %>%
                    data.frame(stringsAsFactors = FALSE) %>%
                    stats::setNames(c('term', model_names)) %>%
                    dplyr::mutate(group = 'gof', statistic = '')
        add_rows <- add_rows[, colnames(gof)]

        # sanity check add_rows_location
        checkmate::assert_numeric(add_rows_location, null.ok = TRUE,
                                  max.len = 1, lower = 0, upper = nrow(gof))
        if (is.null(add_rows_location)) { # bottom if location is not specified
            gof <- dplyr::bind_rows(gof, add_rows)
        } else {
            if (add_rows_location == 0) { # top if location is 0
                gof <- dplyr::bind_rows(add_rows, gof)
            } else if (add_rows_location == nrow(gof)) { # bottom if location is nrow(gof)
                gof <- dplyr::bind_rows(gof, add_rows)
            } else { # middle otherwise
                top <- gof[1:add_rows_location,]
                bot <- gof[(add_rows_location + 1):nrow(gof),]
                gof <- dplyr::bind_rows(top, add_rows, bot)
            }
        }
    }

    # combine estimates and gof
    if (nrow(gof) > 0) {
        tab <- dplyr::bind_rows(est, gof)
    } else {
        tab <- est
    }

    # empty cells
    tab[is.na(tab)] <- ''

    # output
    return(tab)
}
