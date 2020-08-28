#' Extract and combine estimates and goodness-of-fit statistics from several
#' statistical models.
#'
#' @inheritParams modelsummary
#' @return tibble
#' @keywords internal
extract_models <- function(models,
                           statistic = 'std.error',
                           statistic_override = NULL,
                           statistic_vertical = TRUE,
                           conf_level = 0.95,
                           coef_map = NULL,
                           coef_omit = NULL,
                           gof_map = NULL,
                           gof_omit = NULL,
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
    sanity_stars(stars)
    sanity_fmt(fmt)
    sanity_estimate(estimate)

    # model names
    if (is.null(names(models))) {
        model_names <- paste('Model', 1:length(models))
    } else {
        model_names <- names(models)
    }
    model_names <- pad(model_names)

    # if statistics_override is a single function, repeat it in a list to allow
    # map
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
            # make sure no duplicate estimate names *within* a single model.
            # this cannot be in input sanity checks. idx paste allows multiple
            # statistics
            idx <- paste(est[[i]]$term, est[[i]]$statistic)
            if (anyDuplicated(idx) > 2) {
                stop('Two coefficients from a single model cannot share the same name. Check model ', i)
            }
        }

        # coef_omit
        if (!is.null(coef_omit)) {
            est[[i]] <- est[[i]] %>%
                        dplyr::filter(!grepl(coef_omit, term))
        }

        # set model name
        colnames(est[[i]])[3] <- model_names[i]
    }

    f <- function(x, y) dplyr::full_join(x, y, by = c('term', 'statistic'))
    est <- Reduce(f, est) %>% 
           dplyr::mutate(group = 'estimates') %>%
           dplyr::select(group, term, statistic, names(.))

    # reorder estimates (must be done after join
    if (!is.null(coef_map)) {
        idx <- match(est[['term']], coef_map)
        est <- est[order(idx, est[['statistic']]),]
    }

    # extract and combine gof
    f <- function(x, y) dplyr::full_join(x, y, by = 'term')
    gof <- models %>%
           lapply(extract_gof, fmt = fmt, gof_map = gof_map)  %>%
           Reduce(f, .) %>%
           stats::setNames(c('term', model_names))

    # add gof row identifier
    gof <- gof %>%
           dplyr::mutate(group = 'gof') %>%
           dplyr::select(group, term, names(.))

    # omit gof using regex
    if (!is.null(gof_omit)) {
        gof <- gof %>%
               dplyr::filter(!grepl(gof_omit, term))
    }

    # otherwise defined at the model level in extract_gof
    if (is.null(gof_map)) {
        gof_map <- modelsummary::gof_map
    }

    # gof_map: omit, reorder, rename
    gof <- gof[!gof$term %in% gof_map$raw[gof_map$omit],] # omit (black list)
	gof_names <- gof_map$clean[match(gof$term, gof_map$raw)] # rename
	gof_names[is.na(gof_names)] <- gof$term[is.na(gof_names)]
    gof$term <- gof_names
	idx <- match(gof$term, gof_map$clean) # reorder
    gof <- gof[order(idx, gof$term),] 

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
