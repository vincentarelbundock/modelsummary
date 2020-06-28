#' Plot model coefficients using points or point-ranges
#'
#' @inheritParams modelsummary
#' @param draw TRUE returns a 'ggplot2' object, FALSE returns the data.frame
#' used to draw the plot.
#' @param TRUE or FALSE. When the 'models' argument includes several model
#' objects, TRUE draws terms in separate facets, and FALSE draws terms
#' side-by-side (dodged).
#' @export
modelplot <- function(models, 
                      conf_level = .95, 
                      coef_map = NULL, 
                      coef_omit = NULL, 
                      facet = FALSE,
                      draw = TRUE,
                      ...) {

    modelplot_extract <- function(models, 
                                  conf_level,
                                  coef_map,
                                  coef_omit) {
        clean <- function(x) {
            x %>% stringr::str_remove_all('\\[|\\]|,') %>%
                  as.numeric
        }
        if (!is.null(conf_level)) {
            out <- modelsummary:::extract(models, 
                                          statistic = 'conf.int',
                                          conf_level = conf_level, 
                                          coef_map = coef_map,
                                          coef_omit = coef_omit,
                                          fmt = '%.50f') %>% 
                   dplyr::filter(group == 'estimates') %>%
                   dplyr::select(-group) %>%
                   tidyr::pivot_longer(3:ncol(.),  names_to = 'model') %>%
                   tidyr::pivot_wider(names_from = 'statistic') %>%
                   dplyr::mutate(estimate = clean(estimate)) %>%
                   tidyr::drop_na() %>%
                   tidyr::separate(statistic1, into = c('conf.low', 'conf.high'), sep = ', ') %>%
                   dplyr::mutate(dplyr::across(c(conf.low, conf.high), clean))
        } else {
            out <- modelsummary:::extract(models, 
                                          coef_map = coef_map,
                                          coef_omit = coef_omit,
                                          fmt = '%.50f') %>% 
                   dplyr::filter(group == 'estimates', statistic == 'estimate') %>%
                   dplyr::select(-group, -statistic) %>%
                   tidyr::pivot_longer(-term, names_to = 'model', values_to = 'estimate') %>%
                   dplyr::mutate(estimate = clean(estimate)) %>%
                   tidyr::drop_na()
        }
        out <- out %>%
               dplyr::mutate(term = factor(term, rev(unique(term))),
                             model = factor(model, rev(unique(model))))
        return(out)
    }

    dat <- modelplot_extract(models, 
                             coef_map = coef_map,
                             coef_omit = coef_omit,
                             conf_level = conf_level)

    if (!draw) return(dat)

    p <- ggplot2::ggplot(dat) +
         ggplot2::theme_minimal() +
         ggplot2::theme(legend.title = ggplot2::element_blank())

    # geom_pointrange: with confidence interval
    if (!is.null(conf_level)) {
        if (length(unique(dat$model)) == 1) {
            p <- p + ggplot2::geom_pointrange(ggplot2::aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high), ...) 
        } else {
            if (facet) {
                p <- p + ggplot2::geom_pointrange(ggplot2::aes(y = model, x = estimate, xmin = conf.low, xmax = conf.high), ...) +
                         ggplot2::facet_grid(term ~ ., scales = 'free_y')
            } else {
                p <- p + ggplot2::geom_pointrange(ggplot2::aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, color = model),
                                         position = ggplot2::position_dodge(width=.5), ...)
            }
        }
        tmp <- sprintf('Coefficient estimates and %s%% confidence intervals', conf_level * 100)
        p <- p + ggplot2::labs(x = tmp, y = '')
    # ggplot2::geom_point: without confidence interval
    } else {
         if (length(unique(dat$model)) == 1) {
            p <- p + ggplot2::geom_point(ggplot2::aes(y = term, x = estimate), ...) 
        } else {
            if (facet) {
                p <- p + ggplot2::geom_point(ggplot2::aes(y = model, x = estimate), ...) +
                         facet_grid(term ~ ., scales = 'free_y')
            } else {
                p <- p + ggplot2::geom_point(ggplot2::aes(y = term, x = estimate), position = ggplot2::position_dodge(width=.5), ...)
            }
        }
        p <- p + ggplot2::labs(x = 'Coefficient estimates', y = '')
    }
    p
}
