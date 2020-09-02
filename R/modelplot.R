#' Plot model coefficients using points or point-ranges
#'
#' @inheritParams modelsummary
#' @param draw TRUE returns a 'ggplot2' object, FALSE returns the data.frame
#' used to draw the plot.
#' @param facet TRUE or FALSE. When the 'models' argument includes several
#' model objects, TRUE draws terms in separate facets, and FALSE draws terms
#' side-by-side (dodged).
#' @param background A list of 'ggplot2' geoms to add to the background of the
#' plot.  This is especially useful to display annotations "behind" the
#' 'geom_pointrange' that 'modelplot' draws.
#' @importFrom ggplot2 ggplot theme_minimal theme element_blank geom_pointrange geom_point aes facet_grid position_dodge labs
#' @examples
#' \dontrun{
#' 
#' library(modelsummary)
#' 
#' # single model
#' mod <- lm(hp ~ vs + drat, mtcars)
#' modelplot(mod)
#' 
#' # omit terms with string matches or regexes
#' modelplot(mod, coef_omit = 'Interc')
#' 
#' # rename, reorder and subset with 'coef_map'
#' cm <- c('vs' = 'V-shape engine',
#'         'drat' = 'Rear axle ratio')
#' modelplot(mod, coef_map = cm)
#' 
#' # several models
#' models <- list() 
#' models[['Small model']] <- lm(hp ~ vs, mtcars)
#' models[['Medium model']] <- lm(hp ~ vs + factor(cyl) , mtcars)
#' models[['Large model']] <- lm(hp ~ vs + drat + factor(cyl), mtcars)
#' modelplot(models)
#' 
#' # customize your plots with 'ggplot2' functions
#' library(ggplot2)
#' 
#' modelplot(models) +
#'     scale_color_brewer(type = 'qual') +
#'     theme_classic()
#' 
#' # pass arguments to 'geom_pointrange' through the ... ellipsis
#' modelplot(mod, color = 'red', size = 1, fatten = .5)
#' 
#' # add geoms to the background, behind geom_pointrange
#' b <- list(geom_vline(xintercept = 0, color = 'orange'),
#'           annotate("rect", alpha = .1,
#'                    xmin = -.5, xmax = .5,
#'                    ymin = -Inf, ymax = Inf),
#'           geom_point(aes(y = term, x = estimate), alpha = .3,
#'                      size = 10, color = 'red', shape = 'square'))
#' modelplot(mod, background = b)
#'
#' }
#' 
#' @export
modelplot <- function(models, 
                      conf_level = .95, 
                      coef_map = NULL, 
                      coef_omit = NULL, 
                      facet = FALSE,
                      draw = TRUE,
                      background = NULL,
                      ...) {

    modelplot_extract <- function(models, 
                                  conf_level,
                                  coef_map,
                                  coef_omit) {
        clean <- function(x) {
            as.numeric(gsub('\\[|\\]|,', '', x))
        }
        if (!is.null(conf_level)) {
            out <- extract_models(models, 
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
                   stats::na.omit() %>%
                   tidyr::separate(statistic1, into = c('conf.low', 'conf.high'), sep = ', ') %>%
                   dplyr::mutate(dplyr::across(c(conf.low, conf.high), clean))
        } else {
            out <- extract_models(models, 
                                  coef_map = coef_map,
                                  coef_omit = coef_omit,
                                  fmt = '%.50f') %>% 
                   dplyr::filter(group == 'estimates', statistic == 'estimate') %>%
                   dplyr::select(-group, -statistic) %>%
                   tidyr::pivot_longer(-term, names_to = 'model', values_to = 'estimate') %>%
                   dplyr::mutate(estimate = clean(estimate)) %>%
                   stats::na.omit()
        }
        out <- out %>%
               dplyr::mutate(term = factor(term, rev(unique(term))),
                             model = factor(model, unique(model)))
        return(out)
    }

    dat <- modelplot_extract(models, 
                             coef_map = coef_map,
                             coef_omit = coef_omit,
                             conf_level = conf_level)

    if (!draw) return(dat)

    p <- ggplot(dat) +
         theme_minimal() +
         theme(legend.title = element_blank())

    # background geoms
    if (is.list(background)) {
        for (g in background) {
            if (inherits(g, 'ggproto')) {
                p <- p + g
            }
        }
    }

    # geom_pointrange: with confidence interval
    if (!is.null(conf_level)) {
        if (length(unique(dat$model)) == 1) {
            p <- p + geom_pointrange(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high), ...) 
        } else {
            if (facet) {
                p <- p + geom_pointrange(aes(y = model, x = estimate, xmin = conf.low, xmax = conf.high), ...) +
                         facet_grid(term ~ ., scales = 'free_y')
            } else {
                p <- p + geom_pointrange(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, color = model),
                                         position = position_dodge(width=.5), ...)
            }
        }
        tmp <- sprintf('Coefficient estimates and %s%% confidence intervals', conf_level * 100)
        p <- p + labs(x = tmp, y = '')
    # geom_point: without confidence interval
    } else {
         if (length(unique(dat$model)) == 1) {
            p <- p + geom_point(aes(y = term, x = estimate), ...) 
        } else {
            if (facet) {
                p <- p + geom_point(aes(y = model, x = estimate), ...) +
                         facet_grid(term ~ ., scales = 'free_y')
            } else {
                p <- p + geom_point(aes(y = term, x = estimate), position = position_dodge(width=.5), ...)
            }
        }
        p <- p + labs(x = 'Coefficient estimates', y = '')
    }
    p
}
