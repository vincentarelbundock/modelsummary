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
#'   'drat' = 'Rear axle ratio')
#' modelplot(mod, coef_map = cm)
#'
#' # several models
#' models <- list()
#' models[['Small model']] <- lm(hp ~ vs, mtcars)
#' models[['Medium model']] <- lm(hp ~ vs + factor(cyl), mtcars)
#' models[['Large model']] <- lm(hp ~ vs + drat + factor(cyl), mtcars)
#' modelplot(models)
#'
#' # customize your plots with 'ggplot2' functions
#' library(ggplot2)
#'
#' modelplot(models) +
#'   scale_color_brewer(type = 'qual') +
#'   theme_classic()
#'
#' # pass arguments to 'geom_pointrange' through the ... ellipsis
#' modelplot(mod, color = 'red', size = 1, fatten = .5)
#'
#' # add geoms to the background, behind geom_pointrange
#' b <- list(geom_vline(xintercept = 0, color = 'orange'),
#'   annotate("rect", alpha = .1,
#'     xmin = -.5, xmax = .5,
#'     ymin = -Inf, ymax = Inf),
#'   geom_point(aes(y = term, x = estimate), alpha = .3,
#'     size = 10, color = 'red', shape = 'square'))
#' modelplot(mod, background = b)
#' }
#'
#' @export
modelplot <- function(models,
                      conf_level = .95,
                      coef_map = NULL,
                      coef_omit = NULL,
                      coef_rename = NULL,
                      statistic_override = NULL,
                      facet = FALSE,
                      draw = TRUE,
                      background = NULL,
                      ...) {



  if (is.null(conf_level)) {
    out <- modelsummary(
      output="dataframe",
      models=models,
      fmt="%.50f",
      conf_level=conf_level,
      coef_map=coef_map,
      coef_omit=coef_omit,
      coef_rename=coef_rename,
      statistic_override=statistic_override,
      ...
    )
    out <- out[out$part == "estimates" & out$statistic == "estimate",]

    # save for sorting later
    term_order <- unique(out$term)

    # this used to be a `pivot_long` but I can't grok reshape and don't want
    # the tidyr dependency
    out_split <- lapply(4:ncol(out), function(i)
                 data.frame(out[, 1:3], 
                            model=colnames(out)[i], 
                            estimate=out[[i]]))
    out <- bind_rows(out_split)
    out <- out[out$estimate != "",]

    # sort
    out$term <- factor(out$term, term_order)
    out <- out[order(out$term, out$model),]

    # character -> numeric (modelsummary produces characters)
    out$estimate <- as.numeric(out$estimate)

  } else {
    out <- modelsummary(
      output="dataframe",
      models=models,
      fmt="%.50f",
      conf_level=conf_level,
      coef_map=coef_map,
      coef_omit=coef_omit,
      coef_rename=coef_rename,
      statistic="conf.int",
      statistic_override=statistic_override,
      ...
    )

    # save for sorting later
    term_order <- unique(out$term)

    # this used to be a `pivot_long` and a `pivot_wide` but I can't grok
    # reshape and don't want the tidyr dependency
    out <- out[out$part == "estimates",]
    out_split <- lapply(4:ncol(out), function(i) 
                        data.frame(out[, 1:3], model=colnames(out)[i], estimate=out[[i]]))
    out <- bind_rows(out_split)
    out <- reshape(
      out, 
      timevar = "statistic",
      idvar = c("part", "term", "model"),
      direction = "wide")
    colnames(out) <- gsub("^estimate\\.", "", colnames(out))

    # sort
    out <- out[out$estimate != "",]
    out$term <- factor(out$term, term_order)
    out <- out[order(out$term, out$model),]

    # clean statistic
    regex <- "\\[(.*), (.*)\\]"
    out$conf.low <- as.numeric(gsub(regex, "\\1", out$statistic1))
    out$conf.high <- as.numeric(gsub(regex, "\\2", out$statistic1))
    out$statistic1 <- NULL
    out$estimate <- as.numeric(out$estimate)

  }

  dat <- out
  dat$term <- factor(dat$term, rev(unique(dat$term)))
  dat$model <- factor(dat$model, unique(dat$model))

  if (!draw) {
    return(dat)
  } else {
    assert_dependency("ggplot2", "Please install %s or set `modelplot(draw=FALSE)`")
  }

  p <- ggplot2::ggplot(dat) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

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
      p <- p + 
        ggplot2::geom_pointrange(ggplot2::aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high), ...)
    } else {
      if (facet) {
        p <- p + 
          ggplot2::geom_pointrange(ggplot2::aes(y=model, x=estimate, xmin=conf.low, xmax=conf.high), ...) + 
          ggplot2::facet_grid(term ~ ., scales='free_y')
      } else {
        p <- p + 
          ggplot2::geom_pointrange(
            ggplot2::aes(y=term, x=estimate, xmin=conf.low, xmax=conf.high, color=model),
            position=ggplot2::position_dodge(width=.5), ...)
      }
    }
    tmp <- sprintf('Coefficient estimates and %s%% confidence intervals', conf_level * 100)
    p <- p + ggplot2::labs(x=tmp, y='')
    # geom_point: without confidence interval
  } else {
    if (length(unique(dat$model)) == 1) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y=term, x=estimate), ...)
    } else {
      if (facet) {
        p <- p + 
          ggplot2::geom_point(ggplot2::aes(y=model, x=estimate), ...) +
          ggplot2::facet_grid(term ~ ., scales='free_y')
      } else {
        p <- p + ggplot2::geom_point(ggplot2::aes(y=term, x=estimate), 
                                     position=ggplot2::position_dodge(width=.5), ...)
      }
    }
    p <- p + ggplot2::labs(x='Coefficient estimates', y='')
  }
  p
}
