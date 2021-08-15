#' Model Summary Plots with Estimates and Confidence Intervals
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
#' # add_rows: add an empty reference category
#'
#' mod <- lm(hp ~ factor(cyl), mtcars)
#'
#' add_rows = data.frame(
#'   term = "factory(cyl)4",
#'   model = "Model 1",
#'   estimate = NA)
#' attr(add_rows, "position") = 3
#' modelplot(mod, add_rows = add_rows)
#'
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
                      conf_level  = .95,
                      coef_map    = NULL,
                      coef_omit   = NULL,
                      coef_rename = NULL,
                      vcov        = NULL,
                      add_rows    = NULL,
                      facet       = FALSE,
                      draw        = TRUE,
                      background  = NULL,
                      ...) {

  ## settings
  settings_init(settings = list(
    "function_called" = "modelplot"
  ))

  # more informative error message specific to `modelplot`
  sanity_conf_level_modelplot(conf_level)

  ellip <- list(...)

  if ("statistic_override" %in% names(ellip)) {
    if (!is.null(vcov)) {
      stop("The `vcov` and `statistic_override` arguments cannot be used at the same time. The `statistic_override` argument is deprecated. Please use `vcov` instead.")
    }
  }

  if (is.null(conf_level)) {
    estimate <- "estimate"
  } else {
    estimate <- "{estimate}|{std.error}|{conf.low}|{conf.high}"
  }


  out <- modelsummary(
    output      = "dataframe",
    models      = models,
    fmt         = "%.50f",
    estimate    = estimate,
    statistic   = NULL,
    conf_level  = conf_level,
    coef_map    = coef_map,
    coef_omit   = coef_omit,
    coef_rename = coef_rename,
    gof_omit    = ".*",
    vcov        = vcov,
    ...
  )
  out$part <- out$statistic <- NULL

  # save for sorting later
  term_order <- unique(out$term)
  model_order <- colnames(out)[2:ncol(out)]

  out <- stats::reshape(
    out,
    varying = colnames(out)[2:ncol(out)],
    times = colnames(out)[2:ncol(out)],
    v.names = "value",
    timevar = "model",
    direction = "long")

  if (is.null(conf_level)) {
    out$estimate <- as.numeric(out$value)
  } else {
    regex <- "(.*)\\|(.*)\\|(.*)\\|(.*)"
    out$estimate <- as.numeric(gsub(regex, "\\1", out$value))
    out$std.error <- as.numeric(gsub(regex, "\\2", out$value))
    out$conf.low <- as.numeric(gsub(regex, "\\3", out$value))
    out$conf.high <- as.numeric(gsub(regex, "\\4", out$value))
  }

  # clean and sort
  dat <- stats::na.omit(out)
  row.names(dat) <- dat$value <- dat$id <- NULL
  dat$term <- factor(dat$term, term_order)
  dat$model <- factor(dat$model, model_order)
  dat <- dat[order(dat$term, dat$model), ]

  # add_rows
  if (!is.null(add_rows)) {
    pos <- attr(add_rows, 'position')
    for (i in 1:nrow(add_rows)) {
      if (!is.null(pos) && !is.na(pos[i]) && pos[i] <= nrow(dat)) {
        top <- dat[-c(pos[i]:nrow(dat)), , drop = FALSE]
        bot <- dat[c(pos[i]:nrow(dat)), , drop = FALSE]
        dat <- bind_rows(top, add_rows[i, , drop = FALSE], bot)
      } else {
        dat <- bind_rows(dat, add_rows[i, , drop = FALSE])
      }
    }
    dat$term <- as.character(dat$term)
    dat$model <- as.character(dat$model)
    dat$term <- factor(dat$term, unique(dat$term))
    dat$model <- factor(dat$model, unique(dat$model))
  }

  # draw
  if (!draw) {
    return(dat)
  } else {
    assert_dependency("ggplot2")
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
      p <- p + ggplot2::geom_pointrange(
        ggplot2::aes(y = term, x = estimate,
          xmin = conf.low, xmax = conf.high), ...)
    } else {
      if (facet) {
        p <- p +
            ggplot2::geom_pointrange(ggplot2::aes(y = model, x = estimate,
                                                  xmin = conf.low, xmax = conf.high), ...) +
            ggplot2::facet_grid(term ~ ., scales = 'free_y')
      } else {
        p <- p +
          ggplot2::geom_pointrange(
            ggplot2::aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, color = model),
            position = ggplot2::position_dodge(width = .5), ...)
      }
    }
    tmp <- sprintf('Coefficient estimates and %s%% confidence intervals', conf_level * 100)
    p <- p + ggplot2::labs(x = tmp, y = '')
    # geom_point: without confidence interval
  } else {
    if (length(unique(dat$model)) == 1) {
      p <- p + ggplot2::geom_point(ggplot2::aes(y = term, x = estimate), ...)
    } else {
      if (facet) {
        p <- p +
          ggplot2::geom_point(ggplot2::aes(y = model, x = estimate), ...) +
          ggplot2::facet_grid(term ~ ., scales = 'free_y')
      } else {
        p <- p + ggplot2::geom_point(ggplot2::aes(y = term, x = estimate),
                                     position = ggplot2::position_dodge(width = .5), ...)
      }
    }
    p <- p + ggplot2::labs(x = 'Coefficient estimates', y = '')
  }

  return(p)
}
