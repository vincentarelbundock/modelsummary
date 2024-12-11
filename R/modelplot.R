#' Model Summary Plots with Estimates and Confidence Intervals
#'
#' Dot-Whisker plot of coefficient estimates with confidence intervals. For
#' more information, see the Details and Examples sections below, and the
#' vignettes on the `modelsummary` website:
#' https://modelsummary.com/
#' * [modelplot Vignette.](https://modelsummary.com/articles/modelplot.html)
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
#' @template citation
#' @section Examples:
#' ```{r, eval = identical(Sys.getenv("pkgdown"), "true"), fig.asp = .4}
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
#'   model = "(1)",
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
#'
#' # logistic regression example
#' df <- as.data.frame(Titanic)
#' mod_titanic <- glm(
#'   Survived ~ Class + Sex,
#'   family = binomial,
#'   weight = Freq,
#'   data = df
#' )
#'
#' # displaying odds ratio using a log scale
#' modelplot(mod_titanic, exponentiate = TRUE) +
#'   scale_x_log10() +
#'   xlab("Odds Ratios and 95% confidence intervals")
#' ```
#'
#' @export
modelplot <- function(models,
                     conf_level = getOption("modelsummary_conf_level", default = .95),
                     coef_map = getOption("modelsummary_coef_map", default = NULL),
                     coef_omit = getOption("modelsummary_coef_omit", default = NULL),
                     coef_rename = getOption("modelsummary_coef_rename", default = NULL),
                     vcov = getOption("modelsummary_vcov", default = NULL),
                     exponentiate = getOption("modelsummary_exponentiate", default = FALSE),
                     add_rows = getOption("modelsummary_add_rows", default = NULL),
                     facet = getOption("modelsummary_facet", default = FALSE),
                     draw = getOption("modelsummary_draw", default = TRUE),
                     background = getOption("modelsummary_background", default = NULL),
                     ...) {

  ellip <- list(...)
   
  ## settings
   settings_init(settings = list(
   "function_called" = "modelplot"
   ))
   
   # more informative error message specific to `modelplot`
   sanity_conf_level_modelplot(conf_level)
   
  
  
  # Function to remove invalid arguments
  remove_invalid_args <- function(args, valid_args) {
    # Filter out invalid arguments based on their names
    args[names(args) %in% valid_args]
  }

  if (is.null(conf_level)) {
    estimate <- "estimate"
  } else {
    # oooof, ugly hack, but we want to return all those when draw=FALSE
    estimate <- "{estimate}|{std.error}|{conf.low}|{conf.high}|{p.value}"
  }

  # otherwise `modelsummary` returns a tinytable
  out <- modelsummary(
    output      = "dataframe",
    models      = models,
    fmt         = NULL,
    estimate    = estimate,
    statistic   = NULL,
    conf_level  = conf_level,
    coef_map    = coef_map,
    coef_omit   = coef_omit,
    coef_rename = coef_rename,
    gof_omit    = ".*",
    vcov        = vcov,
    exponentiate = exponentiate,
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
    regex <- "(.*)\\|(.*)\\|(.*)\\|(.*)\\|(.*)"
    out$estimate <- as.numeric(gsub(regex, "\\1", out$value))
    out$std.error <- as.numeric(gsub(regex, "\\2", out$value))
    out$conf.low <- as.numeric(gsub(regex, "\\3", out$value))
    out$conf.high <- as.numeric(gsub(regex, "\\4", out$value))
    out$p.value <- as.numeric(gsub(regex, "\\5", out$value))
  }

  dat <- out[!is.na(out$term) & !is.na(out$model) & !is.na(out$estimate),]
  # clean and sort
  row.names(dat) <- dat$value <- dat$id <- NULL
  dat$term <- factor(dat$term, term_order)
  dat$model <- factor(dat$model, model_order)
  dat <- dat[order(dat$term, dat$model), ]

  if (all(is.na(dat$std.error))) dat$std.error <- NULL
  if (all(is.na(dat$p.value))) dat$p.value <- NULL

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
    insight::check_if_installed("ggplot2")
  }

  p <- ggplot2::ggplot(dat)

  # set a new theme only if the default is theme_grey(). this prevents user's
  # theme_set() from being overwritten
  if (identical(ggplot2::theme_get(), ggplot2::theme_grey())) {
    p <- p +
         ggplot2::theme_minimal() +
         ggplot2::theme(legend.title = ggplot2::element_blank())
  }

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
      args_list <- list(
        mapping = ggplot2::aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)
      )
      args_list <- c(args_list, ellip)
      p <- p + do_call(ggplot2::geom_pointrange, args_list)
    } else {
      if (facet) {
        args_list <- list(
          mapping = ggplot2::aes(y = model, x = estimate, xmin = conf.low, xmax = conf.high)
        )
        args_list <- c(args_list, ellip)
        p <- p + do_call(ggplot2::geom_pointrange, c(args_list, ellip)) +
            ggplot2::facet_grid(term ~ ., scales = 'free_y')
      } else {
        args_list <- list(
          mapping = ggplot2::aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, color = model),
          position = ggplot2::position_dodge(width = .5))
        args_list <- c(args_list, ellip)
        p <- p + do_call(ggplot2::geom_pointrange, args_list)
      }
    }
    tmp <- sprintf('Coefficient estimates and %s%% confidence intervals', conf_level * 100)
    p <- p + ggplot2::labs(x = tmp, y = '')
    # geom_point: without confidence interval
  } else {
    if (length(unique(dat$model)) == 1) {
      args_list <- list(
        mapping = ggplot2::aes(y = term, x = estimate)
      )
      args_list <- c(args_list, ellip)
      p <- p + do_call(ggplot2::geom_point, args_list)
    } else {
      if (facet) {
        args_list <- list(
          mapping = ggplot2::aes(y = term, x = estimate)
        )
        args_list <- c(args_list, ellip)
        p <- p +
          do_call(ggplot2::geom_point, args_list) +
          ggplot2::facet_grid(term ~ ., scales = 'free_y')
      } else {
        args_list <- list(
          mapping = ggplot2::aes(y = term, x = estimate),
          position = ggplot2::position_dodge(width = .5)
        )
        args_list <- c(args_list, ellip)
        p <- p + do_call(ggplot2::geom_point, args_list)
      }
    }
    p <- p + ggplot2::labs(x = 'Coefficient estimates', y = '')
  }

  return(p)
}


#' safe do.call
#'
#' @keywords internal
#' @importFrom methods formalArgs
do_call <- function(fun, args) {
  valid <- formalArgs(fun)
  valid <- c(valid, "color", "colour", "size", "linetype", "fill")
  args <- args[names(args) %in% valid]
  do.call(fun, args)
}
