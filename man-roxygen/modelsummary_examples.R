#'
#' @examples
#' \dontrun{
#'
#' # The `modelsummary` website includes \emph{many} examples and tutorials:
#' # https://vincentarelbundock.github.io/modelsummary
#'
#' library(modelsummary)
#'
#' # load data and estimate models
#' data(trees)
#' models <- list()
#' models[['Bivariate']] <- lm(Girth ~ Height, data = trees)
#' models[['Multivariate']] <- lm(Girth ~ Height + Volume, data = trees)
#'
#' # simple table
#' modelsummary(models)
#'
#' # statistic
#' modelsummary(models, statistic = NULL)
#' modelsummary(models, statistic = 'p.value')
#' modelsummary(models, statistic = 'statistic')
#' modelsummary(models, statistic = 'conf.int', conf_level = 0.99)
#' modelsummary(models, statistic = c("t = {statistic}",
#'                                    "se = {std.error}",
#'                                    "conf.int"))
#'
#' # estimate
#' modelsummary(models,
#'   statistic = NULL,
#'   estimate = "{estimate} [{conf.low}, {conf.high}]")
#' modelsummary(models,
#'   estimate = c("{estimate}{stars}",
#'                "{estimate} ({std.error})"))
#'
#' # vcov
#' modelsummary(models, vcov = "robust")
#' modelsummary(models, vcov = list("classical", "stata"))
#' modelsummary(models, vcov = sandwich::vcovHC)
#' modelsummary(models,
#'   vcov = list(stats::vcov, sandwich::vcovHC))
#' modelsummary(models,
#'   vcov = list(c("(Intercept)"="", "Height"="!"),
#'               c("(Intercept)"="", "Height"="!", "Volume"="!!")))
#'
#' # vcov with custom names
#' modelsummary(
#'   models,
#'   vcov = list("Stata Corp" = "stata",
#'               "Newey Lewis & the News" = "NeweyWest"))
#'
#' # coef_rename
#' modelsummary(models, coef_rename = c('Volume' = 'Large', 'Height' = 'Tall'))
#' modelsummary(models, coef_rename = toupper)
#'
#' # coef_map
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#' modelsummary(models, coef_map = c('Volume', 'Height'))
#'
#' # title
#' modelsummary(models, title = 'This is the title')
#'
#' # title with LaTeX label (for numbering and referencing)
#' modelsummary(models, title = 'This is the title \\label{tab:description}')
#'
#' # add_rows
#' rows <- tibble::tribble(~term, ~Bivariate, ~Multivariate,
#'   'Empty row', '-', '-',
#'   'Another empty row', '?', '?')
#' attr(rows, 'position') <- c(1, 3)
#' modelsummary(models, add_rows = rows)
#'
#' # notes
#' modelsummary(models, notes = list('A first note', 'A second note'))
#'
#' # gof_map: data.frame
#' gm <- modelsummary::gof_map
#' gof_custom$omit[gof_custom$raw == 'deviance'] <- FALSE
#' gof_custom$fmt[gof_custom$raw == 'r.squared'] <- "%.5f"
#' modelsummary(models, gof_map = gof_custom)
#'
#' # gof_map: list of lists
#' f1 <- function(x) format(round(x, 3), big.mark=",")
#' f2 <- function(x) format(round(x, 0), big.mark=",")
#' gm <- list(
#'   list("raw" = "nobs", "clean" = "N", "fmt" = f2),
#'   list("raw" = "AIC", "clean" = "aic", "fmt" = f1))
#' modelsummary(models,
#'   fmt = f1,
#'   gof_map = gm)
#'
#' }
#'
