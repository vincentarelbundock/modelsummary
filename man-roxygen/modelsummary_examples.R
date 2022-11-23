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
#' # fmt: function to keep 3 digits including at least 2 after the decimal
#' m <- lm(mpg ~ I(hp * 1000) + drat, data = mtcars)
#' f <- function(x) format(x, digits = 3, nsmall = 2, scientific = FALSE)
#' modelsummary(m, fmt = f, gof_map = NA)
#' 
#' # fmt: same as above but using scientific notation
#' m <- lm(mpg ~ I(hp * 1000) + drat, data = mtcars)
#' f <- function(x) format(x, digits = 3, nsmall = 2)
#' modelsummary(m, fmt = f, gof_map = NA)
#' 
#' # coef_rename
#' modelsummary(models, coef_rename = c('Volume' = 'Large', 'Height' = 'Tall'))
#' modelsummary(models, coef_rename = toupper)
#' modelsummary(models, coef_rename = coef_rename)
#' 
#' # coef_rename = TRUE for variable labels
#' datlab <- mtcars
#' datlab$cyl <- factor(datlab$cyl)
#' attr(datlab$hp, "label") <- "Horsepower"
#' attr(datlab$cyl, "label") <- "Cylinders"
#' modlab <- lm(mpg ~ hp * drat + cyl, data = datlab)
#' modelsummary(modlab, coef_rename = TRUE)
#'
#' # coef_rename: unnamed vector of length equal to the number of terms in the final table
#' m <- lm(hp ~ mpg + factor(cyl), data = mtcars)
#' modelsummary(m, coef_omit = -(3:4), coef_rename = c("Cyl 6", "Cyl 8"))
#'
#' # coef_map
#' modelsummary(models, coef_map = c('Volume' = 'Large', 'Height' = 'Tall'))
#' modelsummary(models, coef_map = c('Volume', 'Height'))
#'
#' # coef_omit: omit the first and second coefficients
#' modelsummary(models, coef_omit = 1:2)
#' 
#' # coef_omit: omit coefficients matching one substring
#' modelsummary(models, coef_omit = "ei", gof_omit = ".*")
#'
#' # coef_omit: omit a specific coefficient
#' modelsummary(models, coef_omit = "^Volume$", gof_omit = ".*")
#' 
#' # coef_omit: omit coefficients matching either one of two substring
#' modelsummary(models, coef_omit = "ei|rc", gof_omit = ".*")
#' 
#' # coef_omit: keep coefficients starting with a substring (using a negative lookahead)
#' modelsummary(models, coef_omit = "^(?!Vol)", gof_omit = ".*")
#' 
#' # coef_omit: keep coefficients matching a substring
#' modelsummary(models, coef_omit = "^(?!.*ei|.*pt)", gof_omit = ".*")
#' 
#' # shape: multinomial model
#' library(nnet)
#' multi <- multinom(factor(cyl) ~ mpg + hp, data = mtcars, trace = FALSE) 
#'
#' # term names and group ids in rows, models in columns
#' modelsummary(multi, shape = response ~ model)
#'
#' # term names and group ids in rows in a single column
#' modelsummary(multi, shape = term : response ~ model)
#' 
#' # term names in rows and group ids in columns
#' modelsummary(multi, shape = term ~ response:model)
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
#' # gof_map: tribble
#' library(tibble)
#' gm <- tribble(
#'   ~raw,        ~clean,      ~fmt,
#'   "r.squared", "R Squared", 5)
#' modelsummary(models, gof_map = gm)
#' 
#' # gof_map: data.frame
#' gm <- modelsummary::gof_map
#' gm$omit[gm$raw == 'deviance'] <- FALSE
#' gm$fmt[gm$raw == 'r.squared'] <- "%.5f"
#' modelsummary(models, gof_map = gm)
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
