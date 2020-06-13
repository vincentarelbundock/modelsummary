#' Data.frame used to clean up and format goodness-of-fit statistics
#'
#' By default, this data frame is passed to the 'gof_map' argument of the
#' 'msummary' or 'modelsummary' functions. Users can modify this data frame to
#' customize the list of statistics to display and their format. See example
#' below.
#' @docType data
#' @keywords datasets
#' @name gof_map
#' @format data.frame with 4 columns of character data: raw, clean, fmt, omit
#' @importFrom dplyr tribble
#' @examples
#' library(modelsummary)
#' mod <- lm(wt ~ drat, data = mtcars)
#' gm <- modelsummary::gof_map
#' gm$omit[gm$raw == 'deviance'] <- FALSE
#' gm$fmt[gm$raw == 'r.squared'] <- "%.5f"
#' msummary(mod, gof_map = gm)
#' @export
gof_map <- dplyr::tribble(
~raw,               ~clean,           ~fmt,   ~omit,
"nobs",             "Num.Obs.",       "%.0f", FALSE,
"nimp",             "Num.Imp.",       "%.0f", FALSE,
"nclusters",        "Num.Clust.",     "%.0f", FALSE,
"nblocks",          "Num.Blocks",     "%.0f", FALSE,
"r.squared",        "R2",             "%.3f", FALSE,
"adj.r.squared",    "R2 Adj.",        "%.3f", FALSE,
"pseudo.r.squared", "R2 Pseudo",      "%.3f", FALSE,
"within.r.squared", "R2 Within",      "%.3f", FALSE,
"r.squared.within", "R2 Within",      "%.3f", FALSE,
"AIC",              "AIC",            "%.1f", FALSE,
"BIC",              "BIC",            "%.1f", FALSE,
"logLik",           "Log.Lik.",       "%.3f", FALSE,
"std.error.type",   "Std.Error Type", "%.3f", FALSE,
"deviance",         "Deviance",       "%.2f", TRUE,
"df.residual",      "DF Resid",       "%.0f", TRUE,
"df.null",          "DF Null",        "%.0f", TRUE,
"sigma",            "Sigma",          "%.3f", TRUE,
"statistic",        "Statistics",     "%.3f", TRUE,
"p.value",          "p",              "%.3f", TRUE,
"df",               "DF",             "%.0f", TRUE,
"null.deviance",    "Deviance Null",  "%.2f", TRUE
)
