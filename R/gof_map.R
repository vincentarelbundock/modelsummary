#' Data.frame used to clean up and format goodness-of-fit statistics
#'
#' @docType data
#' @keywords datasets
#' @name gof_map
#' @format data.frame with 4 columns of character data: raw, clean, fmt, omit
#' @importFrom dplyr tribble
#' @export
gof_map <- dplyr::tribble(
	~ raw,           ~ clean,         ~ fmt,     ~ omit,
	"nobs",          "Num.Obs.",      "%.0f",    FALSE,
	"r.squared",     "R2",            "%.3f",    FALSE,
	"adj.r.squared", "Adj.R2",        "%.3f",    FALSE,
	"AIC",           "AIC",           "%.1f",    FALSE,
	"BIC",           "BIC",           "%.1f",    FALSE,
    "logLik",        "Log.Lik.",      "%.3f",    FALSE,
	"deviance",      "Deviance",      "%.2f",    TRUE,
	"df.residual",   "DF Resid",      "%.0f",    TRUE,
	"df.null",       "DF Null",       "%.0f",    TRUE,
	"sigma",         "Sigma",         "%.3f",    TRUE,
	"statistic",     "Statistics",    "%.3f",    TRUE,
	"p.value",       "p",             "%.3f",    TRUE,
	"df",            "DF",            "%.0f",    TRUE,
	"null.deviance", "Deviance Null", "%.2f",    TRUE
	)
