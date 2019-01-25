#' Data.frame used to clean up and format goodness-of-fit statistics
#'
#' @docType data
#' @keywords datasets
#' @name gof_map
#' @format data.frame with 3 columns of character data: raw, clean, fmt
#' @importFrom dplyr tribble
#' @export
gof_map <- dplyr::tribble(
	~ raw,           ~ clean,         ~ fmt,
	"df.residual",   "DF Resid",      "%.0f",
	"df.null",       "DF Null",       "%.0f",
	"r.squared",     "R2",            "%.3f",
	"adj.r.squared", "Adj.R2",        "%.3f",
	"sigma",         "Sigma",         "%.3f",
	"statistic",     "Statistics",    "%.3f",
	"p.value",       "p",             "%.3f",
	"df",            "DF",            "%.0f",
	"logLik",        "Log.Lik.",      "%.3f",
	"AIC",           "AIC",           "%.1f",
	"BIC",           "BIC",           "%.1f",
	"deviance",      "Deviance",      "%.2f",
	"null.deviance", "Deviance Null", "%.2f",
	"n",             "N",             "%.0f"
	)
