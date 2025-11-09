# R/write_rqss_obj_to_csv.R
# ----------------------------

#' Write an rqss object to CSV
#'
#' Exports (1) fitted-values summary, (2) coefficient table, and (3)
#' quantile-specific goodness-of-fit information (fidelity, effective df,
#' sample size) of an \code{\link[quantreg]{rqss}} object into a single
#' comma-separated file.
#'
#' @param rqssObj  An object of class \code{"rqss"} (usually returned by
#'                 \code{\link[quantreg]{rqss}}).
#' @param fileNameWithoutExtension  Output file name **without extension**.
#'                     The string \code{".csv"} is automatically appended.
#'
#' @return
#' Called for its side effect (writing a file). Nothing is returned.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(quantreg)
#' fit <- rqss(y ~ qss(x, lambda = 1), tau = 0.5, data = mydata)
#' write_rqss_obj_to_csv(fit, "my_rqss")
#' }
write_rqss_obj_to_csv <- function(rqssObj, fileNameWithoutExtension) 
{
  	fileName <- paste0(fileNameWithoutExtension, ".csv")
  	 fitted_vals <- stats::fitted(rqssObj)
 	 fitted_df  <- data.frame(
    		Min    = min(fitted_vals),
    		Q1     = as.numeric(stats::quantile(fitted_vals, 0.25)),
    		Median = as.numeric(stats::quantile(fitted_vals, 0.5)),
    		Mean   = mean(fitted_vals),
    		Q3     = as.numeric(stats::quantile(fitted_vals, 0.75)),
    		Max    = max(fitted_vals)
  		)
  	utils::write.csv(fitted_df, file = fileName, row.names = FALSE)
  	write(" ", file = fileName, append = TRUE)
  	coef_mat <- summary(rqssObj)$coef
  	utils::write.table(data.frame(t(c("", colnames(coef_mat)))), file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = FALSE)
  	utils::write.table(coef_mat, file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = TRUE)
  	write(" ", file = fileName, append = TRUE)
  	sumObj   <- summary(rqssObj)
  	tau      <- as.numeric(sumObj$tau)[1]   
  	fidelity <- as.numeric(sumObj$fidelity)[1]   
  	df_eff   <- as.numeric(sumObj$edf)[1]
  	n        <- as.numeric(sumObj$n)[1]
  	write(paste0("Quantile Fidelity at tau = ", tau, "  is  ", round(fidelity, 2)), file = fileName, append = TRUE)
  	write(paste0("Effective Degrees of Freedom = ", df_eff), file = fileName, append = TRUE)
  	write(paste0("Sample Size = ", n), file = fileName, append = TRUE)
  }
