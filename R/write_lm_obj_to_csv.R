#R/write_lm_obj_to_csv.R
# ------------------------

#' Write an lm object to CSV
#'
#' Exports (1) fitted-values summary, (2) coefficient table, and (3) model-fit
#' statistics of an \code{\link[stats]{lm}} object into a single comma-separated
#' file.
#'
#' @param lmObj  An object of class \code{"lm"} (usually returned by
#'               \code{\link[stats]{lm}}).
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
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' write_lm_obj_to_csv(fit, "mtcars_lm")
#' }

write_lm_obj_to_csv<-function(lmObj, fileNameWithoutExtension)
{
	fitted_df  <- data.frame(
  			Min    = as.numeric(summary(lmObj$fitted.values)[1]),
 	 		Q1     = as.numeric(summary(lmObj$fitted.values)[2]),
 	 		Median = as.numeric(summary(lmObj$fitted.values)[3]),
  			Mean   = as.numeric(summary(lmObj$fitted.values)[4]),
 	 		Q3     = as.numeric(summary(lmObj$fitted.values)[5]),
  			Max    = as.numeric(summary(lmObj$fitted.values)[6])
			)
	utils::write.csv(fitted_df, row.names=FALSE, paste0(fileNameWithoutExtension,".csv"))
	write(" ",file = paste0(fileNameWithoutExtension,".csv"), append = TRUE)
	coef_df <- summary(lmObj)$coefficients
	utils::write.table(data.frame(t(c("",colnames(coef_df)))),append= TRUE, sep=",", col.names=FALSE, row.names=FALSE, file=paste0(fileNameWithoutExtension,".csv"))   
	utils::write.table(coef_df, file= paste0(fileNameWithoutExtension,".csv"), append = TRUE, row.names = TRUE, col.names = FALSE, sep = ",")
	write(" ",file = paste0(fileNameWithoutExtension,".csv"), append = TRUE)


	model_df <- data.frame(
 	 		R2        = as.numeric(summary(lmObj)$r.squared),
  			AdjR2     = as.numeric(summary(lmObj)$adj.r.squared),
  			Sigma     = as.numeric(summary(lmObj)$sigma),
  			F              = as.numeric(summary(lmObj)$fstatistic[1]),
  			p_value   = stats::pf(summary(lmObj)$fstatistic[1],
                		 		summary(lmObj)$fstatistic[2],
                 				summary(lmObj)$fstatistic[3],
                 				lower.tail = FALSE),
  			df_model  = as.numeric(summary(lmObj)$df[1]),
  			df_resid  = as.numeric(summary(lmObj)$df[2])
			)

	utils::write.table(model_df, file=paste0(fileNameWithoutExtension,".csv"), append = TRUE, row.names = FALSE, col.names = TRUE, sep = ",")
}
