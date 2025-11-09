# R/write_plm_objects_to_csv.R
# ------------------------------

#' Export plm model objects to CSV
#'
#' Three helpers that save \code{\link[plm]{plm}} summaries
#' (pooling, within-intercept, and random-effects models)
#' into a single comma-separated file.
#'
#' Each function writes: (1) fitted summary, (2) coefficients,
#' and (3) model-fit statistics. For random models variance
#' components are also included.
#'
#' @name plm_csv_helpers
#' @aliases write_fem_within_intercept_obj_to_csv
#'          write_rem_obj_to_csv
#'          write_pem_obj_to_csv
#'
#' @examples
#' \dontrun{
#' library(plm)
#' data("Produc", package = "plm")
#'
#' # within-intercept model
#' mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'               data = Produc, index = c("state","year"), model = "within")
#' wi_mod<-within_intercept(mod, return.model=TRUE) 
#' write_fem_within_intercept_obj_to_csv(wi_mod, "within_model")
#'
#' # random-effects model
#' re_mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'               data = Produc, index = c("state","year"), model = "random")
#' write_rem_obj_to_csv(re_mod, "random_model")
#'
#' # pooling model
#' pool_mod <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
#'                 data = Produc, index = c("state","year"), model = "pooling")
#' write_pem_obj_to_csv(pool_mod, "pooling_model")
#' }

NULL

#  ----------  1  FEM WITHIN INTERCEPT  ----------
#' @rdname plm_csv_helpers
#' @export

write_fem_within_intercept_obj_to_csv <- function(femWIObj, fileNameWithoutExtension) 
{
  
  	fileName <- paste0(fileNameWithoutExtension, ".csv")
	fitted_vals <- stats::fitted(femWIObj)
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
  	coef_mat <- summary(femWIObj)$coefficients
  	utils::write.table(data.frame(t(c("", colnames(coef_mat)))),  file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = FALSE)
  	utils::write.table(coef_mat, file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = TRUE)
  	write(" ", file = fileName, append = TRUE)
	sumObj   <- summary(femWIObj)
  	model_df <- data.frame(
    			     R2        = as.numeric(sumObj$r.squared[1]),
    			    AdjR2     = as.numeric(sumObj$r.squared[2]),
    			    p_value   = as.numeric(sumObj$fstatistic[4]),
    			    df_model  = as.numeric(sumObj$df[1]),
    			    df_resid  = as.numeric(sumObj$df[2])
  			)
  	utils::write.table(model_df, file   = fileName, append = TRUE, sep    = ",", col.names = TRUE, row.names = FALSE)
}

#  ----------  2  RANDOM EFFECTS MODEL  ----------
#' @rdname plm_csv_helpers
#' @export

write_rem_obj_to_csv <- function(remObj, fileNameWithoutExtension) 
{
	 fileName <- paste0(fileNameWithoutExtension, ".csv")
	 fitted_vals <- stats::fitted(remObj)
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
	coef_mat <- summary(remObj)$coefficients
  	utils::write.table(data.frame(t(c("", colnames(coef_mat)))), file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = FALSE)
  	utils::write.table(coef_mat, file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = TRUE)
	 write(" ", file = fileName, append = TRUE)
  	vc <- summary(remObj)$ercomp          
	sigmaVec <- vc$sigma2                       
	var_df <- data.frame(
  			Component = names(sigmaVec),
 			Var       = as.numeric(sigmaVec),
  			StdDev    = sqrt(as.numeric(sigmaVec))
			)	
  	utils::write.table(var_df,  file   = fileName, append = TRUE, sep    = ",", row.names = FALSE, col.names = TRUE)
  	theta <- as.numeric(vc[2])
  	write(paste0("theta: ", round(theta, 4)), file = fileName, append = TRUE)
  	write(" ", file = fileName, append = TRUE)
	sumObj   <- summary(remObj)
  	model_df <- data.frame(
    			R2       = as.numeric(sumObj$r.squared[1]),
    			AdjR2    = as.numeric(sumObj$r.squared[2]),
    			Chisq    = as.numeric(sumObj$fstatistic[2]),
    			p_value   = as.numeric(sumObj$fstatistic[4]),
    			df_model = as.numeric(sumObj$df[1]),
    			df_resid = as.numeric(sumObj$df[2])
  			)
  	utils::write.table(model_df,  file   = fileName, append = TRUE, sep    = ",", col.names = TRUE, row.names = FALSE)
}

#  ----------  3  POOLING MODEL  ----------
#' @rdname plm_csv_helpers
#' @export

write_pem_obj_to_csv <- function(pemObj, fileNameWithoutExtension) 
{
  
  	fileName <- paste0(fileNameWithoutExtension, ".csv")
  	fitted_vals <- stats::fitted(pemObj)
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
 	 coef_mat <- summary(pemObj)$coefficients
 	 utils::write.table(data.frame(t(c("", colnames(coef_mat)))), file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = FALSE)
 	 utils::write.table(coef_mat, file   = fileName, append = TRUE, sep    = ",", col.names = FALSE, row.names = TRUE)
 	 write(" ", file = fileName, append = TRUE)
  	sumObj   <- summary(pemObj)
  	model_df <- data.frame(
    			R2       = as.numeric(sumObj$r.squared[1]),
    			AdjR2    = as.numeric(sumObj$r.squared[2]),
    			Fstat    = as.numeric(sumObj$fstatistic[2]),
    			p_value   = as.numeric(sumObj$fstatistic[4]),
    			df_model = as.numeric(sumObj$df[1]),
    			df_resid = as.numeric(sumObj$df[2])
  			)
  	utils::write.table(model_df, file   = fileName, append = TRUE,  sep    = ",", col.names = TRUE, row.names = FALSE)
}

