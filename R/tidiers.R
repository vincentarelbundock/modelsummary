#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' Tidy  multiple imputation models created with `mice`
#'
#' @param x A `mira` object containing multiple models based on `mice` imputations.
#' @param conf.int Logical. Should confidence intervals be returned. Defaults to true.
#' @param conf.level Confidence level for intervals. Defaults to .95
#' @param ... extra arguments (not used)
#' @export
#' @keywords internal
#' @note
#' Available stats in result:
#' \itemize{
#'      \item estimate
#'      \item ubar
#'      \item b
#'      \item t
#'      \item dfcom
#'      \item df
#'      \item riv
#'      \item lambda
#'      \item fmi
#'      \item p.value
#'      \item conf.low (if called with conf.int = TRUE)
#'      \item conf.high (if called with conf.int = TRUE)
#' }
tidy.mira <- function(x, conf.int = TRUE, conf.level = .95, ...) {
         out <- summary(mice::pool(x, ...), type = "all", conf.int = conf.int, conf.level = conf.level) %>%
                dplyr::mutate(term = as.character(term)) %>%
                tibble::as_tibble()

         conf_vars <- names(out)[stringr::str_detect(names(out), "%")]
         names(out)[names(out) %in% conf_vars] <- c("conf.low", "conf.high")

         out <- out %>% dplyr::select(term, order(names(.)))
         return(out)
}

#' Glance a multiple imputation `mice` pooled object
#'
#' @param x An object with multiply-imputed models from `mice` (class: `mira`)
#' @param ... extra arguments (not used)
#' @return a tibble with one row
#'
#' @note If x contains `lm` models, R2 is included in the output
#'
#' @examples
#' library(mice)
#' data <- airquality
#' data[4:10,3] <- rep(NA,7)
#' data[1:5,4] <- NA
#' tmp <- mice(data,m=5, seed=500, printFlag = FALSE)
#' mod <- with(tmp, lm(Ozone ~ Solar.R + Wind))
#' glance(mod)
#'
#' @export
#' @keywords internal
#' @family tidiers
glance.mira <- function(x, ...) {
    out <- tibble::tibble('nimp' = length(x$analyses))
	out$nobs <- tryCatch(stats::nobs(x$analyses[[1]]), error = function(e) NULL)
	if (class(x$analyses[[1]])[1] == "lm") {
	    out$r.squared <- mice::pool.r.squared(x, adjusted = FALSE)[1]
	    out$adj.r.squared <- mice::pool.r.squared(x, adjusted = TRUE)[1]
	}
	return(out)
}
