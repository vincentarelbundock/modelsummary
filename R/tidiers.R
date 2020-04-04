#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' Tidy a multiple imputation `mice` object
#'
#' @param x An object with multiply-imputed models from `mice` (class: `mira`)
#' @param ... extra arguments (not used, unless confidence intervals are requested for multiple `lm` models)
#'
#' @note
#' Available stats in mipo object:
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
#' }
#'
#' @note
#' If `mira` object consists of `lm`-models, additional results can be returned to facilitate side-by-side tables with other `lm`-model:
#' \itemize{
#'      \item p.value
#'      \item conf.low (if called with conf.int = TRUE)
#'      \item conf.high (if called with conf.int = TRUE)
#' }
#' @examples
#' library(mice)
#' data <- airquality
#' data[4:10,3] <- rep(NA,7)
#' data[1:5,4] <- NA
#' tmp <- mice(data,m=5, seed=500, printFlag = FALSE)
#' mod <- with(tmp, lm(Temp~ Ozone+Solar.R+Wind))
#' tidy(mod)
#'
#' @return a tibble with one row per term
#' @export
#' @family tidiers
tidy.mira <- function(x, ...) {
    if (class(x$analyses[[1]]) == "lm") {
        out <- tidy.mira.lm(x, ...)
        return(out)
    } else {
	out <- mice::pool(x, ...) %>%
           .$pooled %>%
	       dplyr::mutate(term = as.character(term)) %>%
           tibble::as_tibble() %>%
           dplyr::select(term, order(names(.)))
    return(out)
    }
}

#' Tidy  multiple imputations of `lm` models created with `mice`
#'
#' @param x A `mira` object containing multiple lm-models based on `mice` imputations.
#' @param conf.int Logical. Should confidence intervals be returned. Defaults to true.
#' @param conf.level Confidence level for intervals. Defaults to .95
#' @param ... extra arguments (not used)
#' @export
#'
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
tidy.mira.lm <- function(x, conf.int = TRUE, conf.level = .95, ...) {
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
#' @family tidiers
glance.mira <- function(x, ...) {
    out <- tibble::tibble('m' = length(x$analyses))
	out$nobs <- tryCatch(stats::nobs(x$analyses[[1]]), error = function(e) NULL)
	if (class(x$analyses[[1]]) == "lm") {
	    out$r.squared <- mice::pool.r.squared(x, adjusted = FALSE)[1]
	    out$adj.r.squared <- mice::pool.r.squared(x, adjusted = TRUE)[1]
	}
	return(out)
}
