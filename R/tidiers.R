#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' Tidy a multiple imputation `mice` pooled object
#'
#' @param x An object returned by one of the `mice::pool` function.
#' @param ... extra arguments (not used)
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
	out <- mice::pool(x, ...) %>%
           .$pooled %>%
           dplyr::mutate(term = row.names(.)) %>%
           tibble::as_tibble() %>%
           dplyr::select(term, order(names(.)))
    return(out)
}

#' Glance a multiple imputation `mice` pooled object
#'
#' @param x An object returned by one of the `mice::pool` function.
#' @param ... extra arguments (not used)
#' @return a tibble with one row
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
	out$n <- tryCatch(stats::nobs(x$analyses[[1]]), error = function(e) NULL)
	return(out)
}
