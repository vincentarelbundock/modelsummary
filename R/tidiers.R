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
         out <- summary(mice::pool(x), type = "all", conf.int = conf.int, conf.level = conf.level) %>%
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
#' \dontrun{
#' library(mice)
#' data <- airquality
#' data[4:10,3] <- rep(NA,7)
#' data[1:5,4] <- NA
#' tmp <- mice(data,m=5, seed=500, printFlag = FALSE)
#' mod <- with(tmp, lm(Ozone ~ Solar.R + Wind))
#' glance(mod)
#' }
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

#' Tidy multiple imputation models created with `mitools::MIcombine`
#'
#' @param x A `mira` object containing multiple models based on `mice` imputations.
#' @param ... extra arguments (not used)
#' @return a dataframe with one row per term
#' @export
#' @keywords internal
tidy.MIresult <- function(x, ...) {
    nil <- utils::capture.output(out <- summary(x, ...))
    out <- out %>% 
           tibble::rownames_to_column('term') %>%
           stats::setNames(c('term', 'estimate', 'std.error', 'conf.low', 'conf.high', 'missing.pct'))
    out
}

#' Glance a multiple imputation `mitools::MIcombine` pooled object
#'
#' @param x An object with multiply-imputed models from `mice` (class: `mira`)
#' @param ... extra arguments (not used)
#' @return a dataframe with one row
#' @export
#' @keywords internal
#' @family tidiers
glance.MIresult <- function(x, ...) {
    out <- data.frame('nimp' = x$nimp,
                      'nobs' = NA)
    out
}

#' Tidy a `fixest` model
#'
#' @param x A model object produced by the `fixest::feols` or `fixest::feglm` functions
#' @param ... extra arguments (not used)
#' @return a dataframe with one row per term
#' @export
#' @keywords internal
#' @family tidiers
tidy.fixest <- function(x, conf.int = FALSE, conf.level = .95, ...) {
    out <- x$coeftable
    cols <- c('estimate', 'std.error', 'statistic', 'p.value')
    colnames(out) <- cols
    out$term <- row.names(out)
    out <- out[, c('term', cols)]
    if (conf.int) {
        ci <- stats::confint(x, level = conf.level)
        colnames(ci) <- c('conf.low', 'conf.high')
        ci$term <- row.names(ci)
        out <- merge(out, ci)
    }
    row.names(out) <- NULL
    out
}

#' Glance a `fixest` model
#'
#' @param x A model object produced by the `fixest::feols` or `fixest::feglm` functions
#' @param ... extra arguments (not used)
#' @return a dataframe with one row
#' @export
#' @keywords internal
#' @family tidiers
glance.fixest <- function(x, ...) {
    out <- data.frame('nobs' = x$nobs)
	if(x$method == "feols"){
        out$logLik <- stats::logLik(x)
        out$adj.r.squared <- fixest::r2(x, 'ar2')
	    if(!is.null(x$fixef_sizes) && is.null(x$onlyFixef)){
            out$r.squared.within <- fixest::r2(x, 'wr2')
	    }
	} else {
		bic_ll = formatBicLL(stats::BIC(x), x$loglik)
        out$logLik <- bic_ll$bic
        out$logLik <- bic_ll$ll
        out$adj.pseudo.r.squared <- bic_ll$pseudo_r2
        out$squared.cor <- bic_ll$sq.cor
	}
	out$std.error.type <- attr(x$coeftable, "type")
    if ("fixef_vars" %in% names(x)) {
        for (fe in x$fixef_vars) {
            out[[paste('FE:', fe)]] <- 'X'
        }
    }
    out
}
