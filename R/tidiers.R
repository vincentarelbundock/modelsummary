# adapted from tidymodels/broom, under MIT license

##' @export
#tidy.coeftest <- function(x, conf.int = FALSE, conf.level = .95, ...) {
#  co <- as.data.frame(unclass(x))
#  colnames(co) <- c("estimate", "std.error", "statistic", "p.value") 
#  co$term <- row.names(co)
#  if (conf.int) {
#    ci <- stats::confint(x, level = conf.level)
#    ci <- as.data.frame(ci)
#    colnames(ci) <- c("conf.low", "conf.high")
#    ci$term <- row.names(ci)
#    out <- merge(co, ci, all.x=TRUE, by="term", sort=FALSE)
#  } else {
#    out <- co
#  }
#  row.names(out) <- NULL
#  out
#}

##' @export
#glance.coeftest <- function(x, ...) {
#  out <- data.frame(
#    logLik = sprintf('%.3f', stats::logLik(x)), 
#    AIC = stats::AIC(x), 
#    BIC = stats::BIC(x), 
#    nobs = stats::nobs(x))
#  return(out)
#}
