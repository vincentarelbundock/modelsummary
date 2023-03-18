#' Persistent user settings for the `modelsummary` package
#'
#' @param output Default output format: "kableExtra", "gt", "flextable", "huxtable", "DT", or "markdown" 
#' @export
config_modelsummary <- function(output) {
    dn <- tools::R_user_dir(package = "modelsummary", which = "config")
    if (!dir.exists(dn)) dir.create(dn, recursive = TRUE)

    fn <- file.path(dn, "config.rds")
    if (!file.exists(fn)) {
        config <- list()
    } else {
        config <- readRDS(fn)
    }

    if (missing(output)) {
        return(config)
    }

    if (!missing(output)) {
        checkmate::assert_choice(output, c("kableExtra", "gt", "huxtable", "flextable", "DT", "markdown"), null.ok = TRUE)
        if (isTRUE(output == "kableExtra")) insight::check_if_installed("kableExtra")
        if (isTRUE(output == "gt")) insight::check_if_installed("gt")
        if (isTRUE(output == "huxtable")) insight::check_if_installed("huxtable")
        if (isTRUE(output == "flextable")) insight::check_if_installed("flextable")
        if (isTRUE(output == "DT")) insight::check_if_installed("DT")
        config[["output"]] <- output
    }

    saveRDS(config, file = fn)
}


config_get <- function(x) {
    fn <- file.path(tools::R_user_dir(package = "modelsummary", which = "config"), "config.rds")
    if (!file.exists(fn)) return(NULL)
    return(readRDS(fn)[[x]])
}