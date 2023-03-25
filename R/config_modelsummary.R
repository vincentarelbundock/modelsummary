#' Persistent user settings for the `modelsummary` package
#'
#' @param output Default output format: "kableExtra", "gt", "flextable", "huxtable", "DT", or "markdown" 
#' @param factory_latex Name of package used to generate LaTeX output when `output="latex"`.
#' @param factory_markdown Name of package used to generate LaTeX output when `output="markdown"`.
#' @param factory_html Name of package used to generate LaTeX output when `output="html"`.
#' @param reset TRUE to return to default settings.
#' @export
config_modelsummary <- function(
    output,
    factory_latex,
    factory_html,
    factory_markdown,
    reset = FALSE) {

    dn <- tools::R_user_dir(package = "modelsummary", which = "config")
    if (!dir.exists(dn)) dir.create(dn, recursive = TRUE)

    fn <- file.path(dn, "config.rds")
    if (!file.exists(fn)) {
        config <- list()
    } else {
        config <- readRDS(fn)
    }

    if (isTRUE(reset)) {
        hush(unlink(fn))
        insight::format_warning("`modelsummary` returned to default settings.")
    }

    if (missing(output) && missing(factory_latex) && missing(factory_html) && missing(factory_markdown)) {
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

    if (!missing(factory_latex)) {
        config[["factory_latex"]] <- factory_latex
    }
    if (!missing(factory_html)) {
        config[["factory_html"]] <- factory_html
    }
    if (!missing(factory_markdown)) {
        config[["factory_markdown"]] <- factory_markdown
    }

    saveRDS(config, file = fn)
}


config_get <- function(x) {
    fn <- file.path(tools::R_user_dir(package = "modelsummary", which = "config"), "config.rds")
    if (!file.exists(fn)) return(NULL)
    return(readRDS(fn)[[x]])
}