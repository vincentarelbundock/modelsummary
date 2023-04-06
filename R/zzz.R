.onLoad <- function(libname, pkgname){
    pkgs <- c("kableExtra", "gt", "flextable", "huxtable", "DT")
    flags <- insight::check_if_installed(pkgs, quietly = TRUE)
    if (!any(flags) && !isTRUE(config_get("factory_default") == "markdown")) {
        msg <- c(
            '`modelsummary` has built-in support to draw text-only (markdown) tables. To generate tables in other formats, you must install one or more of these libraries:',
            '
install.packages(c(
    "kableExtra",
    "gt",
    "flextable",
    "huxtable",
    "DT"
))
',
            'Alternatively, you can set markdown as the default table format to silence this alert:',
            '
config_modelsummary(factory_default = "markdown")
')
        insight::format_alert(msg)
    }
}