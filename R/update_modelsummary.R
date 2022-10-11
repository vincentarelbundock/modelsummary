#' Update `modelsummary` and its dependencies
#'
#' Update `modelsummary` and its dependencies to the latest development or CRAN versions. The R session needs to be restarted after install.
#' 
#' @param source one of two strings: "development" or "cran"
#' @export
update_modelsummary <- function(source = "development") {
    checkmate::assert_choice(source, choices = c("development", "cran"))
    insight::check_if_installed("remotes")
    if (source == "development") {
        remotes::install_github("easystats/insight")
        remotes::install_github("easystats/parameters")
        remotes::install_github("easystats/performance")
        remotes::install_github("vincentarelbundock/modelsummary")
    } else {
        update.packages(c("insight", "parameters", "performance", "modelsummary"))
    }
    insight::format_alert("Please restart your R session for the changes to take effect.")
}