#' Update `modelsummary` and its dependencies
#'
#' Update `modelsummary` and its dependencies to the latest R-Universe or CRAN versions. The R session needs to be restarted after install.
#'
#' @param source one of two strings: "development" or "cran"
#' @export
update_modelsummary <- function(source = "development") {
  checkmate::assert_choice(source, choices = c("development", "cran"))
  if (source == "development") {
    repo_easystats <- "https://easystats.r-universe.dev"
    repo_vab <- "https://vincentarelbundock.r-universe.dev"
  } else {
    repo_vab <- repo_easystats <- getOption("repos")["CRAN"]
  }
  utils::install.packages("insight", repos = repo_easystats)
  utils::install.packages("parameters", repos = repo_easystats)
  utils::install.packages("performance", repos = repo_easystats)
  utils::install.packages("modelsummary", repos = repo_vab)
  utils::install.packages("tinytable", repos = repo_vab)
  msg <- "Please restart your R session"
  msg <- c("", strrep("#", nchar(msg)), msg, strrep("#", nchar(msg)))
  insight::format_alert(msg)
}
