#' Internal function to build table with `DT`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return DT object
factory_DT <- function(
  tab,
  align = NULL,
  hrule = NULL,
  notes = NULL,
  title = NULL,
  escape = TRUE,
  ...) {

  insight::check_if_installed("DT")

  colnames(tab) <- gsub("\\|\\|\\|\\|", " / ", colnames(tab))

  dots <- list(...)
  dots[["internal_call"]] <- NULL
  args <- list(
    tab,
    caption = title,
    escape = escape,
    rownames = FALSE)
  args <- c(args, dots)
  do.call(DT::datatable, args)
}
