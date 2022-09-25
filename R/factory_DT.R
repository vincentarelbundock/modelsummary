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

  DT::datatable(
    tab,
    caption = title,
    escape = escape,
    rownames = FALSE,
    ...)

}
