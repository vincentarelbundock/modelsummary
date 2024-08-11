#' Internal function to build table with `data.frame` with other arguments as
#' attributes. Useful for testing.
#'
#' @inheritParams factory_gt
#' @return data.frame
#' @noRd
factory_dataframe <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              notes = NULL,
                              add_rows = NULL,
                              title = NULL,
                              output_file = NULL,
                              output_format = NULL,
                              ...) {

  out <- tab

  # empty higher level headers in datasummary
  colnames(out) <- gsub("^\\|\\|\\|\\|", "", colnames(out))
  colnames(out) <- gsub("\\|\\|\\|\\|$", "", colnames(out))

  # secret internal arguments
  if (!isTRUE(list(...)$internal_call)) {
    colnames(out) <- gsub("\\|\\|\\|\\|", " / ", colnames(out))
    colnames(out) <- ifelse(colnames(out) == " ", colnames(out), trimws(colnames(out)))
    colnames(out) <- pad(trimws(colnames(out)), output_format = output_format)
  }

  # factor -> character (useful for R<4.0.0)
  for (i in seq_along(out)) {
    if (is.factor(out[[i]])) {
      out[[i]] <- as.character(out[[i]])
    }
  }


  attr(out, 'align') <- align
  attr(out, 'hrule') <- hrule
  attr(out, 'notes') <- notes
  attr(out, 'output_file') <- output_file
  attr(out, 'output_format') <- output_format
  attr(out, 'title') <- title

  row.names(out) <- NULL

  # theme
  theme_ms <- getOption("modelsummary_theme_dataframe", default = identity)
  out <- theme_ms(out)

  # write spreadsheet to file
  if (isTRUE(tools::file_ext(output_file) == "csv")) {
    utils::write.csv(out, file = output_file, row.names = FALSE)
  } else if (isTRUE(tools::file_ext(output_file) == "xlsx")) {
    insight::check_if_installed("openxlsx")
    openxlsx::write.xlsx(out, file = output_file)
  }

  return(out)

}
