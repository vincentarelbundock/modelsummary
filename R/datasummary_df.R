#' Draw a table from a data.frame
#'
#' @inheritParams modelsummary
#' @inheritParams datasummary
#' @inheritParams factory
#' @export
datasummary_df <- function(data,
                           output = "default",
                           fmt = "%.2f",
                           align = NULL,
                           hrule = NULL,
                           title = NULL,
                           notes = NULL,
                           add_rows = NULL,
                           add_columns = NULL,
                           ...) {

  sanity_output(output)

  checkmate::assert_data_frame(data)

  for (n in colnames(data)) {
    data[[n]] <- sprintf(fmt, data[[n]])
  }

  factory(data,
          align=align,
          hrule=hrule,
          notes=notes,
          output=output,
          title=title,
          add_rows=add_rows,
          add_columns=add_columns,
          ...)

}
