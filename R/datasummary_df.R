#' Draw a table from a data.frame
#'
#' @inheritParams modelsummary
#' @inheritParams datasummary
#' @param hrule position of horizontal rules (integer vector)
#' @export
datasummary_df <- function(data,
                           output = "default",
                           fmt = 2,
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
    data[[n]] <- rounding(data[[n]], fmt)
  }

  factory(data,
          align = align,
          hrule = hrule,
          notes = notes,
          output = output,
          title = title,
          add_rows = add_rows,
          add_columns = add_columns,
          ...)

}
