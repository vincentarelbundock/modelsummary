#' Draw a table from a data.frame
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @param hrule position of horizontal rules (integer vector)
#' @template citation
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
                           escape = TRUE,
                           ...) {

  sanitize_output(output)
  sanitize_escape(escape)

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
