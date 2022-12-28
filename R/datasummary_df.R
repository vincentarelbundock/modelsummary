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

  sanitize_output(output) # before sanitize_escape
  sanitize_escape(escape) # after sanitize_output

  checkmate::assert_data_frame(data)

  notes <- escape_string(notes)
  title <- escape_string(title)

  for (n in colnames(data)) {
    fmt <- sanitize_fmt(fmt)
    data[[n]] <- fmt(data[[n]])
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
