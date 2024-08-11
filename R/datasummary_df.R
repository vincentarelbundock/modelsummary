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

  settings_init(settings = list("function_called" = "datasummary_df"))

  tmp <- sanitize_output(output) # before sanitize_escape
  output_format <- tmp$output_format
  output_factory <- tmp$output_factory
  output_file <- tmp$output_file
  sanitize_escape(escape) # after sanitize_output
  sanity_align(align)

  checkmate::assert_data_frame(data)

  for (n in colnames(data)) {
    fmt <- sanitize_fmt(fmt)
    data[[n]] <- fmt(data[[n]])
  }

  out <- factory(data,
          align = align,
          hrule = hrule,
          notes = notes,
          output = output,
          title = title,
          escape = escape,
          add_rows = add_rows,
          add_columns = add_columns,
          output_factory = output_factory,
          output_format = output_format,
          output_file = output_file,
          ...)

  settings_rm()
  return(out)

}
