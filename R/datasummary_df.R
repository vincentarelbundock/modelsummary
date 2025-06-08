#' Draw a table from a data.frame
#'
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @param hrule position of horizontal rules (integer vector)
#' @template kableExtra2tinytable
#' @template citation
#' @export
datasummary_df <- function(
  data,
  output = getOption("modelsummary_output", default = "default"),
  fmt = 2,
  align = getOption("modelsummary_align", default = NULL),
  hrule = getOption("modelsummary_hrule", default = NULL),
  title = getOption("modelsummary_title", default = NULL),
  notes = getOption("modelsummary_notes", default = NULL),
  add_rows = getOption("modelsummary_add_rows", default = NULL),
  add_columns = getOption("modelsummary_add_columns", default = NULL),
  escape = getOption("modelsummary_escape", default = TRUE),
  ...
) {
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

  out <- factory(
    data,
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
    ...
  )

  settings_rm()
  return(out)
}
