#' Factory to create tables in different output formats using standardized
#' inputs.
#'
#' @param tab table body (data.frame)
#' @param hrule position of horizontal rules (integer vector)
#' @param span list of column label spans
#' @inheritParams modelsummary
factory <- function(tab,
                    align = NULL,
                    hrule,
                    notes,
                    output,
                    span,
                    title) {


    # sanity check functions are hosted in R/sanity_checks.R
    # more sanity checks are conducted in modelsummary:::extract()
    sanity_align(align, tab)
    sanity_output(output)
    sanity_title(title)
    sanity_notes(notes)

    # parse output
    output_list <- parse_output_arg(output)

    if (output_list$output_factory == 'gt') {
        f <- factory_gt
    } else if (output_list$output_factory == 'kableExtra') {
        f <- factory_kableExtra
    } else if (output_list$output_factory == 'flextable') {
        f <- factory_flextable
    } else if (output_list$output_factory == 'huxtable') {
        f <- factory_huxtable
    } else if (output_list$output_factory == 'dataframe') {
        f <- factory_dataframe
    }

    # de-duplicate columns with whitespace
    colnames(tab) <- pad(colnames(tab))

    # build table
    f(tab, 
      align = align,
      hrule = hrule,
      notes = notes,
      output_file = output_list$output_file,
      output_format = output_list$output_format,
      span = span,
      title = title)
      
}
