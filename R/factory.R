#' Factory to create tables in different output formats using standardized
#' inputs.
#'
#' @export
factory <- function(tab,
                    hrule,
                    notes,
                    output,
                    span,
                    title) {


    # sanity check functions are hosted in R/sanity_checks.R
    # more sanity checks are conducted in modelsummary::extract()
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
    }

    # build table
    f(tab, 
      hrule = hrule,
      notes = notes,
      output_file = output_list$output_file,
      output_format = output_list$output_format,
      span = span,
      title = title)
      
}
