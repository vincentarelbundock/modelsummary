#' Internal function to build table with `data.frame` with other arguments as
#' attributes. Useful for testing.
#'
#' @inheritParams modelsummary
#' @keywords internal
#' @return data.frame 
factory_dataframe <- function(tab,
                              hrule = NULL,
                              notes = NULL,
                              output_file = NULL,
                              output_format = NULL,
                              span = NULL,
                              title = NULL) {


    out <- tab
    attr(out, 'hrule') <- hrule
    attr(out, 'notes') <- notes
    attr(out, 'output_file') <- output_file
    attr(out, 'output_format') <- output_format
    attr(out, 'span') <- span
    attr(out, 'title') <- title

    return(out)

}
