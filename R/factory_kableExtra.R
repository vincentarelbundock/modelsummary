#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return tbl_gt object
factory_kableExtra <- function(tab,
                               title,
                               stars,
                               stars_note,
                               notes,
                               gof_idx,
                               output,
                               ...) {
  
    # kableExtra needs to know the output format ex ante 
    ext <- tools::file_ext(output)

    # modelsummary call 
    if (output %in% c('html', 'latex', 'markdown')) {
        output_format <- output
    } else if (ext == 'tex') {
        output_format <- 'latex'
    } else if (ext %in% c('md', 'txt', 'Rmd')) {
        output_format <- 'markdown'
    } else if (ext %in% c('htm', 'html')) {
        output_format <- 'html'

    # global options
    } else if (output %in% c('default', 'kableExtra')) {
        output_format <- getOption('modelsummary_kableExtra')

        if (is.null(output_format)) {
            output_format <- getOption('knitr.table.format')

            if (is.null(output_format)) {

                if (knitr::is_latex_output()) {
                    output_format <- 'latex'
                } else {
                    output_format <- 'html'
                }

            }

        }
    } 

    if (!is.null(output_format)) {
        tab <- kableExtra::kable(tab,
                                 format = output_format,
                                 caption = title,
                                 booktabs = TRUE, 
                                 linesep = "")
    } 

    # horizontal rule to separate coef/gof
    if (output_format != 'markdown') {
        if (!is.na(gof_idx)) { # check if there are >0 GOF
            tab <- tab %>%
                   kableExtra::row_spec(gof_idx - 1, 
                                        extra_latex_after = '\\midrule') %>%
                   kableExtra::kable_styling()
        }
    }

    # stars note
    stars_note <- make_stars_note(stars)
    if (!is.null(stars_note)) {
        tab <- tab %>% 
               kableExtra::add_footnote(label = stars_note, notation = 'none')
    }

    # user-supplied notes at the bottom of table
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- tab %>% 
                   kableExtra::add_footnote(label = n, notation = 'none')
        }
    }

    # output
    if (ext == '') {
        return(tab)
    } else {
        # function stolen from kableExtra (MIT license). Not exported and CRAN
        # doesn't like :::
        solve_enc <- function(x) {
            out <- enc2utf8(as.character(base::format(x, trim = TRUE, justify = 'none')))
            mostattributes(out) <- attributes(x)
            return(out)
        }
        filecon <- file(output)
        writeLines(solve_enc(tab), con = filecon, useBytes = TRUE)
        close(filecon)
    } 

}
