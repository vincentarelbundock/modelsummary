#' Internal function to build table with `gt`
#'
#' @inheritParams modelsummary
#' @param stars_note passed by `modelsummary()`
#' @keywords internal
#' @return tbl_gt object
build_kableExtra <- function(tab,
                             title,
                             stars,
                             stars_note,
                             notes,
                             filename,
                             output,
                             ...) {

    # clean and measure table
    idx_row <- match('gof', tab$group)
    tab <- tab %>%
           dplyr::select(-statistic, -group) %>%
           dplyr::rename(`       ` = term) # HACK: arbitrary 7 spaces to avoid name conflict
    idx_col <- ncol(tab)

    # output type
    if (output == 'default') {
        output <- 'html'
    }
    ext <- tools::file_ext(output)

    # kable object type based on output 
    if ((output == 'latex') | (ext == 'tex')) {
        tab <- kableExtra::kable(tab, format = 'latex', caption = title,
                                 booktabs = TRUE, linesep = "")

    } else if ((output == 'markdown') | (ext %in% c('md', 'txt'))) {
        tab <- kableExtra::kable(tab, format = 'markdown', caption = title)

    } else if ((output == 'html') | (ext %in% c('htm', 'html'))) {
        tab <- kableExtra::kable(tab, format = 'html', caption = title)
    }

    # horizontal rule to separate coef/gof
    if ((output == 'latex') | (ext == 'tex')) {
        if (!is.na(idx_row)) { # check if there are >0 GOF
            tab <- tab %>%
                   kableExtra::row_spec(idx_row - 1, extra_latex_after = '\\midrule')
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
    ext <- tools::file_ext(output)
    if (output %in% c('markdown', 'html', 'latex')) {
        return(tab)
    } else {
        # function stolen from kableExtra (MIT license)
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
