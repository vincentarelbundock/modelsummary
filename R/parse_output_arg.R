#' parse an `output` argument to determine which table factory and which output
#' format to use.
#'
#' @keywords internal
parse_output_arg <- function(output) {

    # kableExtra produces human-readable code
    if (output %in% c('markdown', 'html', 'latex')) {
        out <- list('output_factory' = 'kableExtra',
                    'output_file' = NULL,
                    'output_format' = output)
        return(out)
    }

    # file extension to guess format
    ext <- tools::file_ext(output)

    # output_file
    if (ext == '') {
        output_file <- NULL
    } else {
        output_file <- output
    }

    # output_format
    if (ext %in% c('md', 'Rmd', 'txt')) {
        output_format <- 'markdown'
    } else if (ext %in% c('tex', 'ltx')) {
        output_format <- 'latex'
    } else if (ext %in% c('docx', 'doc')) {
        output_format <- 'word'
    } else if (ext %in% c('pptx', 'ppt')) {
        output_format <- 'powerpoint'
    } else if (ext %in% c('png', 'jpg', 'rtf')) {
        output_format <- ext
    } else if (ext %in% c('htm', 'html')) {
        output_format <- 'html'
    } else {
        output_format <- output
    }

    # output_factory
    factory_list <- c('default' = getOption('modelsummary_default', default = 'gt'),
                      'dataframe' = 'dataframe',
                      'data.frame' = 'dataframe',
                      'flextable' = 'flextable',
                      'gt' = 'gt',
                      'html' = getOption('modelsummary_html', default = 'gt'),
                      'huxtable' = 'huxtable',
                      'jpg' = getOption('modelsummary_jpg', default = 'flextable'),
                      'kableExtra' = 'kableExtra',
                      'latex' = getOption('modelsummary_latex', default = 'kableExtra'),
                      'markdown' = 'kableExtra',
                      'png' = getOption('modelsummary_png', default = 'flextable'),
                      'powerpoint' =  getOption('modelsummary_powerpoint', default = 'flextable'),
                      'rtf' = getOption('modelsummary_rtf', default = 'gt'),
                      'word' =  getOption('modelsummary_word', default = 'flextable'))

    # sanity check: are user-supplied global options ok?
    sanity_factory(factory_list)

    output_factory <- factory_list[[output_format]]

    # gt cannot knit to latex. 
    if (output_factory == 'gt') {
        if (knitr::is_latex_output()) {
            output_format <- 'latex'
            output_factory <- 'kableExtra'
        }
    }

    # kableExtra must specify output_format ex ante
    if (output_factory == 'kableExtra') {
        if (output_format %in% c('default', 'kableExtra')) {
            automatic <- ifelse(knitr::is_latex_output(), 'latex', 'html')
            output_format <- getOption('modelsummary_kableExtra', default = automatic)
        }
    }

    # result
    out <- list('output_factory' = output_factory,
                'output_file' = output_file,
                'output_format' = output_format)
    return(out)

}

