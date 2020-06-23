#' datasummary template to create a correlation table
#' 
#' @inheritParams datasummary
#' @export
datasummary_correlation <- function(data,
                                    output = 'default',
                                    title = NULL,
                                    notes = NULL,
                                    ...) {
                                        
    # output: factory, file, format
    output_list <- parse_output_arg(output)
                                        
    clean_r <- function(x) {
        x <- sprintf("%.2f", x)
        x <- stringr::str_replace(x, '0\\.', '\\.')
        x <- stringr::str_replace(x, '1\\.00', '1')
        return(x)
    }
    
    nvar <- ncol(data)
    out <- data %>% 
           dplyr::select(where(is.numeric)) %>% 
           stats::cor(use = 'pairwise.complete.obs') %>% 
           data.frame %>%
           tibble::rownames_to_column() %>%
           dplyr::mutate(dplyr::across(where(is.numeric), clean_r))
    
    for (i in 1:nrow(out)) {
        for (j in 2:ncol(out)) {
            out[i, j] <- ifelse(i + 1 < j, '', out[i, j])
        }
    }
    colnames(out)[1] <- ' '

    # greenfield
    if (output_list$output_factory == 'gt') {
        factory <- factory_gt
    } else if (output_list$output_factory == 'kableExtra') {
        factory <- factory_kableExtra
    } else if (output_list$output_factory == 'flextable') {
        factory <- factory_flextable    
    } else if (output_list$output_factory == 'huxtable') {
        factory <- factory_huxtable
    }
    
    out <- factory(out, 
                   output_format = output_list$output_format,
                   output_file = output_list$output_file,
                   title = title, 
                   notes = notes, 
                   span = NULL,
                   #align = strrep('r', ncol(out)), # for kableExtra, ignored by others
                   ...)
    
    return(out)
}
