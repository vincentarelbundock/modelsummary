#' datasummary template to create a correlation table
#' 
#' @inheritParams datasummary
#' @export
datasummary_correlation <- function(data,
                                    output = 'default',
                                    title = NULL,
                                    notes = NULL) {
                                        
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
    
    factory(out, 
            hrule = NULL,
            notes = notes, 
            output = output,
            span = NULL,
            title = title)
    
}
