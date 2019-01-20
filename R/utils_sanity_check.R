#' Internal function to check the sanity of various user inputs
#'
sanity_check <- function(gof_map = NULL) {
    if (!is.null(gof_map)) {
        if (!'data.frame' %in% class(gof_map)) {
            stop('`gof_map` must be a data.frame or a tibble.')
        }
        if (!all(c('raw', 'clean', 'fmt') %in% colnames(gof_map))) {
            stop('`gof_map` must have 3 columns named "raw", "clean", and "fmt".')
        }
    }
}
