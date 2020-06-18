#' datasummary template
#' 
#' @export
datasummary_skim <-  function(data,
                              type = 'numeric',
                              output = 'default',
                              fmt = '%.1f',
                              title = NULL,
                              notes = NULL,
                              histogram = FALSE,
                              ...) {

    # TODO error if there are no numeric
    if (type == 'numeric') {
        dat_new <- data
        if (histogram) {
            f <- All(dat_new, numeric = TRUE, factor = FALSE) ~ 
                 (Mean + SD + P0 + P25 + P50 + P75 + P100) * Arguments(fmt = fmt) + Histogram
        } else {
            f <- All(dat_new, numeric = TRUE, factor = FALSE) ~ 
                 (Mean + SD + P0 + P25 + P50 + P75 + P100) * Arguments(fmt = fmt)
        }
   
    # TODO error if there are no categorical
    } else if (type == 'categorical') {
        dat_new <- data %>%
                   dplyr::mutate(dplyr::across(where(is.character) | where(is.logical), as.factor))
        pctformat = function(x) sprintf("%.1f", x)
        f <- All(dat_new, numeric = FALSE, factor = TRUE) ~ 
            (N = 1) + (`%` = Percent()) * Format(pctformat())
    } else {
        stop('The `type` argument supports these values: "numeric", "categorical".')
    }
    
    # create table
    out <- datasummary(formula = f, 
                       data = dat_new,
                       output = output,
                       title = title,
                       notes)

    # output
    return(out)
    
}

