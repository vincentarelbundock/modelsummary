#' Quick overview of numeric or categorical variables
#' 
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @param histogram TRUE to include a unicode character histogram (boolean)
#' @param type of variables to summarize: "numeric" or "categorical" (character)
#' @export
datasummary_skim <-  function(data,
                              type = 'numeric',
                              output = 'default',
                              fmt = '%.1f',
                              histogram = FALSE,
                              title = NULL,
                              notes = NULL,
                              align = NULL,
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

        nnumeric <- sum(sapply(dat_new, is.numeric))
        if (nnumeric == 0) {
            stop('datasummary_skim(type="numeric") only works with numeric variables. There is such variable in your dataset.')
        }
  
    # TODO error if there are no categorical
    } else if (type == 'categorical') {
        dat_new <- data %>%
                   dplyr::mutate(dplyr::across(where(is.character) | 
                                               where(is.logical), as.factor))
        pctformat = function(x) sprintf("%.1f", x)
        f <- All(dat_new, numeric = FALSE, factor = TRUE) ~ 
            (N = 1) + (`%` = Percent()) * Format(pctformat())

        nfactor <- sum(sapply(dat_new, is.factor))
        if (nfactor == 0) {
            stop('datasummary_skim(type="categorical") only works with logical, character, and factor variables. There is such variable in your dataset.')
        }

    } else {
        stop('The `type` argument supports these values: "numeric", "categorical".')
    }

    # create table
    out <- datasummary(formula = f, 
                       data = dat_new,
                       output = output,
                       title = title,
                       align = align,
                       notes = notes)

    # output
    return(out)
    
}

