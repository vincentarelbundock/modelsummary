#' table1 template
#' 
#' @param formula 1-side formula with a single factor, character, or logical
#'   variable on the right-hand side.
#' @inheritParams modelsummary
#' @export
#' @keywords internal
datasummary_table1 <- function(formula,
                               data,
                               output = 'default',
                               fmt = '%.1f',
                               title = NULL,
                               notes = NULL,
                               ...) {

    # no left-hand support in this template
    sanity_ds_right_handed_formula(formula)
        
    # exclude RHS
    rhs <- labels(terms(formula))
    data_no_rhs <- data[, colnames(data) != rhs]
    
    # no factor variables
    any_factor <- any(sapply(data_no_rhs, is_categorical))
    
    if (any_factor) {
        # create table 1 formula
        f <- (`   ` = Ncol) +
        Literal('Mean (Std. Dev.)', nearData = FALSE) +
        All(data_no_rhs) * Heading() * MeanSD * Arguments(fmt = fmt) +
        Literal('Num. Obs. (%)', nearData = FALSE) +
        All(data_no_rhs, numeric = FALSE, factor = TRUE) * Heading() * Percent(denom = 'col', fn = NPercent)  ~ - 1
    } else {
        # create table 1 formula
        f <- (`   ` = Ncol) +
        Literal('Mean (Std. Dev.)', nearData = FALSE) +
        All(data_no_rhs) * Heading() * MeanSD * Arguments(fmt = fmt) ~ - 1
    }
    
    f <- update(f, paste('. ~ . +', rhs, '+ 1'))
    
    # create table
    out <- datasummary(formula = f, 
                       data = data,
                       output = output,
                       title = title,
                       notes,
                       ...)
    
    # output
    return(out)
    
}