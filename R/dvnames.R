#' Title models with their dependent variables
#'
#' A convenience function for use with a regression model or list of regression models. Returns a named list of models, where the names are the models' respective dependent variables. Pass your list of models to \code{dvnames} before sending to \code{modelsummary} to automatically get dependent variable-titled columns.
#'
#' @param models A regression model or list of regression models
#' @param number Should the models be numbered (1), (2), etc., in addition to their dependent variable names?
#' @param fill If \code{insight::find_response()} cannot find a response, the column title to use in its place. Set to \code{' '} to leave blank.
#'
#' @examples
#'
#' m1 <- lm(mpg ~ hp, data = mtcars)
#' m2 <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' # Without dvnames, column names are Model 1 and Model 2
#' modelsummary(list(m1, m2))
#'
#' # With dvnames, they are "mpg" and "mpg"
#' modelsummary(dvnames(list(m1,m2)))
#'
#' @export

dvnames <- function(models, number = FALSE, fill = 'Model') {
    if (class(models)[1] != 'list') {
        models <- list(models)
    }

    # Get dependent variables
    dvs <- sapply(models, insight::find_response)
    # Replace nulls with fill
    dvs <- sapply(dvs, function(x)
        ifelse(is.null(x), fill, x))
    # Append numbers
    if (number) {
        dvs <- paste0(dvs, ' (', 1:length(dvs), ')')
    }

    # Apply names
    names(models) <- dvs

    return(models)
}
