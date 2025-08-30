#' Create a named vector of variable labels from a list of models
#' 
#' @param models A model or a list of models
#' @return A named vector of variable labels
#' @examples
#' \dontrun{
#' dat <- mtcars
#' dat %<>%
#'   labelled::set_variable_labels(
#'     mpg = "Miles/(US) gallon",
#'     cyl = "Number of cylinders",
#'   )
#' dat$gear %<>% factor()
#' mod <- lm(mpg ~ cyl + disp, dat)
#' modelsummary::get_variable_labels_models(mod)
#' }
get_variable_labels_models <- function(models) {
  # If we pass a single model, convert to list
  if (!is.list(models)) {
    models <- list(models)
  }
  # Loop over models, extract data, extract labels
  out <- list()
  for (mod in models) {
    dat <- hush(insight::get_data(mod))
    lab <- get_variable_labels_data(dat)
    out <- append(out, list(lab))
  }
  out <- lapply(out, function(x) data.frame(clean = names(x), raw = x))
  out <- do.call("rbind", out)
  out <- unique(out)
  if (anyDuplicated(out$clean)) {
    msg <-
      "Some variables share a name but have inconsistent labels. The labels will be ignored."
    warning(msg, call. = FALSE)
  }
  out <- stats::setNames(out$raw, out$clean)
  return(out)
}

#' Return labels from data frame
#' 
#' @param data A data frame
#' @return A named vector of variable labels
#' @examples
#' \dontrun{
#' dat <- mtcars
#' dat %<>%
#'   labelled::set_variable_labels(
#'     mpg = "Miles/(US) gallon",
#'     cyl = "Number of cylinders",
#'   )
#' dat$gear %<>% factor()
#' mod <- lm(mpg ~ cyl + disp, dat)
#' modelsummary::get_variable_labels_data(insight::get_data(mod))
#' }
get_variable_labels_data <- function(data) {
  # global variables: sjlabelled-style
  lab <- attr(data, "label", exact = TRUE)
  # variable attributes: haven-style
  if (is.null(lab)) {
    # Extract labels from data vector
    lab <- sapply(data, function(x) attr(x, "label", exact = TRUE))
    # Use variable name as label for variables that miss a label
    mask <- sapply(lab, is.null)
    lab[mask] <- names(lab)[mask]
    # !! `unlist` would drop NULL values
    lab <- unlist(lab)
  }
  if (length(lab) == 0) {
    lab <- NULL
  }
  return(lab)
}


coef_rename_labels <- function(x, dict) {
  out <- x
  for (i in seq_along(dict)) {
    # escape because user-supplied labels could include special regex characters like parentheses.
    # substrings are dangerous because they could be subbed twice. We
    # probably can't format `cyl4`, `cyl6` because those could be
    # user-supplied variable names.
    # pad otherwise we get ugly labels like "Cylinders6"
    tar <- dict[i]
    src <- names(dict)[i]
    src <- gsub("(\\W)", "\\\\\\1", src) # escape parentheses so they don't catch in regex
    out <- gsub(sprintf("^%s$", src), tar, out, perl = TRUE)
    out <- gsub(sprintf("^%s:", src), paste0(tar, ":"), out, perl = TRUE)
    out <- gsub(sprintf(":%s$", src), paste0(":", tar), out, perl = TRUE)
    out <- gsub(sprintf(":%s:", src), paste0(":", tar, ":"), out, perl = TRUE)
    out <- gsub(
      sprintf("factor\\(%s\\)", src),
      paste0(tar, " "),
      out,
      perl = TRUE
    )
  }
  out <- trimws(out)
  return(out)
}


strip_labels <- function(data) {
  for (x in colnames(data)) {
    class(data[[x]]) <- setdiff(
      class(data[[x]]),
      c("haven_labelled", "vctrs_vctr")
    )
    attr(data[[x]], "label") <- NULL
  }
  return(data)
}
