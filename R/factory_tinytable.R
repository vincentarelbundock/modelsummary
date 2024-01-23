# TODO
# span
# hrule
# hgroup
# hindent
# dot align
# tab[[i]][idx] <- gsub("<", "&lt;", tab[[i]][idx])
# tab[[i]][idx] <- gsub(">", "&gt;", tab[[i]][idx])
 

#' Internal function to build table with `tinytable`
#'
#' @inheritParams factory_gt
#' @noRd
#' @return tinytable object
factory_tinytable <- function(tab,
                              align = NULL,
                              hrule = NULL,
                              hgroup = NULL,
                              hindent = FALSE,
                              notes = NULL,
                              title = NULL,
                              escape = TRUE,
                              ...) {

  insight::check_if_installed("tinytable")


  # tinytable arguments
  valid <- c("x", "theme", "placement", "width", "digits")

  arguments <- c(
    list(...),
    caption = title,
    align = align,
    notes = as.list(notes)
  )

  # # span: compute
  # span_list <- get_span_tinytable(tab)
  # if (!is.null(span_list) && settings_equal("output_format", c("tinytable", "html", "latex"))) {
  #   column_names <- attr(span_list, "column_names")
  #   if (!is.null(column_names)) {
  #     colnames(tab) <- column_names
  #   }
  # } else {
  #     colnames(tab) <- gsub("\\|{4}", " / ", colnames(tab))
  # }

  # create tables with combined arguments
  arguments <- arguments[base::intersect(names(arguments), valid)]
  arguments <- c(list(tab), arguments)
  out <- do.call(tinytable::tt, arguments)

  # align: other factories require a vector of "c", "l", "r", etc.
  if (!is.null(align)) {
    l <- length(align)
    align <- paste(align, collapse = "")
    out <- tinytable::style_tt(out, j = seq_len(l), align = align)
  }

  # output
  if (is.null(settings_get("output_file"))) {
    return(out)
  } else {
    tinytable::save_tt(out, output = settings_get("output_file"))
  }

}
