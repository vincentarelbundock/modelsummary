factory_typst <- function(
  tab,
  align = NULL,
  ...) {

  align[align == "c"] <- "center"
  align[align == "l"] <- "left"
  align[align == "r"] <- "right"
  out <- typstable(tab, align = align, ...)

  if (insight::check_if_installed("knitr", quietly = TRUE)) {
    if (isTRUE(knitr::pandoc_to() == "typst")) {
        out <- paste0("```{=typst}\n", out, "\n```\n")
        out <- knitr::asis_output(out)
    }
  }

  return(out)
}


typstable <- function(
    x,
    align = "left",
    columns = "auto",
    inset = "5pt",
    quarto = TRUE,
    ...) {

    checkmate::assert_data_frame(x)

    # align:
    checkmate::assert(
        checkmate::check_character(align, len = 1),
        checkmate::check_character(align, len = ncol(x))
    )
    checkmate::assert_true(
        all(align %in% c("auto", "left", "right", "center"))
    )
    if (isTRUE(length(align) == 1)) {
        align <- rep(align, ncol(x))
    }
    align <- sprintf(
        "(%s)", paste(align, collapse = ", ")
    )

    # columns:
    checkmate::assert(
        checkmate::check_character(columns, len = 1),
        checkmate::check_character(columns, len = ncol(x))
    )
    checkmate::assert_true(
        all(columns %in% c("auto", "left", "right", "center"))
    )
    if (isTRUE(length(columns) == 1)) {
        columns <- rep(columns, ncol(x))
    }
    columns <- sprintf(
        "(%s)", paste(columns, collapse = ", ")
    )

    # colnames
    if (!is.null(colnames(x))) {
        cn <- paste(sprintf("[%s]", colnames(x)), collapse = ", ")
        cn <- paste0(cn, collapse = ",\n")
    } else {
        cn <- NULL
    }
    cn <- gsub("\\*", "\\\\*", cn)
    cn <- gsub("\\#", "\\\\#", cn)

    # escape asterisks, which have a special delimiter meaning in Typst
    for (i in seq_along(x)) {
        x[[i]] <- gsub("\\*", "\\\\*", x[[i]])
        x[[i]] <- gsub("\\#", "\\\\#", x[[i]])
    }


    # cells
    tab <- data.frame(apply(x, 1:2, function(z) sprintf("[%s]", z)))
    tab <- do.call(paste, c(tab, sep = ", "))
    tab <- c(cn, tab)
    tab <- paste0("  ", tab, collapse = ",\n")

    # output
    out <- sprintf(
"#table(
inset: %s,
columns: %s,
align: %s,
stroke: none,
%s
)",
    inset, columns, align, tab)


    return(out)
}