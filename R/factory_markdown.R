align_str_left <- function(x, pad_n = NULL) {
    if (is.null(pad_n)) pad_n <- max(nchar(x))
    right <- strrep(" ", pad_n - nchar(x))
    paste0(x, right)
}
align_str_right <- function(x, pad_n = NULL) {
    if (is.null(pad_n)) pad_n <- max(nchar(x))
    left <- strrep(" ", pad_n - nchar(x))
    paste0(left, x)
}
align_str_center <- function(x, pad_n = NULL) {
    if (is.null(pad_n)) pad_n <- max(nchar(x))
    left <- strrep(" ", ceiling((pad_n - nchar(x)) / 2))
    right <- strrep(" ", floor((pad_n - nchar(x)) / 2))
    paste0(left, x, right)
}
print.modelsummary_markdown <- function(x, ...) {
    cat("\n\n")
    cat(x, sep = "\n")
}

factory_markdown <- function(tab,
                             align = NULL,
                             hrule = NULL,
                             hgroup = NULL,
                             hindent = FALSE,
                             notes = NULL,
                             title = NULL,
                             escape = TRUE,
                              ...) {


    # fake spans
    colnames(tab) <- gsub("\\|{4}", " / ", colnames(tab))

    # align content
    for (i in seq_along(tab)) {
        pad_n <- max(nchar(colnames(tab)[i]), max(nchar(tab[[i]])))
        if (align[[i]] == "l") {
            tab[[i]] <- align_str_left(tab[[i]], pad_n = pad_n)
        } else if (align[[i]] == "r") {
            tab[[i]] <- align_str_right(tab[[i]], pad_n = pad_n)
        } else if (align[[i]] == "c") {
            tab[[i]] <- align_str_center(tab[[i]], pad_n = pad_n)
        }
    }

    # bind centered column names
    header <- as.data.frame(as.list(colnames(tab)))
    colnames(header) <- colnames(tab)
    for (i in seq_along(tab)) {
        header[[i]] <- align_str_center(header[[i]], nchar(tab[[i]][1]))
    }
    tab <- rbind(header, tab)

    # pipes
    tab[[1]] <- paste("|", tab[[1]])
    for (i in seq_along(tab)) {
        tab[[i]] <- paste(tab[[i]], "| ")
    }
    tab <- do.call(paste0, tab)

    # ruler
    ruler <- gsub("[^\\|]", " ", tab[1])
    ruler <- gsub(" ", "-", ruler)
    ruler <- gsub("-\\|", ":|", ruler) # all except first
    ruler <- sub(":\\|", "-|", ruler) # all except first
    ruler <- sub("\\|-", "|:", ruler) # only first
    ruler <- gsub("-$", "", ruler) # only first

    # group ruleers
    for (i in rev(seq_along(hgroup))) {
        tab <- append(tab, ruler, after = hgroup[[i]][2] + 1)
    }

    # horizontal rulers: knitr and pandoc may not support headers, or I don't know how
    # hrule <- rev(sort(c(1, hrule)))
    hrule <- 1
    for (h in hrule) {
        tab <- append(tab, ruler, after = h)
    }

    # title
    if (!is.null(title)) {
        tab <- c(paste("     Table:", title), "", tab)
    }

    # notes
    if (!is.null(notes)) {
        for (n in notes) {
            tab <- c(tab, "", "__Note:__", paste("^^", n))
        }
    }

    # output
    class(tab) <- c("modelsummary_markdown", "knitr_kable")
    attr(tab, "format") <- "pipe"

    # output
    if (is.null(settings_get("output_file"))) {
        return(tab)
    } else {
        writeLines(paste(tab, collapse = "\n"), con = settings_get("output_file"))
    }
}


 # 'knitr_kable' chr [1:24] "Table: Blah blah" "" "|             |   (1)    |" "|:------------|:--------:|" "|(Intercept)  |  26.664  |" "|             |  (0.972) |" "|factor(cyl)6 |  -6.921  |" ...
 # - attr(*, "format")= chr "pipe"


#     Table: Blah blah
#
# |             |   (1)    |
# |:------------|:--------:|
# |(Intercept)  |  26.664  |
# |             |  (0.972) |
# |factor(cyl)6 |  -6.921  |
# |             |  (1.558) |
# |factor(cyl)8 | -11.564  |
# |             |  (1.299) |
# |Num.Obs.     |  32      |
# |R2           |   0.732  |
# |R2 Adj.      |   0.714  |
# |AIC          | 170.6    |
# |BIC          | 176.4    |
# |Log.Lik.     | -81.282  |
# |F            |  39.698  |
# |RMSE         |   3.07   |
#
# __Note:__
# ^^ a
#
# __Note:__
# ^^ b