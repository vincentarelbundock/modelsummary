modelsummary_settings <- new.env()

settings_cache <- function(setti) {
    out <- list()
    for (s in setti) {
        out[[s]] <- settings_get(s)
    }
    return(out)
}

settings_restore <- function(cache) {
    for (n in names(cache)) {
        settings_set(n, cache[[n]])
    }
}
        
        

settings_init <- function(settings = NULL) {
    settings_rm()

    default_settings <- list(
        "format_numeric_latex" = getOption("modelsummary_format_numeric_latex", default = "siunitx"),
        "modelsummary_format_numeric_html" = getOption("modelsummary_format_numeric_html", default = "minus"),
        "siunitx_scolumns" = FALSE,
        "output_default" = getOption("modelsummary_factory_default", default = "kableExtra"),
        "stars_note" = getOption("modelsummary_stars_note", default = TRUE))


    # in hebrew or chinese locales, the html minus signs does not appear and it underlines the whole number.
    # https://github.com/vincentarelbundock/modelsummary/issues/552
    regex_locale <- c("be", "ca", "de", "en", "et", "eu", "fi", "fr", "hu", "it", "nl")
    regex_locale <- paste(sprintf("^%s", regex_locale), collapse = "|")
    regex_locale <- paste0("(?i)", regex_locale, "|^C$|^C\\.|^POSIX")
    settings_set("known_locale", isTRUE(grepl(regex_locale, Sys.getlocale(category = "LC_CTYPE"))))

    checkmate::assert_list(settings, null.ok = TRUE, names = "unique")

    if (!is.null(settings)) {
        settings <- c(settings, default_settings)
    }

    for (i in seq_along(settings)) {
        settings_set(names(settings)[i], settings[[i]])
    }
}

settings_get <- function(name) {
    if (name %in% names(modelsummary_settings)) {
        get(name, envir = modelsummary_settings)
    } else {
        NULL
    }
}

settings_set <- function(name, value) {
    assign(name, value = value, envir = modelsummary_settings)
}

settings_rm <- function(name = NULL) {
    if (is.null(name)) {
        rm(list = names(modelsummary_settings), envir = modelsummary_settings)
    } else {
        rm(list = name, envir = modelsummary_settings)
    }
}

settings_equal <- function(name, comparison) {
    k <- settings_get(name)
    if (!is.null(k) && length(comparison) == 1 && k == comparison) {
        out <- TRUE
    } else if (!is.null(k) && length(comparison) > 1 && k %in% comparison) {
        out <- TRUE
    } else {
        out <- FALSE
    }
    return(out)
}
