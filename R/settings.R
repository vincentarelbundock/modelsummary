modelsummary_settings <- new.env()

settings_get <- function(name) {
    if (name %in% names(modelsummary_settings)) {
        get(name, envir = modelsummary_settings)
    } else {
        NULL
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
