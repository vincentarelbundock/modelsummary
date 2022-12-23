#' panelsummary
#' 
#' @inheritParams modelsummary
#' @param panels list of lists of lists of lists
#' @export
panelsummary <- function(
    panels, 
    output      = "default",
    fmt         = 3,
    estimate    = "estimate",
    statistic   = "std.error",
    vcov        = NULL,
    conf_level  = 0.95,
    exponentiate = FALSE,
    stars       = FALSE,
    coef_map    = NULL,
    coef_omit   = NULL,
    coef_rename = FALSE,
    gof_map     = NULL,
    gof_omit    = NULL,
    add_columns = NULL,
    add_rows    = NULL,
    align       = NULL,
    notes       = NULL,
    title       = NULL,
    escape      = TRUE,
    ...) {

    settings_init(settings = list("function_called" = "panelsummary"))

    dots <- list(...)

    # sanity
    checkmate::assert_list(panels, min.len = 2)
    if (any(c("shape", "group_map") %in% names(dots))) {
        msg <- "The `shape` and `group_map` arguments are not supported by `panelsummary()`."
        insight::format_error(msg)
    }

    # panel names
    panel_names <- names(panels)
    if (is.null(panel_names)) {
        panel_names <- paste("Panel", seq_along(panels))
    }

    # panel lists to tables
    panels_list <- list()
    for (i in seq_along(panels)) {
        # modelsummary(output="dataframe") changes the output format
        # reset for every call
        settings_init(settings = list("function_called" = "panelsummary"))
        sanitize_output(output)
        args <- modifyList(
            dots,
            list(
                models = panels[[i]],
                output = "dataframe",
                fmt = fmt,
                estimate = estimate,
                statistic = statistic,
                vcov = vcov,
                conf_level = conf_level,
                exponentiate = exponentiate,
                stars = stars,
                coef_map = coef_map,
                coef_omit = coef_omit,
                coef_rename = coef_rename,
                gof_map = gof_map,
                gof_omit = gof_omit,
                escape = FALSE
        ))
        tab <- do.call("modelsummary", args)
        panels_list[[i]] <- tab
    }


    # identical GOF rows should be combined and reported at the bottom
    # do not combine GOF if the model names are different in the different panels
    flag <- TRUE
    for (i in 2:length(panels)) {
        if (!identical(names(panels[[i - 1]]), names(panels[[i]]))) {
            flag <- FALSE
        }
    }
    if (flag) {
        est <- lapply(panels_list, subset, part != "gof")
        gof <- lapply(panels_list, subset, part == "gof")
        gof_same <- lapply(gof, data.table::as.data.table)
        gof_same <- tryCatch(
            Reduce(data.table::fintersect, gof_same),
            error = function(e) NULL)
        if (!is.null(gof_same)) {
            for (i in seq_along(gof)) {
                gof[[i]] <- gof[[i]][!gof[[i]]$term %in% gof_same$term, , drop = FALSE]
                panels_list[[i]] <- rbind(est[[i]], gof[[i]])
            }
        }
        panels_list <- c(panels_list, list(gof_same))
    } else {
        gof_same <- NULL
    }

    panels_nrow <- sapply(panels_list, nrow)

    hrule <- head(cumsum(panels_nrow) + 1, -1)

    tab <- data.table::rbindlist(panels_list, fill = TRUE)

    tab$part <- tab$statistic <- NULL

    colnames(tab)[1] <- " "
    tab[is.na(tab)] <- ""

    # group rows by panel: kableExtra
    if (isTRUE(nrow(gof_same) > 0)) {
        panel_names <- c(panel_names, "Combined GOF")
    }

    end <- cumsum(panels_nrow)
    sta <- c(0, head(end, -1)) + 1
    hgroup <- list()
    for (i in seq_along(panel_names)) {
        hgroup[[panel_names[i]]] <- c(sta[i], end[i])
    }

    args <- modifyList(
        dots,
        list(
            data = tab,
            hrule = hrule,
            hgroup = hgroup,
            output = output,
            add_columns = add_columns,
            add_rows = add_rows,
            align = align,
            notes = notes,
            title = title,
            escape = escape,
            ...
    ))
    out <- do.call(datasummary_df, args)

    # invisible return
    if (!is.null(settings_get("output_file")) ||
        isTRUE(output == "jupyter") ||
        (output == "default" && settings_equal("output_default", "jupyter"))) {
        settings_rm()
        return(invisible(out))
    # visible return
    } else {
        settings_rm()
        return(out)
    }

}
