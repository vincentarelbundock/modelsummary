#' Model summary tables, stacked in panels
#'
#' Display the results from several statistical models in "panels": side-by-side
#' and stacked on top of each other. This function supports dozens of statistical
#' models, and it can produce tables in HTML, LaTeX, Word, Markdown, PDF,
#' PowerPoint, Excel, RTF, JPG, or PNG. The appearance of the tables can be
#' customized extensively by specifying the `output` argument, and by using
#' functions from one of the supported table customization packages:
#' `tinytable`, `kableExtra`, `gt`, `flextable`, `huxtable`, `DT`. For more information, see
#' the Details and Examples sections below, and the vignettes on the
#' `modelsummary` website:
#' https://modelsummary.com/
#' * [The `modelsummary` Vignette includes dozens of examples of tables with extensive customizations.](https://modelsummary.com/vignettes/modelsummary.html)
#' * [The Appearance Vignette shows how to modify the look of tables.](https://modelsummary.com/vignettes/appearance.html)
#'
#' @param panels a nested list of models
#' * Unnamed nested list with 2 panels: `list(list(model1, model2), list(model3, model4))`
#' * Named nested list with 2 panels: `list("Panel A" = list(model1, model2), "Panel B" = list(model3, model4))`
#' @inheritParams modelsummary
#' @keywords internal
#' @noRd
modelsummary_rbind <- function(
  panels,
  output = "default",
  fmt = 3,
  estimate = "estimate",
  statistic = "std.error",
  vcov = NULL,
  conf_level = 0.95,
  exponentiate = FALSE,
  stars = FALSE,
  coef_map = NULL,
  coef_omit = NULL,
  coef_rename = FALSE,
  gof_map = NULL,
  gof_omit = NULL,
  gof_custom = NULL,
  add_columns = NULL,
  add_rows = NULL,
  align = NULL,
  shape = NULL,
  group_map = NULL,
  notes = NULL,
  title = NULL,
  escape = TRUE,
  ...
) {
  settings_init(settings = list("function_called" = "modelsummary_rbind"))

  dots <- list(...)

  # sanity
  sanity_stars(stars)
  checkmate::assert_list(panels, min.len = 2)

  number_of_panels <- length(panels)

  for (i in seq_along(panels)) {
    panels[[i]] <- sanitize_models(panels[[i]], ...)
  }

  # need the settings for later -- before escape_string
  tmp <- sanitize_output(output) # early
  output_format <- tmp$output_format
  output_factory <- tmp$output_factory
  output_file <- tmp$output_file
  sanitize_escape(escape)

  # panel names
  # model names dictionary: use unique names for manipulation
  if (is.null(names(panels))) {
    panel_names <- NULL
  } else {
    panel_names <- names(panels)
  }
  if (!is.null(panel_names)) {
    panel_names <- pad(panel_names, output_format = output_format)
  }

  # If there are no common model names but all the panels have the same number
  # of models, we make assumptions.
  len <- sapply(panels, length)
  if (isTRUE(length(unique(len)) == 1)) {
    int <- intersect(names(panels[[1]]), names(panels[[2]]))
    for (i in seq_along(panels)) {
      flag1 <- isTRUE(length(int) == 0)
      flag2 <- length(names(panels[[i]])) != length(names(panels[[i]]))
      if (flag1 || flag2) {
        names(panels[[i]]) <- sprintf("(%s)", seq_along(panels[[i]]))
      }
    }
  }

  # panel lists to tables
  panels_list <- list()
  for (i in seq_along(panels)) {
    # modelsummary(output="dataframe") changes the output format
    # reset for every call
    args <- utils::modifyList(
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
        shape = term + statistic ~ model,
        group_map = NULL,
        gof_map = gof_map,
        gof_omit = gof_omit,
        gof_custom = gof_custom,
        escape = escape
      ),
      keep.null = TRUE
    )
    tab <- do.call("modelsummary", args)
    panels_list[[i]] <- tab
  }

  # modelsummary(output="dataframe") re-inits this
  sanitize_output(output)

  # identical GOF rows should be combined and reported at the bottom
  # do not combine GOF if the model names are different in the different panels
  flag <- isTRUE(shape == "rcollapse")
  # panels are not all the same length
  if (length(unique(sapply(panels, length))) > 1) {
    for (i in 2:length(panels)) {
      if (!identical(names(panels[[i - 1]]), names(panels[[i]]))) {
        flag <- FALSE
      }
    }
  }

  if (flag) {
    # fixed effects should not be collapsed unless they are exactly identical across panels
    fe_collapse <- TRUE
    fe <- lapply(panels_list, subset, grepl("^FE: ", term))
    fe_n <- sapply(fe, nrow)
    if (fe_n[1] == 0 || length(unique(fe_n)) > 1) {
      fe_collapse <- FALSE
    } else {
      fe <- lapply(fe, function(x) x[order(x$term), ])
      for (i in 2:length(fe)) {
        if (any(fe[[i]] != fe[[i - 1]])) {
          fe_collapse <- FALSE
        }
      }
    }

    if (fe_collapse) {
      est <- lapply(panels_list, subset, part != "gof")
      gof <- lapply(panels_list, subset, part == "gof")
    } else {
      est <- lapply(panels_list, subset, part != "gof" | grepl("^FE: ", term))
      gof <- lapply(panels_list, subset, part == "gof" & !grepl("^FE: ", term))
    }

    gof_same <- lapply(gof, data.table::as.data.table)
    gof_same <- tryCatch(
      Reduce(data.table::fintersect, gof_same),
      error = function(e) NULL
    )

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

  panels_list <- Filter(function(x) !is.null(x), panels_list)

  panels_nrow <- sapply(panels_list, nrow)

  # only one hrule after the last data, before gof_same
  if (is.null(gof_same) || isTRUE(nrow(gof_same) == 0)) {
    hrule <- NULL
  } else {
    hrule <- sum(utils::head(panels_nrow, -1)) + 1
  }

  tab <- data.table::rbindlist(panels_list, fill = TRUE)

  tab$part <- tab$statistic <- NULL

  colnames(tab)[1] <- " "
  tab[is.na(tab)] <- ""

  # pad
  colnames(tab) <- pad(colnames(tab), output_format = output_format)

  # group rows by panel: kableExtra
  if (isTRUE(nrow(gof_same) > 0)) {
    panel_names <- c(panel_names, "Combined GOF")
  }

  end <- cumsum(panels_nrow)
  sta <- c(0, utils::head(end, -1)) + 1

  # Issue #626: add rows moves hgroup
  pos <- rev(sort(attr(add_rows, "position")))
  if (!is.null(pos)) {
    sta <- sta + sapply(sta, function(x) sum(pos <= x))
    end <- end + sapply(end, function(x) sum(pos <= x))
  }

  hgroup <- list()
  for (i in seq_along(panel_names)) {
    hgroup[[panel_names[i]]] <- c(sta[i], end[i])
  }

  # indent
  if (isTRUE(nrow(gof_same) > 0)) {
    hindent <- utils::tail(hgroup, 1)
    hgroup <- utils::head(hgroup, -1)
  } else {
    hindent <- NULL
  }

  # stars
  if (!isFALSE(stars) && !any(grepl("\\{stars\\}", c(estimate, statistic)))) {
    stars_note <- make_stars_note(
      stars,
      output_format = output_format,
      output_factory = output_factory
    )
    if (is.null(notes)) {
      notes <- stars_note
    } else {
      notes <- c(stars_note, notes)
    }
  }

  # align
  if (is.null(align)) {
    n_stub <- sum(grepl("^ *$", colnames(tab)))
    align <- paste0(strrep("l", n_stub), strrep("c", ncol(tab) - n_stub))
    if (isTRUE(checkmate::check_data_frame(add_columns))) {
      align <- paste0(align, strrep("c", ncol(add_columns)))
    }
  }

  ## build table
  out <- factory(
    tab,
    align = align,
    fmt = fmt,
    hrule = hrule,
    hgroup = hgroup,
    hindent = hindent,
    notes = notes,
    output = output,
    title = title,
    add_rows = add_rows,
    add_columns = add_columns,
    escape = escape,
    output_factory = output_factory,
    output_format = output_format,
    output_file = output_file,
    ...
  )

  # invisible return
  if (
    !is.null(output_file) ||
      isTRUE(output == "jupyter") ||
      (output == "default" && settings_equal("output_default", "jupyter"))
  ) {
    settings_rm()
    return(invisible(out))
    # visible return
  } else {
    settings_rm()
    return(out)
  }
}
