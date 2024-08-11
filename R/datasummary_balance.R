#' Balance table: Summary statistics for different subsets of the data (e.g.,
#' control and treatment groups)
#'
#' Creates balance tables with summary statistics for different subsets of the
#' data (e.g., control and treatment groups). It can also be used to create
#' summary tables for full data sets. See the Details and Examples sections
#' below, and the vignettes on the `modelsummary` website: 
#' * https://modelsummary.com/
#' * https://modelsummary.com/articles/datasummary.html
#'
#' @param formula 
#' + `~1`: show summary statistics for the full dataset
#' + one-sided formula: with the "condition" or "column" variable on the right-hand side.
#' + two-side formula: with the subset of variables to summarize on the left-hand side and the condition variable on the right-hand side.
#' @param data A data.frame (or tibble). If this data includes columns called
#'   "blocks", "clusters", and/or "weights", the "estimatr" package will consider
#'   them when calculating the difference in means. If there is a `weights`
#'   column, the reported mean and standard errors will also be weighted.
#' @param dinm TRUE calculates a difference in means with uncertainty
#'   estimates. This option is only available if the `estimatr` package is
#'   installed. If `data` includes columns named "blocks", "clusters", or
#'   "weights", this information will be taken into account automatically by
#'   `estimatr::difference_in_means`.
#' @param dinm_statistic string: "std.error" or "p.value"
#' @inheritParams datasummary
#' @inheritParams modelsummary
#' @template citation
#' @template options
#' @export
#' @section Examples:
#' ```{r, eval = identical(Sys.getenv("pkgdown"), "true")}
#' library(modelsummary)
#' datasummary_balance(~am, mtcars)
#' ```
datasummary_balance <- function(formula,
                                data,
                                output = "default",
                                fmt = fmt_decimal(digits = 1, pdigits = 3),
                                title = NULL,
                                notes = NULL,
                                align = NULL,
                                stars = FALSE,
                                add_columns = NULL,
                                add_rows = NULL,
                                dinm = TRUE,
                                dinm_statistic = "std.error",
                                escape = TRUE,
                                ...) {


    ## settings
    settings_init(settings = list("function_called" = "datasummary_balance"))

    ## sanity checks
    tmp <- sanitize_output(output) # before sanitize_escape
    output_format <- tmp$output_format
    output_factory <- tmp$output_factory
    output_file <- tmp$output_file

    # this is going to be detected by fmt_mathmode() when we call
    # datasummary(output="dataframe") so we can get siunitx formatting even in
    # internal calls.
    settings_set("output_format_ultimate", output_format)

    sanity_align(align)
    sanitize_escape(escape) # after sanitize_output
    sanity_ds_right_handed_formula(formula)
    sanity_stars(stars)
    checkmate::assert_formula(formula)
    checkmate::assert_data_frame(data, min.rows = 1, min.cols = 1)
    checkmate::assert_flag(dinm)
    checkmate::assert_choice(dinm_statistic, choices = c("std.error", "p.value"))
    data <- sanitize_datasummary_balance_data(formula, data)


    if ("p.value" %in% dinm_statistic) {
      insight::check_if_installed("estimatr")
    }

    ## rhs condition variable
    rhs <- labels(stats::terms(formula))

    ## haven labels are not supported. Implementation is complicated because
    ## tables::All() does not accept labels, so we need to strip them from the
    ## dataset used in formulas, but not from the actual `data` argument.
    flag <- any(sapply(data, inherits, "haven_labelled"))
    if (isTRUE(flag)) {
        data <- strip_labels(data)
        warn_once(
            msg = "Labelled data are not supported by the `datasummary_balance()` function.",
            id = "balance haven labels"
        )
    }

    if (formula == ~1) {
        #No groups to calculate mean differences
        dinm <- FALSE
        rhs <- NULL
    } else {
        ## nobs in column spans via factor levels
        lev <- table(data[[rhs]])
        lev <- paste0(names(lev), " (N=", lev, ")")
        levels(data[[rhs]]) <- lev
    }

    ## exclude otherwise All() makes them appear as rows
    idx <- setdiff(colnames(data), c(rhs, "clusters", "blocks", "weights"))
    data_norhs <- data[, idx, drop = FALSE]

    ## 3-parts table: numeric + dinm / factor
    any_numeric <- any(sapply(data_norhs, is.numeric))
    any_factor <- any(sapply(data_norhs, is.factor))
    n_factor <- sum(sapply(data_norhs, is.factor))

    ## difference in means
    if (!any_numeric) {
        dinm <- FALSE
    }

    if (dinm && !isTRUE(check_dependency("estimatr"))) {
        dinm <- FALSE
        warning("Please install the `estimatr` package or set `dinm=FALSE` to suppress this warning.",
                call. = FALSE)
    }

    if (dinm && (length(unique(data[[rhs]])) > 2)) {
        dinm <- FALSE
        warning("The difference in means can only be calculate with two groups in the right-hand side variable. Set `dinm=FALSE` to suppress this warning.",
                call. = FALSE)
    }

    ## factors
    if (any_factor) {

        ## enforce 2-column stub, even when there is only one factor
        tmp1 <- data
        tmp2 <- data_norhs
        tmp1$bad_factor_for_stub <- as.factor(sample(c("A", "B"), nrow(tmp1), replace = TRUE))
        tmp2$bad_factor_for_stub <- as.factor(sample(c("A", "B"), nrow(tmp2), replace = TRUE))

        # pctformat = function(x) sprintf("%.1f", x)
        pctformat <- sanitize_fmt(1)
        nformat <- function(x) {
          sanitize_fmt(0)(as.numeric(x))
        }
        if (!is.null(rhs)) {
            f_fac <- stats::as.formula(sprintf(
                "All(tmp2, factor = TRUE, numeric = FALSE) ~
                 Factor(%s) * (N * Format(nformat()) + Heading('Pct.') * Percent('col') * Format(pctformat()))", rhs))
        } else {
            f_fac <- stats::as.formula(
                "All(tmp2, factor = TRUE, numeric = FALSE) ~
                 (N * Format(nformat()) + Heading('Pct.') * Percent('col') * Format(pctformat()))")
        }
        tab_fac <- datasummary(formula = f_fac,
                               data = tmp1,
                               fmt = fmt,
                               internal_call = TRUE,
                               output = "data.frame")

        ## datasummary(output="dataframe") changes the output format
        sanitize_output(output)
        settings_set("output_format_ultimate", output_format)

        ## enforce 2-column stub, even when there is only one factor
        idx <- grep("bad_factor_for_stub", tab_fac[[1]])
        tab_fac <- tab_fac[1:(idx - 1), , drop = FALSE]
    }

    ## numerics
    if (any_numeric) {
        ## tab_fac has 2 stub columns when there is more than one factor, but only 1 otherwise
        emptyfun <- function(x) return(" ")
        empty <- ifelse(any_factor, "Heading(' ') * emptyfun + ", "")
        # weights
        if ("weights" %in% colnames(data)) {
            #Grouped
            if(!is.null(rhs)) {
              f_num <- "All(data_norhs) ~ %s Factor(%s) * (
                        Heading('Mean') * weighted.mean * Arguments(w = weights, na.rm = TRUE) +
                        Heading('Std. Dev.') * modelsummary:::weighted_sd * Arguments(w = weights))"
              f_num <- stats::as.formula(sprintf(f_num, empty, rhs))
          #No groups
          } else {
              f_num <- "All(data_norhs) ~ %s (
                    Heading('Mean') * weighted.mean * Arguments(w = weights, na.rm = TRUE) +
                    Heading('Std. Dev.') * modelsummary:::weighted_sd * Arguments(w = weights))"
              f_num <- stats::as.formula(sprintf(f_num, empty))
            }
        # no weights
        } else {
            #Grouped
            if(!is.null(rhs)) {
              f_num <- "All(data_norhs) ~ %s Factor(%s) * (Mean + Heading('Std. Dev.') * SD)"
              f_num <- stats::as.formula(sprintf(f_num, empty, rhs))
            #No groups
            } else {
              f_num <- "All(data_norhs) ~ %s (Mean + Heading('Std. Dev.') * SD)"
              f_num <- stats::as.formula(sprintf(f_num, empty))
            }
        }
        tab_num <- datasummary(formula = f_num,
                               fmt = fmt,
                               data = data,
                               internal_call = TRUE,
                               output = "data.frame")

        ## datasummary(output="dataframe") changes the output format
        sanitize_output(output)
        settings_set("output_format_ultimate", output_format)
    }

    ## combine
    if (any_numeric && any_factor) {
        top <- tab_num
        mid <- attr(get_span_kableExtra(tab_fac), "column_names")
        if (is.null(mid)) {
            mid <- colnames(tab_fac)
        } else {
            mid <- trimws(mid)
        }
        mid <- stats::setNames(as.data.frame(as.list(mid)), colnames(top))
        bot <- stats::setNames(tab_fac, colnames(top))
        tab <- bind_rows(top, mid, bot)

        ## restore attributes destroyed by bind_rows
        idx <- grep("header|stub|align", names(attributes(tab_num)), value = TRUE)
        for (i in idx) {
            attr(tab, i) <- attr(tab_num, i)
        }
        ## empty numeric column looks real but is actually a stub
        attr(tab, "stub_width") <- attr(tab_fac, "stub_width")

    } else if (any_numeric) {
        tab <- tab_num
    } else if (any_factor) {
        tab <- tab_fac
    } else {
        stop("The `datasummary_balance` function was unable to extract summary statistics.")
    }

    ## differences in means for numeric variables
    if (any_numeric && isTRUE(dinm)) {
        ## dinm
        numeric_variables <- colnames(data_norhs)[sapply(data_norhs, is.numeric)]
        tmp <- lapply(numeric_variables,
                      function(lhs) DinM(lhs = lhs,
                                         rhs = rhs,
                                         data = data,
                                         fmt = fmt,
                                         statistic = dinm_statistic,
                                         stars = stars,
                                         escape = escape))
        tmp <- do.call("rbind", tmp)

        ## use poorman's left_join because merge breaks the order, even with sort=FALSE
        ## this also protects attributes
        tab <- left_join(tab, tmp, by = " ")

        tab[is.na(tab)] <- ""
    }

    ## horizontal rule
    if (any_factor && any_numeric) {
        hrule <- nrow(tab_num) + 1
    } else {
        hrule <- NULL
    }

    ## align: default (TODO: `add_columns` support)
    if (is.null(align) && !is.null(attr(tab, "stub_width")) && is.null(add_columns)) {
        align <- paste0(strrep("l", attr(tab, "stub_width")),
                        strrep("r", ncol(tab) - attr(tab, "stub_width")))
    }

    ## weights warning
    if (isTRUE(any_factor) && "weights" %in% colnames(data)) {
      msg <- 'When the `data` used in `datasummary_balance` contains a "weights" column, the means, standard deviations, difference in means, and standard errors of numeric variables are adjusted to account for weights. However, the counts and percentages for categorical variables are not adjusted.'
      warning(msg, call. = FALSE)
    }

    ## make table
    out <- factory(
        tab,
        align = align,
        hrule = hrule,
        notes = notes,
        fmt = fmt,
        output = output,
        add_rows = add_rows,
        add_columns = add_columns,
        title = title,
        escape = escape,
        output_factory = output_factory,
        output_format = output_format,
        output_file = output_file,
        ...)

    # invisible return
    if (!is.null(output_file) ||
        output == "jupyter" ||
        (output == "default" && settings_equal("output_default", "jupyter"))) {
      settings_rm()
      return(invisible(out))
    # visible return
    } else {
      settings_rm()
      return(out)
    }

}


#' Difference in means using `estimatr`
#'
#' @noRd
DinM <- function(lhs, rhs, data, fmt, statistic, stars = TRUE, escape = TRUE) {

  insight::check_if_installed("estimatr")

  if (!"clusters" %in% colnames(data)) {
      clusters <- NULL
  }
  if (!"weights" %in% colnames(data)) {
      weights <- NULL
  }
  if (!"blocks" %in% colnames(data)) {
      blocks <- NULL
  }

  # needed for names with spaces
  data[["condition_variable_placeholder"]] <- data[[rhs]]
  data[["outcome_variable_placeholder"]] <- data[[lhs]]

  out <- estimatr::difference_in_means(
    outcome_variable_placeholder ~ condition_variable_placeholder,
    data = data, blocks = blocks, clusters = clusters, weights = weights)

  out <- estimatr::tidy(out)

  rounding <- sanitize_fmt(fmt)

  out[["estimate"]] <- rounding(out[["estimate"]])

  if (!isFALSE(stars) && "p.value" %in% colnames(out)) {
    out$estimate <- paste0(
      out$estimate,
      make_stars(out$p.value, stars))
  }

  if (identical(statistic, "p.value")) {
    out[[statistic]] <- rounding(out[[statistic]], pval = TRUE)
  } else {
    out[[statistic]] <- rounding(out[[statistic]])
  }

  out <- out[, c("estimate", statistic), drop = FALSE]

  out$variable <- lhs

  if (statistic == "std.error") {
    colnames(out) <- c("Diff. in Means", "Std. Error", " ")
  } else if (statistic == "p.value") {
    colnames(out) <- c("Diff. in Means", "p", " ")
  } else {
    colnames(out) <- c("Diff. in Means", statistic, " ")
  }

  out

}


#' internal function for sanity checks
#'
#' @noRd
#' @keywords internal
sanitize_datasummary_balance_data <- function(formula, data) {

  # tables::tabular does not play well with tibbles
  data <- as.data.frame(data)

  # tables::All() does not play well with labelled data (hack formula which
  # includes All())
  sanity_ds_data(All(data) ~ x + y, data)

  # rhs condition variable
  rhs <- labels(stats::terms(formula))

  # LHS selects columns
  lhs <- stats::update(formula, ".~1")
  lhs <- all.vars(lhs)
  lhs <- setdiff(lhs, ".")
  if (length(lhs) > 0) {
    cols <- intersect(c(lhs, rhs), colnames(data))
    if (length(cols) > 1) {
        data <- data[, cols, drop = FALSE]
    }
  }
  
  if (formula != ~1) {


      if (!rhs %in% colnames(data)) {
        stop("Variable ", rhs, " must be in data.")
      }

      if (length(unique(data[[rhs]])) > 10) {
        stop(sprintf("Each value of the `%s` variable will create two separate columns. This variable has more than 10 unique values, so the table would be too wide to be readable.",
            rhs))
      }
      data <- data[!is.na(data[[rhs]]), , drop = FALSE]
  } else {
      #No grouping variable - summarise full dataset
      rhs <- NULL
  }

  if ("weights" %in% colnames(data)) {
    if (anyNA(data[["weights"]])) {
      stop("The `weights` column cannot include missing data.")
    }
  }

  # sanity checks on other variables
  drop_too_many_levels <- NULL
  drop_entirely_na <- NULL

  for (n in colnames(data)) {
    # categorical data must be factor
    if (is.character(data[[n]]) || is.logical(data[[n]])) {
      data[[n]] <- factor(data[[n]], exclude = NULL)
    }

    if (is.null(rhs) || n != rhs) {
      # completely missing
      if (all(is.na(data[[n]]))) {
        data[[n]] <- NULL
        drop_entirely_na <- c(drop_entirely_na, n)
      } else {

        # factors with too many levels
        if (is.factor(data[[n]])) {
          if (length(levels(data[[n]])) > 50) {
            data[[n]] <- NULL
            drop_too_many_levels <- c(drop_too_many_levels, n)
          }
        }
      }
    }
  }

  if (!is.null(drop_too_many_levels)) {
    warning(sprintf("These variables were omitted because they include more than 50 levels: %s.", paste(drop_too_many_levels, collapse = ", ")),
            call. = FALSE)
  }

  if (!is.null(drop_entirely_na)) {
    warning(sprintf("These variables were omitted because they are entirely missing: %s.", paste(drop_entirely_na, collapse = ", ")),
            call. = FALSE)
  }

  return(data)
}
