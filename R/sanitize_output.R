get_factory_name <- function(x, default) {
  out <- getOption(paste0("modelsummary_factory_", x))
  if (is.null(out)) {
    out <- config_get(paste0("factory_", x))
  }
  if (is.null(out)) {
    out <- default
  }
  return(out)
}


#' parse an `output` argument to determine which table factory and which output
#' format to use.
#'
#' @noRd
sanitize_output <- function(output) {

  # useful in modelsummary_rbind()
  if (is.null(output)) {
    return(NULL)
  }

  flag <- checkmate::check_string(output)
  fun <- settings_get("function_called")
  if (!isTRUE(flag) && !is.null(fun) && fun == "modelsummary") {
    stop("The `output` argument must be a string. Type `?modelsummary` for details. This error is sometimes raised when users supply multiple models to `modelsummary` but forget to wrap them in a list. This works: `modelsummary(list(model1, model2))`. This does *not* work: `modelsummary(model1, model2)`")
  }

  object_types <- c('default', 'gt', 'kableExtra', 'flextable', 'huxtable', 'DT', "tinytable",
                    'html', 'jupyter', 'latex', 'latex_tabular', 'markdown',
                    'dataframe', 'data.frame', 'typst', 'modelsummary_list')
  extension_types <- c('html', 'tex', 'md', 'txt', 'docx', 'pptx', 'rtf',
                       'jpg', 'png', 'csv', 'xlsx')

  checkmate::assert_string(output)

  cond1 <- output %in% object_types
  if (isFALSE(cond1)) {
    extension <- tools::file_ext(output)
    cond2 <- extension %in% extension_types
    if (isTRUE(cond2)) {
      checkmate::assert_path_for_output(output, overwrite = TRUE)
    } else {
      msg <- paste0('The `output` argument must be ',
        paste(object_types, collapse = ', '),
        ', or a valid file path with one of these extensions: ',
        paste(extension_types, collapse = ', '))
      stop(msg)
    }
  }

  extension_dict <- c(
    "csv"  = "dataframe",
    "xlsx" = "dataframe",
    "md"   = "markdown",
    "Rmd"  = "markdown",
    "txt"  = "markdown",
    "tex"  = "latex",
    "ltx"  = "latex",
    "docx" = "word",
    "doc"  = "word",
    "pptx" = "powerpoint",
    "ppt"  = "powerpoint",
    "png"  = "png",
    "jpg"  = "jpg",
    "rtf"  = "rtf",
    "htm"  = "html",
    "html" = "html")

  factory_dict <- c(
    "dataframe" = "dataframe",
    "data.frame" = "dataframe",
    "flextable" = "flextable",
    "tinytable" = "tinytable",
    "gt" = "gt",
    "huxtable" = "huxtable",
    "DT" = "DT",
    "kableExtra" = "kableExtra",
    "latex_tabular" = "kableExtra",
    "modelsummary_list" = "modelsummary_list",
    "typst" = "typst",
    "markdown" = get_factory_name("markdown", default = "modelsummary"),
    "jupyter" = get_factory_name("html", default = "kableExtra"),
    "latex" = get_factory_name("latex", default = "kableExtra"),
    "html" = get_factory_name("html", default = "kableExtra"),
    "jpg" = get_factory_name("jpg", default = "kableExtra"),
    "png" = get_factory_name("png", default = "kableExtra"),
    "rtf" = get_factory_name("rtf", default = "gt"),
    "word" = get_factory_name("word", default = "flextable"),
    "powerpoint" = get_factory_name("powerpoint", default = "flextable"))

  ## sanity check: are user-supplied global options ok?
  sanity_factory(factory_dict)

  ## save user input to check later
  output_user <- output

  # defaults
  if (output == "default") {
      output <- getOption("modelsummary_factory_default", default = NULL)
      if (is.null(output)) {
        output <- config_get("factory_default")
      }
      if (is.null(output)) {
        if (isTRUE(insight::check_if_installed("kableExtra", quietly = TRUE))) {
          output <- "kableExtra"
        } else if (isTRUE(insight::check_if_installed("gt", quietly = TRUE))) {
          output <- "gt"
        } else if (isTRUE(insight::check_if_installed("tinytable", quietly = TRUE))) {
          output <- "tinytable"
        } else if (isTRUE(insight::check_if_installed("flextable", quietly = TRUE))) {
          output <- "flextable"
        } else if (isTRUE(insight::check_if_installed("huxtable", quietly = TRUE))) {
          output <- "huxtable"
        } else if (isTRUE(insight::check_if_installed("DT", quietly = TRUE))) {
          output <- "DT"
        } else {
          output <- "markdown"
        }
      }
  } else if (output == "jupyter") {
      output <- "html"
  }

  # rename otherwise an extension is wrongly detected
  if (output == "data.frame") {
    output <- "dataframe"
  }

  # file extension for auto-detect
  ext <- tools::file_ext(output)

  # don't write to file if the extension is unknown or missing
  output_file <- NULL
  output_format <- output
  if (ext %in% names(extension_dict)) {
    output_format <- extension_dict[ext]
    output_file <- output
  }

  if (isTRUE(check_dependency("knitr")) && isTRUE(check_dependency("rmarkdown"))) {

    ## various strategies to guess the knitr output format
    fmt <- c(
      hush(knitr::pandoc_to()),
      hush(names(rmarkdown::metadata[["format"]])),
      hush(rmarkdown::default_output_format(knitr::current_input())$name))

    word_fmt <- c(
      "docx",
      "word",
      "word_document",
      "rdocx_document",
      "officedown::rdocx_document",
      "word_document2",
      "bookdown::word_document2")

    markdown_fmt <- c(
      "md",
      "gfm",
      "markdown",
      "markdown_strict",
      "commonmark",
      "github_document",
      "reprex_render",
      "reprex::reprex_render")

    if (any(word_fmt %in% fmt) && output_user %in% c("flextable", "default")) {
      output_format <- "word"

    # unfortunately, `rmarkdown::default_output_format` only detects
    # `html_document` on reprex, so this will only work in `github_document`
    ## reprex and github: change to markdown output format only if `output` is "default"
    } else if (any(markdown_fmt %in% fmt) &&
               output_user == "default"  &&
               # respect global options, even in qmd->md documents
               is.null(getOption("modelsummary_factory_default", default = NULL))) {
      output_format <- "markdown"


    } else if (isTRUE(knitr::pandoc_to() == "typst")) {
      output_format <- "typst"
    }

  }

  # choose factory based on output_format
  output_factory <- factory_dict[[output_format]]

  # kableExtra must specify output_format ex ante (but after factory choice)
  if (output_factory == 'kableExtra' && output_format %in% c('default', 'kableExtra')) {
    if (isTRUE(check_dependency("knitr"))) {
      if (knitr::is_latex_output()) {
        output_format <- "latex"
      }
    }
  }

  ## warning siunitx & booktabs in preamble
  if (settings_equal("format_numeric_latex", "siunitx") && (output %in% c("latex", "latex_tabular") || tools::file_ext(output) == "tex")) {
      msg <- 'To compile a LaTeX document with this table, the following commands must be placed in the document preamble:

\\usepackage{booktabs}
\\usepackage{siunitx}
\\newcolumntype{d}{S[
    input-open-uncertainty=,
    input-close-uncertainty=,
    parse-numbers = false,
    table-align-text-pre=false,
    table-align-text-post=false
 ]}

To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")
'
      warn_once(msg, "latex_siunitx_preamble")
  }

  # settings environment
  settings_set("output_factory", unname(output_factory))
  settings_set("output_format", unname(output_format))
  settings_set("output_file", unname(output_file))

}
