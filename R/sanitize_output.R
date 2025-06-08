latex_compilation_tips <- 'To compile a LaTeX document with this table, the following commands must be placed in the document preamble:

\\usepackage{tabularray}
\\usepackage{float}
\\usepackage{graphicx}
\\usepackage{codehigh}
\\usepackage[normalem]{ulem}
\\UseTblrLibrary{booktabs}
\\UseTblrLibrary{siunitx}
\\newcommand{\\tinytableTabularrayUnderline}[1]{\\underline{#1}}
\\newcommand{\\tinytableTabularrayStrikeout}[1]{\\sout{#1}}
\\NewTableCommand{\\tinytableDefineColor}[3]{\\definecolor{#1}{#2}{#3}}

To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")
'


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
  extension_dict <- c(
    "csv" = "dataframe",
    "xlsx" = "dataframe",
    "md" = "markdown",
    "Rmd" = "markdown",
    "txt" = "markdown",
    "tex" = "latex",
    "ltx" = "latex",
    "docx" = "word",
    "doc" = "word",
    "pptx" = "powerpoint",
    "ppt" = "powerpoint",
    "png" = "png",
    "jpg" = "jpg",
    "rtf" = "rtf",
    "htm" = "html",
    "html" = "html"
  )

  factory_dict <- c(
    "dataframe" = "dataframe",
    "data.frame" = "dataframe",
    "flextable" = "flextable",
    "tinytable" = "tinytable",
    "gt" = "gt",
    "huxtable" = "huxtable",
    "DT" = "DT",
    "kableExtra" = "kableExtra",
    "modelsummary_list" = "modelsummary_list",
    "typst" = "tinytable",
    "markdown" = get_factory_name("markdown", default = "tinytable"),
    "jupyter" = get_factory_name("html", default = "tinytable"),
    "latex" = get_factory_name("latex", default = "tinytable"),
    "latex_tabular" = get_factory_name("latex_tabular", default = "tinytable"),
    "html" = get_factory_name("html", default = "tinytable"),
    "png" = get_factory_name("png", default = "tinytable"),
    "word" = get_factory_name("word", default = "tinytable"),
    "jpg" = get_factory_name("jpg", default = "kableExtra"),
    "rtf" = get_factory_name("rtf", default = "gt"),
    "powerpoint" = get_factory_name("powerpoint", default = "flextable")
  )
  sanity_factory(factory_dict)

  fmt_dict <- list(
    typst = c("typst"),
    latex = c("latex", "pdf"),
    word = c("docx", "word"),
    markdown = c("md", "gfm", "markdown", "markdown_strict", "commonmark")
  )

  # useful in modelsummary_rbind()
  if (is.null(output)) return(NULL)

  if (
    !isTRUE(checkmate::check_string(output)) &&
      settings_equal("function_called", "modelsummary")
  ) {
    msg <- "The `output` argument must be a string. This error is sometimes raised when users supply multiple models to `modelsummary` but forget to wrap them in a list. This works: `modelsummary(list(model1, model2))`. This does *not* work: `modelsummary(model1, model2)`"
    insight::format_error(msg)
  }
  checkmate::assert_string(output)

  # rename otherwise an extension is wrongly detected
  if (isTRUE(output == "data.frame")) output <- "dataframe"

  if (output == "jupyter") output <- "html"

  ext <- tools::file_ext(output)

  if (isTRUE(ext == "")) {
    object_types <- c(
      'default',
      'tinytable',
      'gt',
      'kableExtra',
      'flextable',
      'huxtable',
      'DT',
      'html',
      'jupyter',
      'latex',
      'latex_tabular',
      'markdown',
      'dataframe',
      'typst',
      'modelsummary_list'
    )
    if (!isTRUE(checkmate::check_choice(output, object_types))) {
      msg <- sprintf(
        "`output` must be a file path or one of: %s",
        paste(object_types, collapse = ", ")
      )
      insight::format_error(msg)
    }
  } else if (!ext %in% names(extension_dict)) {
    msg <- sprintf(
      "`output` supports these file path extensions: %s",
      paste(names(extension_dict), collapse = ", ")
    )
    insight::format_error(msg)
  } else {
    checkmate::assert_path_for_output(output, overwrite = TRUE)
  }

  # default: explicit > knitr > options > config
  if (isTRUE(output == "default")) {
    output_format <- NULL
    if (isTRUE(check_dependency("knitr"))) {
      fmt <- hush(knitr::pandoc_to())
      if (any(fmt_dict$word %in% fmt)) {
        output_format <- "word"
      } else if (any(fmt_dict$latex %in% fmt)) {
        output_format <- "latex"
      } else if (any(fmt_dict$markdown %in% fmt)) {
        output_format <- "markdown"
      } else if (any(fmt_dict$typst %in% fmt)) {
        output_format <- "typst"
      }
    }
    if (is.null(output_format))
      output_format <- getOption("modelsummary_factory_default", default = NULL)
    if (is.null(output_format)) output_format <- config_get("factory_default")
    if (is.null(output_format)) output_format <- "tinytable"
    output_file <- NULL
  } else if (ext %in% names(extension_dict)) {
    output_format <- extension_dict[ext]
    output_file <- output
  } else {
    output_format <- output
    output_file <- NULL
  }

  # do this here again with output_format now in case user supplies global option
  if (isTRUE(output_format == "data.frame")) output_format <- "dataframe"

  output_factory <- factory_dict[[output_format]]

  # kableExtra must specify output_format ex ante (but after factory choice)
  if (
    output_factory == "kableExtra" &&
      output_format %in% c("default", "kableExtra")
  ) {
    if (isTRUE(check_dependency("knitr")) && knitr::is_latex_output()) {
      output_format <- "latex"
    }
  }

  if (
    settings_equal("format_numeric_latex", "siunitx") &&
      (output %in%
        c("latex", "latex_tabular") ||
        tools::file_ext(output) == "tex")
  ) {
    warn_once(latex_compilation_tips, "latex_siunitx_preamble")
  }

  settings_set("output_factory", unname(output_factory))
  settings_set("output_format", unname(output_format))
  settings_set("output_file", unname(output_file))
  out <- list(
    output_factory = unname(output_factory),
    output_format = unname(output_format),
    output_file = unname(output_file)
  )
  return(out)
}
