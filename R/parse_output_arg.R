#' parse an `output` argument to determine which table factory and which output
#' format to use.
#'
#' @noRd
parse_output_arg <- function(output) {

  extension_dict <- c(
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
    "dataframe"  = "dataframe",
    "data.frame" = "dataframe",
    "flextable"  = "flextable",
    "gt"         = "gt",
    "huxtable"   = "huxtable",
    "kableExtra" = "kableExtra",
    "markdown"   = "kableExtra",
    "html"       = getOption("modelsummary_html", default = "kableExtra"),
    "latex"      = getOption("modelsummary_latex", default = "kableExtra"),
    "jpg"        = getOption("modelsummary_jpg", default = "kableExtra"),
    "png"        = getOption("modelsummary_png", default = "kableExtra"),
    "rtf"        = getOption("modelsummary_rtf", default = "gt"),
    "word"       = getOption("modelsummary_word", default = "flextable"),
    "powerpoint" = getOption("modelsummary_powerpoint", default = "flextable"))

  # sanity check: are user-supplied global options ok?
  sanity_factory(factory_dict)

  # valid defaults are set in `sanity_output`
  if (output == "default") {
    output <- getOption('modelsummary_default', default = 'kableExtra')
  }

  # kableExtra is the only factory that produces human-readable code
  if (output %in% c('markdown', 'html', 'latex')) {
    out <- list(
      'output_factory' = 'kableExtra',
      'output_file'    = NULL,
      'output_format'  = output)
    return(out)
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

  # knit to word
  if (check_dependency("knitr") && check_dependency("rmarkdown")) {
    fmt <- try(rmarkdown::default_output_format(
      knitr::current_input())$name, silent = TRUE)
    if (!inherits(fmt, "try-error")) {
      word_fmt <- c("word_document", "rdocx_document", "officedown::rdocx_document")
      if (any(word_fmt %in% fmt)) {
        output_format <- "word"
      }
    }
  }

  # choose factory based on output_format
  output_factory <- factory_dict[[output_format]]

  # kableExtra must specify output_format ex ante (but after factory choice)
  if (output_factory == 'kableExtra' &&
      output_format %in% c('default', 'kableExtra')) {
    automatic <- ifelse(knitr::is_latex_output(), 'latex', 'html')
    output_format <- getOption('modelsummary_kableExtra', default = automatic)
  }

  # result
  out <- list(
    # unname to avoid weird issue in kableExtra::kbl do.call arguments
    "output_factory" = unname(output_factory),
    "output_file"    = unname(output_file),
    "output_format"  = unname(output_format))
  return(out)

}
