#' parse an `output` argument to determine which table factory and which output
#' format to use.
#'
#' @noRd
sanitize_output <- function(output) {


  flag <- checkmate::check_string(output)
  fun <- settings_get("function_called")
  if (!isTRUE(flag) && !is.null(fun) && fun == "modelsummary") {
    stop("The `output` argument must be a string. Type `?modelsummary` for details. This error is sometimes raised when users supply multiple models to `modelsummary` but forget to wrap them in a list. This works: `modelsummary(list(model1, model2))`. This does *not* work: `modelsummary(model1, model2)`")
  }

  object_types <- c('default', 'gt', 'kableExtra', 'flextable', 'huxtable',
                    'html', 'jupyter', 'latex', 'latex_tabular', 'markdown',
                    'dataframe', 'data.frame', 'modelsummary_list')
  extension_types <- c('html', 'tex', 'md', 'txt', 'docx', 'pptx', 'rtf',
                       'jpg', 'png')

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


  ## deprecated global options
  bad <- c("modelsummary_default", "modelsummary_html", "modelsummary_latex",
           "modelsummary_markdown", "modelsummary_jpg", "modelsummary_png",
           "modelsummary_rtf", "modelsummary_word", "modelsummary_powerpoint")
  for (a in bad) {
      b <- gsub("modelsummary_", "modelsummary_factory_", a)
      tmp <- getOption(a, default = NULL)
      if (!is.null(tmp)) {
          options(b = tmp)
          msg <- sprintf('The "%s" global option is deprecated. Please use "%s" instead.', a, b)
          rlang::warn( message = msg,
                      .frequency = "once",
                      .frequency_id = a)
      }
  }


  factory_dict <- c(
    "dataframe"      = "dataframe",
    "data.frame"     = "dataframe",
    "flextable"      = "flextable",
    "gt"             = "gt",
    "huxtable"       = "huxtable",
    "kableExtra"     = "kableExtra",
    "markdown"       = "kableExtra",
    "latex_tabular"  = "kableExtra",
    "modelsummary_list" = "modelsummary_list",
    "jupyter"        = getOption("modelsummary_factory_html", default       = "kableExtra"),
    "latex"          = getOption("modelsummary_factory_latex", default      = "kableExtra"),
    "html"           = getOption("modelsummary_factory_html", default       = "kableExtra"),
    "jpg"            = getOption("modelsummary_factory_jpg", default        = "kableExtra"),
    "png"            = getOption("modelsummary_factory_png", default        = "kableExtra"),
    "rtf"            = getOption("modelsummary_factory_rtf", default        = "gt"),
    "word"           = getOption("modelsummary_factory_word", default       = "flextable"),
    "powerpoint"     = getOption("modelsummary_factory_powerpoint", default = "flextable"))

  ## sanity check: are user-supplied global options ok?
  sanity_factory(factory_dict)

  ## save user input to check later
  output_user <- output

  # defaults
  if (output == "default") {
      output <- getOption("modelsummary_factory_default", default = "kableExtra")
  } else if (output == "jupyter") {
      output <- "html"
  }

  # kableExtra is the only factory that I use for markdown
  if (output == 'markdown') {
    settings_set("output_factory", "kableExtra")
    settings_set("output_format", "markdown")
    settings_set("output_file", NULL)
    return(invisible(NULL))
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

  # knit to word using flextable
  if (isTRUE(check_dependency("knitr")) && isTRUE(check_dependency("rmarkdown"))) {
    ## try to guess the knitr output format
    fmt <- try(rmarkdown::default_output_format(knitr::current_input())$name, silent = TRUE)
    if (!inherits(fmt, "try-error")) {
      word_fmt <- c("word_document",
                    "rdocx_document", "officedown::rdocx_document",
                    "word_document2", "bookdown::word_document2")
      # unfortunately, `rmarkdown::default_output_format` only detects
      # `html_document` on reprex, so this will only work in `github_document`
      markdown_fmt <- c("github_document",
                        "reprex_render",
                        "reprex::reprex_render")
      ## change to word output format only if `output` is "default" or "flextable"
      if (any(word_fmt %in% fmt) && output_user %in% c("flextable", "default")) {
        output_format <- "word"
      ## reprex and github: change to markdown output format only if `output` is "default"
      } else if (any(markdown_fmt %in% fmt) && (output_user == "default")) {
        output_format <- "markdown"
      }
    }
  }

  # choose factory based on output_format
  output_factory <- factory_dict[[output_format]]

  # kableExtra must specify output_format ex ante (but after factory choice)
  if (output_factory == 'kableExtra' &&
      output_format %in% c('default', 'kableExtra') &&
      knitr::is_latex_output()) {
    output_format <- "latex"
  }

  ## warning siunitx & booktabs in preamble
  if (settings_equal("format_numeric_latex", "siunitx") && (output %in% c("latex", "latex_tabular") || tools::file_ext(output) == "tex")) {
      rlang::warn( message = 
'To compile a LaTeX document with this table, the following commands must be placed in the document preamble:

\\usepackage{booktabs}
\\usepackage{siunitx}
\\newcolumntype{d}{S[input-symbols = ()]}

To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\\num{}`, call:

options("modelsummary_format_numeric_latex" = "plain")
',
      .frequency = "once",
      .frequency_id = "siunitx_preamble")
  }

  # settings environment
  settings_set("output_factory", unname(output_factory))
  settings_set("output_format", unname(output_format))
  settings_set("output_file", unname(output_file))

}
