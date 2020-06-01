library(gt)
library(magrittr)
library(tibble)
library(kableExtra)

tab <- tibble::tribble(
~ ` `,               ~ gt,           ~ kableExtra, ~ huxtable,    ~ flextable,
'html',                      "✔",            "■",          "■",           "■",
'latex',                  "■",            "✔",          "■",           " ",
'markdown',             " ",            "✔",          "■",           " ",
'gt',             "✔",            "",          "",           " ",
'kableExtra',             " ",            "✔",          "",           " ",
'huxtable',             " ",            "",          "✔",           " ",
'flextable',             " ",            "",          "",           "✔",
".html",      "✔",            "■",          "■",           "■",
".tex",       "■",            "✔",          "■",           " ",
".md",        " ",            "✔",          "■",           " ",
".docx",      " ",            " ",          "✔",           "■",
".pptx",      " ",            " ",          "✔",           "■",
".xlsx",      " ",            " ",          "✔",           " ",
".jpg, .png",   "✔",            "■",          "■",           "■",          
'.rtf',                 "✔",            "■",          "■",           " ",
'HTML',                    "✔",            "■",          "■",           "■",
'PDF',                       " ",            "✔",          "■",           " ",
'MS Word',                " ",            " ",          "✔",           "■",
'RTF',                    "✔",            "■",          "■",           " "
)

tab %>%
    kable("html",
          align = 'lcccc',
          caption = "Supported output formats. Checkmarks ✔ identify modelsummary's default table writer. Black boxes ■ identify alternative supported table writers.") %>%
    kable_styling(full_width = FALSE) %>%
    pack_rows('Display: msummary(models, "format")', 1, 7) %>%
    pack_rows('Save: msummary(models, "filename.ext")', 8, 15) %>%
    pack_rows('Rmarkdown and knitr: msummary(models)', 16, 19) %>%
    save_kable('~/Downloads/output_formats.html')
