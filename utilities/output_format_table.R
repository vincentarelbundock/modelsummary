library(tidyverse)
library(kableExtra)

tab <- tibble::tribble(
~ ` `,        ~ gt,           ~ kableExtra, ~ huxtable,    ~ flextable,
'html',       "✔",            " ",          " ",           " ",
'latex',      " ",            "✔",          " ",           " ",
'markdown',   " ",            "✔",          " ",           " ",
".html",      "✔",            "■",          "■",           "■",
'.rtf',       "✔",            "■",          "■",           " ",
".tex",       "■",            "✔",          "■",           " ",
".md",        " ",            "✔",          " ",           " ",
".docx",      " ",            " ",          "■",           "✔",
".pptx",      " ",            " ",          "■",           "✔",
".jpg",       " ",            "■",          " ",           "✔",
".png",       "■",            "■",          " ",           "✔",
'HTML',       "✔",            "■",          "■",           "■",
'RTF',        "✔",            " ",          "■",           " ",
'PDF',        " ",            "✔",          "■",           " ",
'MS Word',    " ",            " ",          "■",           "✔"
)

tab %>%
    select(` `, gt, kableExtra, flextable, huxtable) %>%
    kable("html",
          align = 'lcccc',
          caption = "Supported output formats. Checkmarks ✔ identify modelsummary's default table writer. Boxes ■ identify alternative supported table writers.") %>%
    kable_styling(full_width = FALSE) %>%
    pack_rows('Display: msummary(models, "format")', 1, 3) %>%
    pack_rows('Save: msummary(models, "filename.ext")', 4, 11) %>%
    pack_rows('Rmarkdown and knitr: msummary(models)', 12, 15) %>%
    save_kable('~/Downloads/output_formats.html')
