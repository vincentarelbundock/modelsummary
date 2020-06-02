library(tidyverse)
library(kableExtra)

tab <- tibble::tribble(
~ ` `,        ~ gt,           ~ kableExtra, ~ huxtable,    ~ flextable,
'default',    "✔",            "•",          "•",           "•",
'gt',         "✔",            " ",          " ",           " ",
'kableExtra', " ",            "✔",          " ",           " ",
'flextable',  " ",            " ",          " ",           "✔",
'huxtable',   " ",            " ",          "✔",           " ",
".html",      "✔",            "•",          "•",           "•",
'.rtf',       "✔",            " ",          "•",           " ",
".tex",       "•",            "✔",          "•",           " ",
".md",        " ",            "✔",          " ",           " ",
".docx",      " ",            " ",          "•",           "✔",
".pptx",      " ",            " ",          "•",           "✔",
".jpg",       " ",            " ",          " ",           "✔",
".png",       "•",            " ",          " ",           "✔",
'HTML',       "✔",            "•",          "•",           "•",
'RTF',        "✔",            " ",          "•",           " ",
'PDF',        " ",            "✔",          "•",           " ",
'MS Word',    " ",            " ",          "•",           "✔",
'html',       " ",            "✔",          " ",           " ",
'latex',      " ",            "✔",          " ",           " ",
'markdown',   " ",            "✔",          " ",           " "
)

tab %>%
    select(` `, gt, kableExtra, flextable, huxtable) %>%
    kable("html",
          align = 'lcccc') %>%
          #caption = "Supported output formats. Checkmarks ✔ identify modelsummary's default table writer. Dots • identify alternative table writers which can be selected through global options (e.g., `options(modelsummary_default='kableExtra')`).") %>%
    kable_styling(full_width = FALSE) %>%
    pack_rows('Display: msummary(models, "package")', 1, 5) %>%
    pack_rows('Save: msummary(models, "filename.ext")', 6, 13) %>%
    pack_rows('Embed in Rmarkdown or knitr: msummary(models)', 14, 17) %>%
    pack_rows('Human-readable code: msummary(models, "language")', 18, 20) %>%
    save_kable('~/Downloads/output_formats.html')
