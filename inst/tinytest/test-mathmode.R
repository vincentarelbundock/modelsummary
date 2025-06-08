source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

mod <- list(
  lm(mpg ~ hp, mtcars),
  lm(mpg ~ hp + drat, mtcars)
)

# d-column: known output
expect_snapshot_print(
  modelsummary(mod, align = "ldd", output = "latex"),
  "mathmode-latex_ldd"
)

# LaTeX global options
expect_snapshot_print(
  modelsummary(mod, output = "latex"),
  "mathmode-latex_null"
)

options("modelsummary_format_numeric_latex" = "dollars")
expect_snapshot_print(
  modelsummary(mod, output = "latex", escape = FALSE),
  "mathmode-latex_dollars"
)

options("modelsummary_format_numeric_latex" = "anything else")
expect_snapshot_print(
  modelsummary(mod, output = "latex"),
  "mathmode-latex_anything"
)

options("modelsummary_format_numeric_latex" = NULL)


# HTML global options
options(tinytable_html_mathjax = TRUE)
expect_snapshot_print(
  print_html(modelsummary(mod, output = "html")),
  "mathmode-null.html"
)

options("modelsummary_format_numeric_html" = "dollars")
expect_snapshot_print(
  print_html(modelsummary(mod, output = "html")),
  "mathmode-html_dollars.html"
)
options(tinytable_html_mathjax = NULL)

options("modelsummary_format_numeric_html" = "anything else")
expect_snapshot_print(
  print_html(modelsummary(mod, output = "html")),
  "mathmode-html_anything.html"
)
options("modelsummary_format_numeric_html" = NULL)
