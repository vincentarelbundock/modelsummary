source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")

# markdown d-column snapshot
mod <- lm(mpg ~ I(hp / 1000) + am + vs + factor(cyl), mtcars)
expect_snapshot_print(
  modelsummary(mod, "markdown", align = "ld"),
  "align-md_dcolumn"
)
