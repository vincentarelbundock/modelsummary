source("helpers.R")
requiet("tinysnapshot")
requiet("magrittr")
using("tinysnapshot")

# flextable is not installed on CRAN's M1 machine
# huxtable is not installed on CRAN's solaris / fedora-clang / fedora-gcc
requiet("flextable")
requiet("huxtable")

models <- list()
models[["OLS 1"]] <- lm(hp ~ mpg + wt, mtcars)
models[["Poisson 1"]] <- glm(hp ~ mpg + drat, mtcars, family = poisson())
models[["OLS 2"]] <- lm(vs ~ hp + wt, mtcars)
models[["Logit 1"]] <- glm(vs ~ hp + drat, mtcars, family = binomial())
models[["Logit 2"]] <- glm(am ~ hp + disp, mtcars, family = binomial())

# markdown caption and notes
# minor UTF8 encoding issue on CRAN and Windows
expect_warning(
  modelsummary(
    models,
    "huxtable",
    title = "test title",
    notes = "test note",
    stars = TRUE
  ) %>%
    huxtable::to_md()
)
unknown <- suppressWarnings(
  modelsummary(
    models,
    output = "huxtable",
    title = "test title",
    notes = "test note",
    stars = TRUE
  ) %>%
    huxtable::to_md()
)
expect_snapshot_print(unknown, "huxtable-md_title_note_stars")

# save to file
options(modelsummary_factory_html = "huxtable")
options(modelsummary_factory_rtf = "huxtable")
options(modelsummary_factory_latex = "huxtable")
options(modelsummary_factory_word = "huxtable")
options(modelsummary_factory_powerpoint = "huxtable")

random <- random_string()

filename <- paste0(random, ".html")
tab <- modelsummary(models, filename)
unlink(filename)

filename <- paste0(random, ".rtf")
tab <- modelsummary(models, filename)
unlink(filename)

filename <- paste0(random, ".tex")
tab <- modelsummary(models, filename)
unlink(filename)

filename <- paste0(random, ".docx")
tab <- modelsummary(models, filename)
unlink(filename)

filename <- paste0(random, ".pptx")
tab <- modelsummary(models, filename)
unlink(filename)

options(modelsummary_factory_html = "kableExtra")
options(modelsummary_factory_rtf = "gt")
options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_factory_word = "flextable")
options(modelsummary_factory_powerpoint = "flextable")
