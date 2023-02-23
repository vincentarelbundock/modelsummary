# CRAN fails on artefacts
# skip_on_cran()
source("helpers.R")
requiet("digest")
requiet("flextable")

mod <- list()
mod[[1]] <- lm(hp ~ mpg, mtcars)
mod[[2]] <- lm(hp ~ mpg + drat, mtcars)

# # save known files
# modelsummary(mod, output = test_path("known_output/output-file.md"))
# modelsummary(mod, output = test_path("known_output/output-file.txt"))
# modelsummary(mod, output = test_path("known_output/output-file.tex"))
# modelsummary(mod, output = test_path("known_output/output-file.txt"))
# modelsummary(mod, output = test_path("known_output/output-file.rtf"))
# modelsummary(mod, output = test_path("known_output/output-file.docx"))
# modelsummary(mod, output = test_path("known_output/output-file.pptx"))
# modelsummary(mod, output = test_path("known_output/output-file.jpg"))
# modelsummary(mod, output = test_path("known_output/output-file.png"))

# exit_file("broken")
extensions <- c(".md", ".tex", ".txt", ".rtf") # , ".jpg")
for (x in extensions) {
  testname <- paste0("output='table", x, "'")
  fn1 <- paste0(random_string(), x)
  fn2 <- file.path("known_output", paste0("output-file", x))
  modelsummary(mod, output = fn1)
  expect_true(compare_files(fn1, fn2))
  unlink(fn1)
}

# docx hash changes from write to write
# output='table.docx'
fn1 <- paste0(random_string(), ".docx")
modelsummary(mod, output = fn1)
expect_true(file.info(fn1)$size > 10000)
unlink(fn1)


# pptx hash changes from write to write
# output='table.pptx'
fn1 <- paste0(random_string(), ".pptx")
modelsummary(mod, output = fn1)
expect_true(file.info(fn1)$size > 10000)
unlink(fn1)
