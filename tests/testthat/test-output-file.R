# CRAN fails on artefacts
skip_on_cran()
skip_on_ci()
requiet("digest")
requiet("flextable")

random_string <- function() {
  paste(sample(letters, 30, replace=TRUE), collapse="")
}

compare_files <- function(x, y) {
  known <- digest::digest(x, file = TRUE)
  unknown <- digest::digest(y, file = TRUE)
  expect_identical(known, unknown)
}

mod <- list()
mod[[1]] <- lm(hp ~ mpg, mtcars)
mod[[2]] <- lm(hp ~ mpg + drat, mtcars)

# # save known files
# modelsummary(mod, output = test_path("known_output/output-file.md"))
# modelsummary(mod, output = test_path("known_output/output-file.tex"))
# modelsummary(mod, output = test_path("known_output/output-file.txt"))
# modelsummary(mod, output = test_path("known_output/output-file.rtf"))
# modelsummary(mod, output = test_path("known_output/output-file.docx"))
# modelsummary(mod, output = test_path("known_output/output-file.pptx"))
# modelsummary(mod, output = test_path("known_output/output-file.jpg"))
# modelsummary(mod, output = test_path("known_output/output-file.png"))

extensions <- c(".md", ".tex", ".txt", ".rtf", ".jpg")
for (x in extensions) {
  testname <- paste0("output='table", x, "'")
  test_that(testname, {
    fn1 <- paste0(random_string(), x)
    fn2 <- test_path(paste0("known_output/output-file", x))
    modelsummary(mod, output = fn1)
    compare_files(fn1, fn2)
    unlink(fn1)
  })
}


# png hash changes from write to write
test_that("output='table.png'", {
  fn1 <- paste0(random_string(), x)
  modelsummary(mod, output = fn1)
  expect_gt(file.info(fn1)$size, 10000)
  unlink(fn1)
})

# docx hash changes from write to write
test_that("output='table.docx'", {
  fn1 <- paste0(random_string(), ".docx")
  modelsummary(mod, output = fn1)
  expect_gt(file.info(fn1)$size, 10000)
  unlink(fn1)
})

# pptx hash changes from write to write
test_that("output='table.pptx'", {
  fn1 <- paste0(random_string(), ".pptx")
  modelsummary(mod, output = fn1)
  expect_gt(file.info(fn1)$size, 10000)
  unlink(fn1)
})
