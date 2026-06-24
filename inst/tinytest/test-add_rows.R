source("helpers.R")
requiet("tibble")

mod <- list()

# exit_file("<<-")

dat <- mtcars
dat$cyl <- factor(dat$cyl)
# dat <<- dat
mod$OLS <- lm(am ~ cyl, data = dat)
mod$Logit <- glm(am ~ cyl, data = dat, family = binomial())

# data.frame
rows = read.csv(
  text = "term      , OLS , Logit
   cyl4      , -   , -
   NEW GOF 1 , ?   , ?
   NEW GOF 2 , X   , X
   NEW GOF 3 , Y   , Y"
)
attr(rows, "position") <- c(3, 8, 9, 12)
tab <- modelsummary(mod, add_rows = rows, output = "data.frame")
expect_equivalent(ncol(tab), 5)
expect_true(nrow(tab) > 14)

# Issue #964: "coef_end" rows belong above the coef/gof separator.
line_style_rows <- function(tab) {
  idx <- vapply(tab@lazy_style, function(z) identical(z$line, "b"), logical(1))
  as.integer(vapply(tab@lazy_style[idx], function(z) z$i, numeric(1)))
}

new_row <- data.frame(term = "test", `(1)` = "new row", check.names = FALSE)
attr(new_row, "position") <- "coef_end"
tab <- modelsummary(lm(mpg ~ hp, data = mtcars), add_rows = new_row, output = "tinytable")
expect_true(match("test", tab@data[[1]]) < match("Num.Obs.", tab@data[[1]]))
expect_equivalent(line_style_rows(tab), 5)

attr(new_row, "position") <- "gof_start"
tab <- modelsummary(lm(mpg ~ hp, data = mtcars), add_rows = new_row, output = "tinytable")
expect_true(match("test", tab@data[[1]]) < match("Num.Obs.", tab@data[[1]]))
expect_equivalent(line_style_rows(tab), 4)

# add_rows numeric are formatted by fmt
tmp <- data.frame(a = 1:2, b = 2:3, c = 3:4)
tab <- datasummary(
  hp + mpg ~ mean + sd,
  data = mtcars,
  add_rows = tmp,
  fmt = "%.0f",
  output = "dataframe"
)
expect_equivalent(tab$sd, c("69", "6", "3", "4"))

