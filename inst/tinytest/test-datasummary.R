source("helpers.R")

penguins <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv",
  na.strings = "")

# regression test: gt duplicate span labels
tab <- datasummary(
  All(mtcars) ~ Factor(vs) * (Factor(am) * (Mean + SD)),
  data = mtcars, output = "gt")
expect_inherits(tab, "gt_tbl")

# tables::All() warning does not accept tibbles yet
# https://github.com/vincentarelbundock/modelsummary/issues/395
requiet("tibble")
dat <- tibble::as_tibble(mtcars)
tab <- datasummary(All(dat) ~ Mean + SD, data = dat, output = "dataframe")
expect_equal(dim(tab), c(11, 3))

# big.mark formatting
set.seed(10)
rock <- data.frame(
  area = runif(100, 1000, 10000),
  shape = runif(100, 0, 1))
f <- area * Arguments(fmt = 1, big.mark = ",") +
  shape * Arguments(fmt = 3) ~
  Min + Mean + Max
tab <- datasummary(f, data = rock, output = "data.frame")
expect_true("5,007.9" %in% tab[["Mean"]])

# cannot nest two continuous variables
expect_error(suppressMessages(datasummary(mpg * hp ~ Mean + SD, mtcars)))

# numeric content of simple tables
# 2 rows 2 cols
truth <- data.frame(
  c("mpg", "hp"),
  c("20.09", "146.69"),
  c("6.03", "68.56"))
tab <- datasummary(mpg + hp ~ mean + sd,
  output = "dataframe",
  data = mtcars)
expect_true(all(truth == tab))

# nested cols: 1 level
truth <- data.frame(
  rows = c("mpg", "hp"),
  `0 mean` = c("17.15", "160.26"),
  `0 sd` = c("3.83", "53.91"),
  `1 mean` = c("24.39", "126.85"),
  `1 sd` = c("6.17", "84.06"))
tab <- datasummary(mpg + hp ~ Factor(am) * (mean + sd),
  output = "dataframe",
  data = mtcars,
  fmt = "%.2f")
expect_true(all(truth == tab))

# nested cols: 2 levels
tab <- datasummary(mpg + hp ~ Factor(cyl) * Factor(am) * (mean + sd),
  output = "dataframe",
  data = mtcars)
expect_equivalent(dim(tab), c(2, 13))

# nested rows: 1 level
truth <- data.frame(
  am = c("0", "", "1", ""),
  b = c("mpg", "hp", "mpg", "hp"),
  mean = c("17.15", "160.26", "24.39", "126.85"),
  sd = c("3.83", "53.91", "6.17", "84.06"))
tab <- datasummary(Factor(am) * (mpg + hp) ~ mean + sd,
  output = "dataframe",
  data = mtcars,
  fmt = "%.2f")
expect_true(all(truth == tab))

# nested rows: 2 levels
tab <- datasummary(Factor(cyl) * Factor(am) * mpg + hp ~ mean + sd,
  output = "dataframe",
  data = mtcars)
expect_equivalent(dim(tab), c(7, 5))

# Header carry-forward
coln <- c(" ", "4 / mean", "4 / sd", "6 / mean", "6 / sd", "8 / mean", "8 / sd", "median")
tab <- datasummary(mpg + hp ~ Factor(cyl) * (mean + sd) + median,
  data = mtcars,
  output = "dataframe")
expect_equivalent(colnames(tab), coln)


# Factor() is equivalent to assign
tab1 <- datasummary(Factor(am) * (mpg + hp) ~ mean + sd,
  output = "dataframe",
  data = mtcars)
tmp <- mtcars
tmp$am <- factor(tmp$am)
tab2 <- datasummary(am * (mpg + hp) ~ mean + sd,
  output = "dataframe",
  data = tmp)
expect_identical(tab1, tab2)

# logical and characters converted to factors automatically
tmp <- mtcars
tmp$am <- ifelse(tmp$am == 0, FALSE, TRUE)
tmp$cyl <- as.character(tmp$cyl)
tab <- datasummary(cyl * am * (mpg + hp) ~ mean + sd,
  output = "dataframe",
  data = tmp)
expect_equivalent(dim(tab), c(12, 5))

# datasummary: output format do not produce errors
requiet("huxtable")
requiet("kableExtra")

# output formats do not produce errors
for (o in c("huxtable", "flextable", "kableExtra", "data.frame", "markdown", "latex", "html")) {
  tab <- datasummary(All(mtcars) ~ Mean + SD, mtcars, output = o)
}

# failure after move from tables Depends to re-export
tmp <- datasummary(flipper_length_mm + body_mass_g ~ mean * sex,
  output = "data.frame",
  data = penguins)
expect_equivalent(dim(tmp), c(2, 3))

# datasummary: return numeric values
tmp <- datasummary(flipper_length_mm + body_mass_g ~ mean * sex,
  output = "data.frame",
  fmt = NULL,
  data = penguins)
expect_true(is.numeric(tmp$female))
expect_true(is.numeric(tmp$male))

# regression test
url <- "https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv"
penguins <- read.csv(url, na.strings = "")
tab <- datasummary(island * species * body_mass_g ~ sex * (Mean + SD), data = penguins)
expect_inherits(tab, "tinytable")
tab <- datasummary(island * species * body_mass_g ~ sex * (Mean + SD) * DropEmpty(), data = penguins)
expect_inherits(tab, "tinytable")
