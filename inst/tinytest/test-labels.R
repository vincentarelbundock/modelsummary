source("helpers.R")
requiet("haven")

# datasummary labels",{
# datasummary()
dat <- mtcars
dat$am <- haven::labelled(
  dat$am,
  label = "Transmission")
dat$mpg <- haven::labelled(
  dat$mpg,
  label = "Miles per Gallon")
dat$hp <- haven::labelled(
  dat$hp,
  label = "Horsepower")
dat$cyl <- haven::labelled(
  dat$cyl,
  label = "Cylinders")


tab <- datasummary(
  output = "dataframe",
  mpg + hp ~ Factor(am) * (Mean + SD) + Factor(vs) * (Mean + SD),
  data = dat)
expect_true(all(c("Miles per Gallon", "Horsepower") %in% tab[[1]]))
expect_true("Transmission / 0 / Mean" %in% colnames(tab))
expect_true("vs / 0 / Mean" %in% colnames(tab))

# datasummary_skim()
tab <- datasummary_skim(dat)
expect_inherits(tab, "kableExtra")
tab <- datasummary_skim(dat, output = "dataframe", histogram = FALSE)
expect_true("Miles per Gallon" %in% tab[[1]])
expect_true("Transmission" %in% tab[[1]])
expect_true("Horsepower" %in% tab[[1]])
expect_true("vs" %in% tab[[1]])

# datasummary_correlation()
tab <- datasummary_correlation(dat, output = "dataframe")
expect_true("Transmission" %in% tab[[1]])
expect_true("Transmission" %in% colnames(tab))

# datasummary_crosstab()
tab <- datasummary_crosstab(am * cyl ~ gear * vs, data = dat, output = "data.frame")
expect_true("Cylinders" %in% colnames(tab))
expect_true("Transmission" %in% colnames(tab))

# # datasummary_balance is not supported yet
# expect_warning(expect_warning(
#     tab <- datasummary_balance(~am, data = dat),
#     pattern = "belled.*balance"),
#     pattern = "It is not safe")



# modelsummary: use variable labels by default
data(trees)
dat <- trees

# no labels
mod <- list(
  "Bivariate" = lm(Girth ~ Height, data = dat),
  "Multivariate" = lm(Girth ~ Height + Volume, data = dat))
tab <- modelsummary(mod, "dataframe")
expect_true(all(c("Height", "Volume") %in% tab$term))

# with labels
dat$Height <- haven::labelled(dat$Height, label = "Height (in feet)")
dat$Volume <- haven::labelled(dat$Volume, label = "Volume (in liters)")
mod <- list(
  "Bivariate" = lm(Girth ~ Height, data = dat),
  "Multivariate" = lm(Girth ~ Height + Volume, data = dat))
tab <- modelsummary(mod, "dataframe", coef_rename = TRUE)
expect_true(all(c("Height (in feet)", "Volume (in liters)") %in% tab$term))



# interaction
data(trees)
dat <- trees
dat$Height <- haven::labelled(dat$Height, label = "Height (in feet)")
dat$Volume <- haven::labelled(dat$Volume, label = "Volume (in liters)")
mod <- lm(Girth ~ Height * Volume, data = dat)
tab <- modelsummary(mod, output = "dataframe", coef_rename = TRUE)
expect_true("Height (in feet) × Volume (in liters)" %in% tab$term)



# coef_rename ignores labels
data(trees)
dat <- trees
dat$height_2 <- haven::labelled(dat$Height, label = "Height (in feet)")
dat$volume_2 <- haven::labelled(dat$Volume, label = "Volume (in liters)")
mod <- list(
  "Bivariate" = lm(Girth ~ height_2, data = dat),
  "Multivariate" = lm(Girth ~ height_2 + volume_2, data = dat))
tab <- modelsummary(mod, "dataframe", coef_rename = coef_rename)
expect_true(all(c("Height 2", "Volume 2") %in% tab$term))



# modelsummary: also applies variable labels for depvar
dat <- mtcars
dat$mpg <- haven::labelled(dat$mpg, label = "Miles per gallon")
dat$cyl <- as.factor(dat$cyl)
dat$cyl <- haven::labelled(dat$cyl, label = "Number of cylinders")
mod <- list(
  lm(mpg ~ cyl + drat + disp, data = dat),
  lm(hp ~ cyl + drat + disp, data = dat))
tab <- modelsummary(dvnames(mod), "dataframe", coef_rename = TRUE)
expect_equivalent(names(tab)[4:5], c("Miles per gallon", "hp"))
expect_true("Number of cylinders" %in% tab$term)
