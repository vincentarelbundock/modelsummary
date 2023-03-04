source("helpers.R")
requiet("tinysnapshot")
using("tinysnapshot")
# results is the same, but order of `str()` components is swapped
if (getRversion() < "4.0.0") exit_file("old R")


# too many rows in add_columns
ac <- read.csv(text =
  "first,last
   blah,2
   junk,4
   another,5")
expect_error(datasummary(mpg + hp ~ mean + sd, data = mtcars, add_columns = ac))

# add_columns support in modelsummary.
mod <- lm(mpg ~ hp, mtcars)
ac <- data.frame(X = letters[1:4])
attr(ac, "position") <- 2
tab <- modelsummary(mod, output = "dataframe", gof_map = NA, add_columns = ac)
expect_equivalent("X", colnames(tab)[2])
expect_equivalent(ncol(tab), 5)


# datasummary add_columns
ac <- read.csv(text = 
"first,last
blah,2
junk,4")
attr(ac, 'position') <- c(1, NA)
tab <- datasummary(mpg + hp ~ mean + sd,
  data = mtcars,
  add_columns = ac,
  fmt = '%.2f',
  output = 'dataframe')
expect_snapshot_print(tab, "add_columns-dataframe")