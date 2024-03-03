source("helpers.R")

tmp <- head(mtcars)

# no error + code coverage
tmp$cyl <- as.factor(tmp$cyl)

ar <- tail(mtcars, 1)
ac <- data.frame(new = 1:6)

tab <- datasummary_df(
    data = tmp,
    output = "default",
    fmt = "%.0f",
    align = strrep("r", ncol(tmp)),
    hrule = 3,
    title = "blah blah title",
    notes = c("first note", "second note"),
    add_rows = ar)
expect_inherits(tab, "tinytable")
