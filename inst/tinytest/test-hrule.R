source("helpers.R")
# Issue #913
# hrule = 2 means after header+row1. hrule=1 doesn't make sense because
# there is always a separator between header and first row.
expect_error(datasummary_df(head(iris), output = "tinytable", hrule = 1), ">= 2")
