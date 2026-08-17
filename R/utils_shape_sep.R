# Separator used by `data.table::dcast()` in `shape_estimates()` to glue the
# components of a reshaped column name together, as in "model||||statistic".
# Model labels are user-supplied and may contain anything, so code that needs
# to recover the components must split on this string rather than search for
# substrings.
SHAPE_SEP <- "||||"

# Split reshaped column names into their components.
shape_sep_split <- function(x) {
  strsplit(x, SHAPE_SEP, fixed = TRUE)
}
