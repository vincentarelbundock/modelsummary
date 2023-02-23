weighted_sd <- function(x, w) {
  # Adapted from `vtable` by Nick Huntington-Klein under MIT license
  if (min(w) < 0) {
    stop('Negative weights found.')
  }
  idx <- !is.na(x) & !is.na(w)
  x <- x[idx]
  w <- w[idx]
  weightsum <- sum(w)
  if (weightsum == 0) {
    stop('Weights sum to 0 among nonmissing data.')
  }
  mean_x <- sum(w * x) / weightsum
  num_nonzero <- sum(w > 0)
  var_x <- sum(w * ((x - mean_x)^2)) / (weightsum * (num_nonzero - 1) / num_nonzero)
  sd_x <- sqrt(var_x)
  return(sd_x)
}