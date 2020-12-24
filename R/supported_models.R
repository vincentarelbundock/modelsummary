#' List of model objects from which `modelsummary` can extract estimates and
#' statistics
#' 
#' @export
supported_models <- function() {
  broom_tidy <- ls(getNamespace("broom"))
  broom_tidy <- broom_tidy[grepl("^tidy", broom_tidy)]
  broom_supported <- gsub("^tidy.", "", broom_tidy)
  easystats_tidy <- ls(getNamespace("parameters"))
  easystats_tidy <- easystats_tidy[grepl("^model_parameters", easystats_tidy)]
  easystats_supported <- gsub("^model_parameters.", "", easystats_tidy)
  supported <- sort(unique(c(broom_supported, easystats_supported)))
  return(supported)
}
