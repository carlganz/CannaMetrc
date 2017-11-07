metrc_create_plantings <- function(name, type  = c("seed", "clone"), count, strain, actual_date) {
  type <- match.call(type)
  
  stopifnot(is.integer(count))
  
  actual_date <- format(as.Date(actual_date), "%Y-%m-%d")
  
  structure(data.frame(
    Name = name,
    Type = type,
    Count = count,
    Strain = strain,
    ActualDate = actual_date
  ), class = c("metrc_plantings", "data.frame"))
  
}

metrc_create_package <- function(...) {
  UseMethod("metrc_create_package")
}

metrc_create_package.metrc_plantings <- function() {
  
}