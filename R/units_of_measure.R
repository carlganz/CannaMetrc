#' Get Units of Measurement
#' @note See \url{https://api-co.metrc.com/Documentation/#UnitsOfMeasure.get_unitsofmeasure_v1_active}
#' @export

metrc_get_units <- function() {
  metrc_call("GET", "unitsofmeasure/v1/active") %>% 
    bind_rows()
  
}