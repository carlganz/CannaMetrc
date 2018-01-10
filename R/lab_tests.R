#' Get Lab Test States
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#LabTests.get_labtests_v1_states}

metrc_get_lab_test_states <- function() {
    metrc_call("GET", "labtests/v1/states") %>%
      unlist()
}

#' Get Lab Test Types
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#LabTests.get_labtests_v1_types}

metrc_get_lab_test_types <- function() {
  metrc_call("GET", "labtests/v1/types") %>%
    bind_rows()
  
}

#' Post Lab Tests
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#LabTests.post_labtests_v1_record}

metrc_post_lab_test <- function(license_number, results) {
  metrc_call("POST", "labtests/v1/states", license_number = license_number, body = results)
  
}