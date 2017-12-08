#' Get Lab Test States
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#LabTests.get_labtests_v1_states}

metrc_get_lab_test_states <- function() {
    
    url <- modify_url(
      BASE_URL(), path = "labtests/v1/states"
    )
    
    resp <- GET(url, metrc_auth())
    
    if (http_type(resp) != "application/json") {
      stop("metrc API did not return JSON.", call. = FALSE)
    }
    
    if (http_error(resp)) {
      stop(paste0("metrc API errored:\n", 
                  http_status(resp)$message), call. = FALSE)
    }
    
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
      unlist()
}

#' Get Lab Test Types
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#LabTests.get_labtests_v1_types}

metrc_get_lab_test_types <- function() {
  
  url <- modify_url(
    BASE_URL(), path = "labtests/v1/types"
  )
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE) %>%
    bind_rows()
}

#' Post Lab Tests
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#LabTests.post_labtests_v1_record}

metrc_post_lab_test <- function(license_number, results) {
  
  url <- modify_url(
    BASE_URL(), path = "labtests/v1/states",
    query = list(
      licenseNumber = license_number
    )
  )
  
  resp <- POST(url, metrc_auth(), body = results)
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n", 
                http_status(resp)$message), call. = FALSE)
  }
  
  if (http_type(resp) != "application/json") {
    return(TRUE)
  } else {
    fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }
  
}