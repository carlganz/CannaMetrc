#' Get Incoming Transfers
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_incoming}
metrc_get_transfers_incoming <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "transfers/v1/incoming",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Outgoing Transfers
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_outgoing}
metrc_get_transfers_outgoing <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "transfers/v1/outgoing",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Rejected Transfers
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_rejected}
metrc_get_transfers_outgoing <- function(license_number) {
  url <- modify_url(BASE_URL,
                    path = "transfers/v1/rejected",
                    query = list(licenseNumber = license_number))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Deliveries
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_{id}_deliveries}
metrc_get_transfers_deliveries <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL,
                    path = paste0("transfers/v1/", id, "/deliveries"))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Packages
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_{id}_deliveries}
metrc_get_transfers_packages <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL,
                    path = paste0("transfers/v1/", id, "/packages"))
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

#' Get Package States
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_delivery_{id}_packages}
metrc_get_transfers_packages <- function() {
  url <- modify_url(BASE_URL,
                    path = "transfers/v1/delivery/packages/states")
  
  resp <- GET(url, metrc_auth())
  
  if (http_type(resp) != "application/json") {
    stop("metrc API did not return JSON.", call. = FALSE)
  }
  
  if (http_error(resp)) {
    stop(paste0("metrc API errored:\n",
                http_status(resp)$message),
         call. = FALSE)
  }
  
  fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
}