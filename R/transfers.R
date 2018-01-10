#' Get Incoming Transfers
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_incoming}
metrc_get_transfers_incoming <- function(license_number) {
  metrc_call("GET", "transfers/v1/incoming", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Outgoing Transfers
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_outgoing}
metrc_get_transfers_outgoing <- function(license_number) {
  metrc_call("GET", "transfers/v1/outgoing", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Rejected Transfers
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_rejected}
metrc_get_transfers_rejected <- function(license_number) {
  metrc_call("GET", "transfers/v1/rejected", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Deliveries
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_{id}_deliveries}
metrc_get_transfers_delivery <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", paste0("transfers/v1/", id, "/deliveries")) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Packages
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Transfers.get_transfers_v1_delivery_{id}_packages}
metrc_get_transfers_packages <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", paste0("transfers/v1/delivery/", id, "/packages")) %>%
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Package States
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Transfers.get_transfers_v1_delivery_{id}_packages}
metrc_get_transfers_package_states <- function() {
  metrc_call("GET", "transfers/v1/delivery/packages/states") %>%
    unlist()
}