#' Get Customer Types
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_customertypes}
metrc_get_sales_customer_types <- function() {
  metrc_call("GET", "sales/v1/customertypes") %>%
    unlist()
}

#' Get Sales Receipts
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_receipts}
metrc_get_sales_receipts <- function(license_number) {
  metrc_call("GET", "sales/v1/receipts", license_number = license_number) %>% {
    tibble(
      Id = map_int(., "Id"),
      ReceiptNumber = map_chr(., "ReceiptNumber"),
      SalesDateTime = map_chr(., "SalesDateTime"),
      SalesCustomerType = map_chr(., "SalesCustomerType"),
      PatientLicenseNumber = map_chr(., "PatientLicenseNumber"),
      CaregiverLicenseNumber = map_chr(., "CaregiverLicenseNumber"),
      TotalPackages = map_int(., "TotalPackages"),
      TotalPrice = map_dbl(., "TotalPrice"),
      Transactions = map(., "Transactions") %>%
        map(function(x) {x  %>% map(dropNullsOrEmpty) %>% bind_rows()})
    )
  }
}

#' Get Receipt
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_receipts_{id}}
metrc_get_sales_receipt <- function(id) {
  stopifnot(is.integer(id))
  metrc_call("GET", "sales/v1/receipts", id = id) %>% {
    tibble(
      Id = .[["Id"]],
      ReceiptNumber = .[["ReceiptNumber"]],
      SalesDateTime = .[["SalesDateTime"]],
      SalesCustomerType = .[["SalesCustomerType"]],
      PatientLicenseNumber = .[["PatientLicenseNumber"]],
      CaregiverLicenseNumber = .[["CaregiverLicenseNumber"]],
      TotalPackages = .[["TotalPackages"]],
      TotalPrice = .[["TotalPrice"]],
      Transactions = .[["Transactions"]] %>% 
        map(dropNullsOrEmpty) %>% bind_rows() %>% list()
    )
  }
    
}

#' Post Receipts
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.post_sales_v1_receipts}
metrc_post_sales_receipt <- function(license_number, sales) {
  metrc_call("POST", "sales/v1/receipts", license_number = license_number, body = sales)
  
}

#' Put Receipts
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.put_sales_v1_receipts}
metrc_put_sales_receipt <- function(license_number, sales) {
  metrc_call("PUT", "sales/v1/receipts", license_number= license_number, body = sales)
  
}

#' Delete Receipt
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.delete_sales_v1_receipts_{id}}
metrc_delete_sales_receipt <- function(license_number, id) {
  stopifnot(is.integer(id))
  metrc_call("DELETE", "sales/v1/receipts", id = id, license_number= license_number)
  
}

#' Get Sales Transactions
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_transactions}
metrc_get_sales_transactions <- function(license_number) {
  metrc_call("GET", "salestransactions/v1", license_number = license_number)
}

#' Get Sales Transactions by Date
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_transactions_{date}}
metrc_get_sales_transactions_date <- function(license_number, date) {
  date <- format(as.Date(date), "%Y-%m-%d")
  metrc_call("GET", "sales/v1/transactions", id = date, license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Post Sales Transactions by Date
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.post_sales_v1_transactions_{date}}
metrc_post_sales_transactions_date <- function(license_number, date, package_label,
                                               quantity, unit_of_measure, total_amount) {
  date <- format(as.Date(date), "%Y-%m-%d")
  metrc_call("POST", "sales/v1/transactions", id = date, license_number = license_number, body = data.frame(
    PackageLabel = package_label,
    Quantity = quantity, 
    UnitOfMeasure = unit_of_measure, 
    TotalAmount = total_amount
  ))
  
}

#' Put Sales Transactions by Date
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.post_sales_v1_transactions_{date}}
metrc_put_sales_transactions_date <- function(license_number, date, package_label,
                                               quantity, unit_of_measure, total_amount) {
  date <- format(as.Date(date), "%Y-%m-%d")
  metrc_call("PUT", "sales/v1/transactions", id = date, license_number = license_number, body = data.frame(
    PackageLabel = package_label,
    Quantity = quantity, 
    UnitOfMeasure = unit_of_measure, 
    TotalAmount = total_amount
  ))
  
}

#' Get Deliveries
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.get_sales_v1_deliveries}
metrc_get_deliveries <- function(license_number) {
  metrc_call("GET", "sales/v1/deliveries", license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% bind_rows()
}

#' Get Delivery Info
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.get_sales_v1_deliveries}
metrc_get_delivery <- function(license_number, delivery_id) {
  stopifnot(is.integer(delivery_id))
  metrc_call("GET", "sales/v1/delivery", id = delivery_id, license_number = license_number) %>% 
    map(dropNullsOrEmpty) %>% as_tibble()
}

#' Return Reasons
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.get_sales_v1_delivery_returnreasons}
metrc_get_return_reasons <- function() {
  metrc_call("GET", "sales/v1/delivery/returnreasons") %>%
    unlist()
}

#' Post Deliveries
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.post_sales_v1_deliveries}
metrc_post_deliveries <- function(license_number, deliveries) {
  metrc_call("POST", "sales/v1/deliveries", license_number = license_number, body = deliveries)

}

#' Put Deliveries
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.put_sales_v1_deliveries}
metrc_put_deliveries <- function(license_number, deliveries) {
  metrc_call("PUT", "sales/v1/deliveries", license_number = license_number, body = deliveries)
  
}

#' Complete Deliveries
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.put_sales_v1_deliveries_complete}
metrc_complete_deliveries <- function(license_number, deliveries) {
  metrc_call("PUT", "sales/v1/deliveries/complete", license_number = license_number, body = deliveries)
  
}

#' Delete Delivery
#' @export
#' @note See \url{https://api-or.metrc.com/Documentation/#Sales.delete_sales_v1_delivery_{id}}
metrc_delete_sales_receipt <- function(license_number, id) {
  stopifnot(is.integer(id))
  metrc_call("DELETE", "sales/v1/delivery", id = id, license_number = license_number)
  
}

