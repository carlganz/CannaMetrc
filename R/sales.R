#' Get Customer Types
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_customertypes}
metrc_get_sales_customer_types <- function() {
  url <- modify_url(BASE_URL, path = "sales/v1/customertypes")
  
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

#' Get Sales Receipts
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_receipts}
metrc_get_sales_receipts <- function(license_number) {
  url <- modify_url(BASE_URL, path = "sales/v1/receipts",
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

#' Get Receipt
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_receipts_{id}}
metrc_get_sales_receipt <- function(id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL, path = paste0("sales/v1/receipts/",id))
  
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

#' Post Receipts
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.post_sales_v1_receipts}
metrc_post_sales_receipt <- function(license_number, sales) {
  url <- modify_url(BASE_URL, path = "sales/v1/receipts",
                    query = list(licenseNumber = license_number))
  
  resp <- POST(url, metrc_auth(), body = sales)
  
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

#' Put Receipts
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.put_sales_v1_receipts}
metrc_put_sales_receipt <- function(license_number, sales) {
  
  url <- modify_url(BASE_URL, path = "sales/v1/receipts",
                    query = list(licenseNumber = license_number))
  
  resp <- PUT(url, metrc_auth(), body = sales)
  
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

#' Delete Receipt
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.delete_sales_v1_receipts_{id}}
metrc_delete_sales_receipt <- function(license_number, id) {
  stopifnot(is.integer(id))
  
  url <- modify_url(BASE_URL, path = paste0("sales/v1/receipts/", id),
                    query = list(licenseNumber = license_number))
  
  resp <- DELETE(url, metrc_auth())
  
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

#' Get Sales Transactions
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_transactions}
metrc_get_sales_transactions <- function(license_number) {
  url <- modify_url(BASE_URL, path = "sales/v1/transactions",
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

#' Get Sales Transactions by Date
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.get_sales_v1_transactions_{date}}
metrc_get_sales_transactions_date <- function(license_number, date) {
  date <- format(as.Date(date), "%Y-%m-%d")
  url <- modify_url(BASE_URL, path = paste0("sales/v1/transactions/", date),
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

#' Post Sales Transactions by Date
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.post_sales_v1_transactions_{date}}
metrc_post_sales_transactions_date <- function(license_number, date, package_label,
                                               quantity, unit_of_measure, total_amount) {
  date <- format(as.Date(date), "%Y-%m-%d")
  url <- modify_url(BASE_URL, path = paste0("sales/v1/transactions/", date),
                    query = list(licenseNumber = license_number))
  
  resp <- POST(url, metrc_auth(), body = data.frame(
    PackageLabel = package_label,
    Quantity = quantity, 
    UnitOfMeasure = unit_of_measure, 
    TotalAmount = total_amount
  ))
  
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

#' Put Sales Transactions by Date
#' @export
#' @note See \url{https://api-co.metrc.com/Documentation/#Sales.post_sales_v1_transactions_{date}}
metrc_put_sales_transactions_date <- function(license_number, date, package_label,
                                               quantity, unit_of_measure, total_amount) {
  date <- format(as.Date(date), "%Y-%m-%d")
  url <- modify_url(BASE_URL, path = paste0("sales/v1/transactions/", date),
                    query = list(licenseNumber = license_number))
  
  resp <- PUT(url, metrc_auth(), body = data.frame(
    PackageLabel = package_label,
    Quantity = quantity, 
    UnitOfMeasure = unit_of_measure, 
    TotalAmount = total_amount
  ))
  
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