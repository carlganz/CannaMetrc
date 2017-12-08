context("Test Oregon Sandbox")
library(dplyr)
library(purrr)
### load metrc info
Sys.setenv(
  "metrc_software_key" = Sys.getenv("metrc_oregon_sandbox_software_key"),
  "metrc_user_key" =  Sys.getenv("metrc_oregon_sandbox_user_key"),
  "metrc_state" = "OR",
  "metrc_demo" = "TRUE"
)

test_that("Environmental vars exist", {
  expect_true(Sys.getenv("metrc_software_key")!="")
  expect_true(Sys.getenv("metrc_user_key")!="")
  expect_true(Sys.getenv("metrc_state")=="OR")
})

test_that("Facilites", {
  facilities <<- metrc_get_facilities() %>%
    mutate_(license = ~map_chr(License, "Number"),
            licenseType = ~map_chr(License, "LicenseType"))
  expect_true(nrow(facilities) > 0)
})

test_that("Rooms", {
  rooms <<-metrc_get_rooms_active(facilities$license[3])
  expect_true(nrow(rooms) > 0)
  room <- metrc_get_room(rooms$Id[1])
  expect_true(nrow(room) == 1)
})

test_that("Units of Measure", {
  units <- metrc_get_units()
  expect_true(length(units) > 0)
})

random_name <- paste0(sample(LETTERS, 10), collapse = "")

test_that("Strains", {
  strains <<- metrc_get_strains_active(facilities$license[4])
  expect_true(nrow(strains) > 0)
  strain <- metrc_get_strain(strains$Id[1])
  expect_true(nrow(strain) == 1)
  expect_true(metrc_post_strains(facilities$license[4], random_name, "None", NA, NA, 100, 0))
  strains <<- metrc_get_strains_active(facilities$license[4])
  expect_true(nrow(strain <- strains[strains$Name == random_name, ]) == 1)
  strain <- strains %>% filter_(~Name == random_name)
  expect_true(strain$IndicaPercentage == 100)
  expect_true(metrc_post_strains_update(facilities$license[4], strain$Id, random_name, "None", NA, NA, 75, 25))
  strain <- metrc_get_strain(strain$Id)
  expect_true(strain$IndicaPercentage == 75)
  expect_true(metrc_delete_strain(facilities$license[4], strain$Id))
  strains <<- metrc_get_strains_active(facilities$license[4])
  expect_true(nrow(strains[strains$Name == random_name, ]) == 0)
})

test_that("Items", {
  
})

test_that("Transfers", {
  incoming <- metrc_get_transfers_incoming(facilities$license[3])
  expect_true(nrow(incoming) > 0)
  outgoing <- metrc_get_transfers_outgoing(facilities$license[3])
  expect_true(nrow(outgoing) > 0)
  expect_true(length(metrc_get_transfers_package_states()) > 0)
  expect_true(nrow(metrc_get_transfers_packages(incoming$Id[1])) > 0)
  expect_true(nrow(metrc_get_transfers_delivery(incoming$Id[1])) > 0)
})

test_that("Packages", {
  
})

test_that("Sales", {
  
})

test_that("Plant Batches", {
  
})

test_that("Plants", {
  
})

test_that("Lab Tests", {
  
})

test_that("Harvests", {
  
})
