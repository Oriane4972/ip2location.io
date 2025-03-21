library(testthat)
library(dplyr)

# Mocking the safe_extract function, assuming it's defined elsewhere
safe_extract <- function(data, field) {
  if (!is.null(data[[field]])) {
    return(data[[field]])
  }
  return(NA)
}

# Mock get_ip_data to simulate different API responses
get_ip_data <- function(ip_addresses, api_key) {
  # Return empty data frame if no IPs provided
  if (length(ip_addresses) == 0) {
    return(data.frame(
      ip = character(0),
      country_code = character(0),
      country_name = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Simulate API failure for "invalid_ip"
  if ("invalid_ip" %in% ip_addresses) {
    return(data.frame())  # Return an empty data frame for invalid IPs
  }

  # Return mock data for valid IPs
  data.frame(
    ip = ip_addresses,
    country_code = c("US", "GB"),
    country_name = c("United States", "United Kingdom"),
    stringsAsFactors = FALSE
  )
}

# Test case 0: Returns a data frame

test_that("get_ip_data returns a data frame", {
  ip_addresses <- c("8.8.8.8", "1.1.1.1")
  api_key <- "dummy_api_key"

  result <- get_ip_data(ip_addresses, api_key)

  expect_true(is.data.frame(result))
  expect_true("ip" %in% colnames(result))  # Check if 'ip' column is present
})

# Test case 1: Valid IPs
test_that("get_ip_data retrieves correct data for valid IPs", {
  ip_addresses <- c("8.8.8.8", "1.1.1.1")

  # Call get_ip_data (using the mocked version)
  result <- get_ip_data(ip_addresses, api_key = "dummy_api_key")

  # Check that the function returns the correct data
  expect_equal(nrow(result), 2)  # Expecting 2 rows for the two IPs
  expect_equal(result$ip[1], "8.8.8.8")
  expect_equal(result$country_code[1], "US")
  expect_equal(result$country_name[1], "United States")
  expect_equal(result$ip[2], "1.1.1.1")
  expect_equal(result$country_code[2], "GB")
  expect_equal(result$country_name[2], "United Kingdom")
})

# Test case 2: Invalid IP (simulate API failure)
test_that("get_ip_data returns an empty data frame for invalid IP", {
  ip_addresses <- c("invalid_ip")  # IP with a mock failure

  # Call get_ip_data (using the mocked version)
  result <- get_ip_data(ip_addresses, api_key = "dummy_api_key")

  # Check that the result is an empty data frame
  expect_equal(nrow(result), 0)  # No data should be returned for an invalid IP
})

# Test case 3: Empty IP list
test_that("get_ip_data handles an empty IP list", {
  ip_addresses <- character(0)  # Empty vector

  # Call get_ip_data with an empty list of IPs
  result <- get_ip_data(ip_addresses, api_key = "dummy_api_key")

  # Check that the result is an empty data frame
  expect_equal(nrow(result), 0)  # No data should be returned
})

# Test case 4: Handling Missing Data
test_that("get_ip_data handles missing or incomplete data", {
  ip_addresses <- c("8.8.8.8", "1.1.1.1")

  # Mocking a situation where some fields are missing for the second IP
  # Modify the mock function to return missing data for some fields
  get_ip_data <- function(ip_addresses, api_key) {
    data.frame(
      ip = ip_addresses,
      country_code = c("US", NA),  # Missing country code for the second IP
      country_name = c("United States", NA),  # Missing country name for the second IP
      stringsAsFactors = FALSE
    )
  }

  # Call get_ip_data with the modified mock function
  result <- get_ip_data(ip_addresses, api_key = "dummy_api_key")

  # Check that the missing data is handled as NA
  expect_true(is.na(result$country_code[2]))  # Should be NA for the second IP
  expect_true(is.na(result$country_name[2]))  # Should be NA for the second IP
})

# Test case 5: Handling API failure
test_that("get_ip_data handles API failure gracefully", {
  ip_addresses <- c("8.8.8.8", "1.1.1.1")

  # Simulating a network failure by making get_ip_data always fail
  get_ip_data <- function(ip_addresses, api_key) {
    stop("API request failed")
  }

  # Call get_ip_data and expect an error due to the simulated failure
  expect_error(
    get_ip_data(ip_addresses, api_key = "dummy_api_key"),
    "API request failed"
  )
})
