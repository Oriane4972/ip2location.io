library(testthat)

# Test case for safe_extract
test_that("safe_extract extracts values correctly", {
  
  # Create a sample list for testing
  sample_list <- list(
    country = list(name = "USA", code = "US"),
    city = "New York",
    latitude = 40.7128,
    longitude = -74.0060
  )
  
  # Test extracting a valid key (direct access to the list element)
  result <- safe_extract(sample_list, "country")
  expect_equal(result, "USA;US")  # Expecting a string "USA;US" due to collapse
  
  # Test extracting a valid key (non-nested)
  result <- safe_extract(sample_list, "city")
  expect_equal(result, "New York")  # It should extract the value "New York"
  
  # Test extracting a key that doesn't exist (should return NA)
  result <- safe_extract(sample_list, "non_existent_key")
  expect_true(is.na(result))  # It should return NA
  
  # Test edge case: empty list
  empty_list <- list()
  result <- safe_extract(empty_list, "any_key")
  expect_true(is.na(result))  # It should return NA
  
  # Test edge case: NULL list (should not throw an error)
  null_list <- NULL
  result <- safe_extract(null_list, "any_key")
  expect_true(is.na(result))  # It should return NA
})
