library(testthat)
library(mockery)

test_that("save_ip_data handles empty input gracefully", {
  
  # Mocking the get_ip_data function to return an empty data frame
  mock_get_ip_data <- mock(data.frame())
  stub(save_ip_data, "get_ip_data", mock_get_ip_data)
  
  # Call the save_ip_data function with empty input
  temp_file <- tempfile()
  save_ip_data(character(0), "mock_api_key", "mock_base_url", temp_file)
  
  # Check if the file was created
  if (length(character(0)) == 0) {
    # If the input is empty, we expect the file not to be created
    expect_false(file.exists(temp_file))
  } else {
    # If there was data, ensure the file was created
    expect_true(file.exists(temp_file))
    
    # Attempt to read the data from the file
    saved_data <- tryCatch({
      read.csv(temp_file)
    }, error = function(e) NULL)
    
    # If saved_data is NULL, we handle it gracefully
    if (is.null(saved_data)) {
      saved_data <- data.frame()  # Assign empty data frame to saved_data to avoid further errors
    }
    
    # Check if the file is empty (should have zero rows and zero columns)
    expect_equal(nrow(saved_data), 0)
    expect_equal(ncol(saved_data), 0)
  }
  
  # Clean up the temporary file
  unlink(temp_file)
})

test_that("save_ip_data saves correct data to CSV", {
  # Mocking the get_ip_data function to return valid data
  mock_ip_data <- data.frame(ip = "8.8.8.8", country_code = "US", country_name = "United States")
  mock_get_ip_data <- mock(mock_ip_data)
  stub(save_ip_data, "get_ip_data", mock_get_ip_data)
  
  # Temporary file to save the data
  temp_file <- tempfile()
  
  # Call the function with a valid IP address
  save_ip_data(c("8.8.8.8"), "mock_api_key", "mock_base_url", temp_file)
  
  # Read the saved data
  saved_data <- read.csv(temp_file)
  
  # Check that the saved data has the expected values
  expect_equal(nrow(saved_data), 1)
  expect_equal(saved_data$ip, "8.8.8.8")
  expect_equal(saved_data$country_code, "US")
  expect_equal(saved_data$country_name, "United States")
  
  # Clean up the temporary file
  unlink(temp_file)
})
