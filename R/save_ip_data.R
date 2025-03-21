# save_ip_data.R

#' Save IP Data to CSV
#'
#' This function retrieves information for multiple IP addresses using the IP2Location.io API,
#' processes the data, and saves it to a CSV file. The function also handles missing values by assigning NA 
#' for any field not returned by a given API plan version.
#'
#' @param ip_addresses A vector of IP addresses.
#' @param api_key Your IP2Location.io API key.
#' @param base_url The base URL for the IP2Location.io API.
#' @param output_file The file path where to save the CSV (default is "IP2location.csv").
#' @return None (data is written to a CSV).
#' @export
#' @importFrom dplyr bind_rows `%>%` group_by slice ungroup select
#' @importFrom tidyselect all_of
#' @importFrom utils write.csv

save_ip_data <- function(ip_addresses, api_key, base_url, output_file = "IP2location.csv") {
  
  # Get IP data using the get_ip_data function
  ip_data <- get_ip_data(ip_addresses, api_key, base_url)
  
  # Identify non-NA columns and NA columns
  non_na_columns <- colnames(ip_data)[!apply(ip_data, 2, function(x) all(is.na(x)))]
  na_columns <- setdiff(colnames(ip_data), non_na_columns)
  
  # Combine non-NA columns first, followed by NA columns
  ip_data_ordered <- ip_data %>%
    select(all_of(non_na_columns), all_of(na_columns)) # Use all_of() for external vectors
  
  # Write the ordered data to a CSV file
  if (nrow(ip_data_ordered) > 0) {
    write.csv(ip_data_ordered, output_file, row.names = FALSE)
    cat("Data saved to", output_file, "\n")
    } else {
      cat("No data to write to CSV.\n")
    }
}
