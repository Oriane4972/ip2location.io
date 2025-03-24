# get_ip_data.R

library(httr)

#' Retrieve and Save Data for Multiple IP Addresses
#'
#' This function retrieves information for a list of IP addresses using the 'IP2Location.io' API,
#' processes the data, and returns the data as a dataframe. The function also handles missing
#' values by assigning NAs for any data field not provided by the user's API plan.
#'
#' @param ip_addresses A vector of IP addresses.
#' @param api_key Your 'IP2Location.io' API key in quotation marks.
#' @return A data frame with the extracted data for each IP.
#' @export
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows `%>%` group_by slice ungroup select
#' @importFrom tidyselect all_of
#'
#' @note This function uses the 'IP2Location.io' API. Make sure you have a valid API key.
#' \url{https://www.ip2location.io/pricing}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' ip_addresses <- c("8.8.8.8", "1.1.1.1")         # Example IP addresses
#' api_key <- "your_api_key_here"                  # Replace with your API key
#' ip_data <- get_ip_data(ip_addresses, api_key)   # Returns a dataframe
#' # If the user wants to save the dataframe as a CSV, they can do so:
#' write.csv(ip_data, "IP2location.csv", row.names = FALSE)
#' }
#'
#' @details
#' The function extracts the following fields (some of which may contain NAs
#' depending on the user's API plan) for each IP address:
#' \tabular{ll}{
#'   \strong{Field} \tab \strong{Description} \cr
#'   ip \tab IP address \cr
#'   country_code \tab Country code \cr
#'   country_name \tab Country name \cr
#'   region_name \tab Region name \cr
#'   district \tab District \cr
#'   city_name \tab City name \cr
#'   latitude \tab Latitude \cr
#'   longitude \tab Longitude \cr
#'   zip_code \tab Zip code \cr
#'   time_zone \tab Time zone \cr
#'   asn \tab Autonomous system number \cr
#'   as \tab Autonomous system \cr
#'   isp \tab Internet service provider \cr
#'   domain \tab Domain \cr
#'   net_speed \tab Network speed \cr
#'   idd_code \tab International dialing code \cr
#'   area_code \tab Area code \cr
#'   weather_station_code \tab Weather station code \cr
#'   weather_station_name \tab Weather station name \cr
#'   mcc \tab Mobile country code \cr
#'   mnc \tab Mobile network code \cr
#'   mobile_brand \tab Mobile brand \cr
#'   elevation \tab Elevation \cr
#'   usage_type \tab Usage type \cr
#'   address_type \tab Address type \cr
#'   ads_category \tab Ads category \cr
#'   ads_category_name \tab Ads category name \cr
#'   continent_name \tab Continent name \cr
#'   continent_hemisphere \tab Continent hemisphere \cr
#'   country_capital \tab Country capital \cr
#'   country_language \tab Country language \cr
#'   region_code \tab Region code \cr
#'   time_zone_olson \tab Time zone (Olson format) \cr
#'   time_zone_current_time \tab Current time in the time zone \cr
#'   is_proxy \tab Whether the IP is a proxy (limited to public proxies in the Free and Starter plans) \cr
#'   fraud_score \tab Fraud score \cr
#'   proxy_last_seen \tab Last seen time of the proxy \cr
#'   proxy_type \tab Proxy type \cr
#'   proxy_threat \tab Proxy threat \cr
#'   proxy_provider \tab Proxy provider \cr
#'   proxy_is_vpn \tab Whether the proxy is a VPN \cr
#'   proxy_is_tor \tab Whether the proxy is Tor \cr
#'   proxy_is_data_center \tab Whether the proxy is a data center \cr
#'   proxy_is_public_proxy \tab Whether the proxy is a public proxy \cr
#'   proxy_is_web_proxy \tab Whether the proxy is a web proxy \cr
#'   proxy_is_web_crawler \tab Whether the proxy is a web crawler \cr
#'   proxy_is_residential_proxy \tab Whether the proxy is a residential proxy \cr
#'   proxy_is_consumer_privacy_network \tab Whether the proxy is a consumer privacy network \cr
#'   proxy_is_enterprise_private_network \tab Whether the proxy is an enterprise private network \cr
#'   proxy_is_spammer \tab Whether the proxy is a spammer \cr
#'   proxy_is_scanner \tab Whether the proxy is a scanner \cr
#'   proxy_is_botnet \tab Whether the proxy is a botnet \cr
#' }

get_ip_data <- function(ip_addresses, api_key) {

  base_url <- "https://api.ip2location.io"

  # Initialize an empty list to collect all the data for each IP
  all_data <- list()

  for (ip in ip_addresses) {

    # Construct the API request URL with parameters
    url <- paste0(base_url, "?key=", api_key, "&ip=", ip, "&format=json")

    # Send the GET request to the API
    response <- GET(url)

    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the JSON response content
      response_content <- content(response, "text")
      parsed_response <- fromJSON(response_content)

      # Check if the response is valid (not empty)
      if (length(parsed_response) == 0) {
        message("No data returned for IP:", ip, "\n")
        next  # Skip to the next IP if no data is returned
      }

      # Extracting data for each IP
      ip_data <- data.frame(
        ip = safe_extract(parsed_response, "ip"),
        country_code = safe_extract(parsed_response, "country_code"),
        country_name = safe_extract(parsed_response, "country_name"),
        region_name = safe_extract(parsed_response, "region_name"),
        district = safe_extract(parsed_response, "district"),
        city_name = safe_extract(parsed_response, "city_name"),
        latitude = safe_extract(parsed_response, "latitude"),
        longitude = safe_extract(parsed_response, "longitude"),
        zip_code = safe_extract(parsed_response, "zip_code"),
        time_zone = safe_extract(parsed_response, "time_zone"),
        asn = safe_extract(parsed_response, "asn"),
        as = safe_extract(parsed_response, "as"),
        isp = safe_extract(parsed_response, "isp"),
        domain = safe_extract(parsed_response, "domain"),
        net_speed = safe_extract(parsed_response, "net_speed"),
        idd_code = safe_extract(parsed_response, "idd_code"),
        area_code = safe_extract(parsed_response, "area_code"),
        weather_station_code = safe_extract(parsed_response, "weather_station_code"),
        weather_station_name = safe_extract(parsed_response, "weather_station_name"),
        mcc = safe_extract(parsed_response, "mcc"),
        mnc = safe_extract(parsed_response, "mnc"),
        mobile_brand = safe_extract(parsed_response, "mobile_brand"),
        elevation = safe_extract(parsed_response, "elevation"),
        usage_type = safe_extract(parsed_response, "usage_type"),
        address_type = safe_extract(parsed_response, "address_type"),
        ads_category = safe_extract(parsed_response, "ads_category"),
        ads_category_name = safe_extract(parsed_response, "ads_category_name"),
        continent_name = safe_extract(parsed_response$continent, "name"),
        continent_hemisphere = safe_extract(parsed_response$continent, "hemisphere"),
        country_capital = safe_extract(parsed_response$country, "capital"),
        country_language = safe_extract(parsed_response$country, "language"),
        region_code = safe_extract(parsed_response$region, "code"),
        time_zone_olson = safe_extract(parsed_response$time_zone_info, "olson"),
        time_zone_current_time = safe_extract(parsed_response$time_zone_info, "current_time"),
        is_proxy = safe_extract(parsed_response, "is_proxy"),
        fraud_score = safe_extract(parsed_response, "fraud_score"),
        proxy_last_seen = safe_extract(parsed_response$proxy, "last_seen"),
        proxy_type = safe_extract(parsed_response$proxy, "proxy_type"),
        proxy_threat = safe_extract(parsed_response$proxy, "threat"),
        proxy_provider = safe_extract(parsed_response$proxy, "provider"),
        proxy_is_vpn = safe_extract(parsed_response$proxy, "is_vpn"),
        proxy_is_tor = safe_extract(parsed_response$proxy, "is_tor"),
        proxy_is_data_center = safe_extract(parsed_response$proxy, "is_data_center"),
        proxy_is_public_proxy = safe_extract(parsed_response$proxy, "is_public_proxy"),
        proxy_is_web_proxy = safe_extract(parsed_response$proxy, "is_web_proxy"),
        proxy_is_web_crawler = safe_extract(parsed_response$proxy, "is_web_crawler"),
        proxy_is_residential_proxy = safe_extract(parsed_response$proxy, "is_residential_proxy"),
        proxy_is_consumer_privacy_network = safe_extract(parsed_response$proxy, "is_consumer_privacy_network"),
        proxy_is_enterprise_private_network = safe_extract(parsed_response$proxy, "is_enterprise_private_network"),
        proxy_is_spammer = safe_extract(parsed_response$proxy, "is_spammer"),
        proxy_is_scanner = safe_extract(parsed_response$proxy, "is_scanner"),
        proxy_is_botnet = safe_extract(parsed_response$proxy, "is_botnet"),
        stringsAsFactors = FALSE
      )

      # Store data with IP as the unique key
      all_data[[ip]] <- ip_data

    } else {
      # Handle request failure by printing an error message for the IP address
      message("Request failed for IP:", ip, "with status code:", status_code(response), "\n")
    }
  }

  # After collecting the data (inside the for loop), combine all data into a single data frame
  response_data <- bind_rows(all_data)

  # Remove duplicates based on the IP column
  response_data_cleaned <- response_data %>%
    group_by(ip) %>%
    slice(1) %>%
    ungroup()

  # Identify non-NA columns and NA columns
  non_na_columns <- colnames(response_data_cleaned)[!apply(response_data_cleaned, 2, function(x) all(is.na(x)))]
  na_columns <- setdiff(colnames(response_data_cleaned), non_na_columns)

  # Combine non-NA columns first, followed by NA columns
  ip_data_ordered <- response_data_cleaned %>%
    select(all_of(non_na_columns), all_of(na_columns)) # Use all_of() for external vectors

  # Return the processed dataframe
  return(ip_data_ordered)

}
