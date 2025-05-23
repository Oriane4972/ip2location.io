% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ip_data.R
\name{get_ip_data}
\alias{get_ip_data}
\title{Retrieve and Save Data for Multiple IP Addresses}
\usage{
get_ip_data(ip_addresses, api_key)
}
\arguments{
\item{ip_addresses}{A vector of IP addresses.}

\item{api_key}{Your IP2Location.io API key in quotation marks.}
}
\value{
A data frame with the extracted data for each IP.
}
\description{
This function retrieves information for a list of IP addresses using the IP2Location.io API,
processes the data, and returns the data as a dataframe. The function also handles missing
values by assigning NAs for any data field not provided by the user's API plan.
}
\details{
The function extracts the following fields (some of which may contain NAs
depending on the user's API plan) for each IP address:
\tabular{ll}{
\strong{Field} \tab \strong{Description} \cr
ip \tab IP address \cr
country_code \tab Country code \cr
country_name \tab Country name \cr
region_name \tab Region name \cr
district \tab District \cr
city_name \tab City name \cr
latitude \tab Latitude \cr
longitude \tab Longitude \cr
zip_code \tab Zip code \cr
time_zone \tab Time zone \cr
asn \tab Autonomous system number \cr
as \tab Autonomous system \cr
isp \tab Internet service provider \cr
domain \tab Domain \cr
net_speed \tab Network speed \cr
idd_code \tab International dialing code \cr
area_code \tab Area code \cr
weather_station_code \tab Weather station code \cr
weather_station_name \tab Weather station name \cr
mcc \tab Mobile country code \cr
mnc \tab Mobile network code \cr
mobile_brand \tab Mobile brand \cr
elevation \tab Elevation \cr
usage_type \tab Usage type \cr
address_type \tab Address type \cr
ads_category \tab Ads category \cr
ads_category_name \tab Ads category name \cr
continent_name \tab Continent name \cr
continent_hemisphere \tab Continent hemisphere \cr
country_capital \tab Country capital \cr
country_language \tab Country language \cr
region_code \tab Region code \cr
time_zone_olson \tab Time zone (Olson format) \cr
time_zone_current_time \tab Current time in the time zone \cr
is_proxy \tab Whether the IP is a proxy (limited to public proxies in the Free and Starter plans) \cr
fraud_score \tab Fraud score \cr
proxy_last_seen \tab Last seen time of the proxy \cr
proxy_type \tab Proxy type \cr
proxy_threat \tab Proxy threat \cr
proxy_provider \tab Proxy provider \cr
proxy_is_vpn \tab Whether the proxy is a VPN \cr
proxy_is_tor \tab Whether the proxy is Tor \cr
proxy_is_data_center \tab Whether the proxy is a data center \cr
proxy_is_public_proxy \tab Whether the proxy is a public proxy \cr
proxy_is_web_proxy \tab Whether the proxy is a web proxy \cr
proxy_is_web_crawler \tab Whether the proxy is a web crawler \cr
proxy_is_residential_proxy \tab Whether the proxy is a residential proxy \cr
proxy_is_consumer_privacy_network \tab Whether the proxy is a consumer privacy network \cr
proxy_is_enterprise_private_network \tab Whether the proxy is an enterprise private network \cr
proxy_is_spammer \tab Whether the proxy is a spammer \cr
proxy_is_scanner \tab Whether the proxy is a scanner \cr
proxy_is_botnet \tab Whether the proxy is a botnet \cr
}
}
\note{
This function uses the IP2Location.io API. Make sure you have a valid API key.
\url{https://www.ip2location.io/pricing}
}
\examples{
\dontrun{
# Example usage
ip_addresses <- c("8.8.8.8", "1.1.1.1")         # Example IP addresses
api_key <- "your_api_key_here"                  # Replace with your API key
ip_data <- get_ip_data(ip_addresses, api_key)   # Returns a dataframe
# If the user wants to save the dataframe as a CSV, they can do so:
write.csv(ip_data, "IP2location.csv", row.names = FALSE)
}

}
