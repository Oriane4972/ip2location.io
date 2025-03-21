# safe_extract.R

#' Safe Data Extraction from API Response
#'
#' This function extracts the value of a given field from the parsed API response.
#' It handles missing or empty data gracefully by returning NA if the field is missing.
#'
#' @param x The parsed JSON response (usually a list).
#' @param field The name of the field to extract (as a string).
#' @return The extracted field value, or NA if missing or empty.
#' @export

safe_extract <- function(x, field) {
  tryCatch({
    # Handle both missing and empty data gracefully
    if (is.null(x[[field]]) || length(x[[field]]) == 0) {
      return(NA)  # Return NA if the field is missing or empty
    } else if (length(x[[field]]) > 1) {
      return(paste(x[[field]], collapse = ";"))  # Join multiple values
    } else {
      return(x[[field]])  # Return the field value
    }
  }, error = function(e) {
    # Return NA in case of any error while extracting
    return(NA)
  })
}