library(downloader)
library(jsonlite)

#' Download and Parse JSON Data from a URL
#'
#' This function downloads JSON data from a specified URL and saves it to a file.
#' It then reads the JSON content from the file and parses it into an R object.
#'
#' @param url A character string specifying the URL from which to download the data.
#' @param filename A character string specifying the name of the file to save the downloaded data.
#'
#' @return An R object containing the parsed JSON data.
#'
#' @details The function uses the `downloader` package to handle the data download process and the `jsonlite` package to parse the JSON content. The downloaded JSON is read from the specified file and converted into an R object using `fromJSON`.
#'
#' @examples
#' \dontrun{
#' url <- "http://example.com/data.json"
#' filename <- tempfile(fileext = ".json")
#' data <- getdata_api(url, filename)
#' print(data)
#' }
#'
#' @import downloader
#' @import jsonlite
#' @export
getdata_api <- function(url, filename) {
  tryCatch({
    # Attempt to download the data
    download(url, filename, mode = "w", quiet = TRUE)
    
    # Check if file is empty or not downloaded correctly
    if (file.info(filename)$size == 0) {
      message("Downloaded file is empty or invalid.")
      return(NULL)
    }
    
    # Read JSON data from the text file
    json_content <- readLines(filename, warn = FALSE)
    
    # Check if the content is empty before parsing
    if (length(json_content) == 0) {
      message("No data in the downloaded file.")
      return(NULL)
    }
    
    # Parse the JSON string into an R object
    data <- fromJSON(paste(json_content, collapse = ""))
    
    return(data)
  }, error = function(e) {
    message("Error in downloading or parsing the data: ", e$message)
    return(NULL)
  }, warning = function(w) {
    message("Warning: ", w$message)
    return(NULL)  # Return NULL for warnings too
  })
}
