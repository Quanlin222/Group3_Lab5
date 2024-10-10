library(testthat)
library(jsonlite)
library(downloader)

# Mock data for testing
test_url <- "https://jsonplaceholder.typicode.com/todos/1"
test_filename <- tempfile(fileext = ".json")

test_that("getdata_api successfully downloads and parses JSON data", {
  # Run the function and check the structure of the returned data
  data <- suppressWarnings(getdata_api(test_url, test_filename))
  
  # Check that data is a list
  expect_true(is.list(data))
  
  # Check that the returned data contains specific fields
  expect_true("userId" %in% names(data))
  expect_true("title" %in% names(data))
})

test_that("getdata_api handles incorrect URLs gracefully", {
  bad_url <- "http://invalid.url/data.json"
  
  # Suppress warnings while calling the function
  data <- suppressWarnings(getdata_api(bad_url, test_filename))
  expect_null(data)  # Check if the return value is NULL
})

test_that("getdata_api handles empty responses", {
  empty_url <- "https://jsonplaceholder.typicode.com/posts/0"
  data <- suppressWarnings(getdata_api(empty_url, test_filename))
  
  # Ensure data is NULL or empty
  expect_true(is.null(data) || length(data) == 0)
})
