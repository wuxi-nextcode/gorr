library(testthat)
library(assertthat)
library(dplyr)
library(gorr)

if (!all(c("GOR_API_KEY", "GOR_API_PROJECT") %in% names(Sys.getenv()))) {
    message("GOR_API_KEY and GOR_API_PROJECT environment variables have not been set, skipping API tests")
} else {
     test_check("gorr")
}
