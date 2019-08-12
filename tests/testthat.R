library(testthat)
library(assertthat)
library(gorr)

if (!all(c("GORR_API_KEY", "GORR_API_PROJECT") %in% names(Sys.getenv())))
    stop("GORR_API_KEY and GORR_API_PROJECT environment variables have not been set")


test_check("gorr")
