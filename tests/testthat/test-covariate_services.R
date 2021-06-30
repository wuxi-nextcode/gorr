library(dplyr)

context("test-covariate_services.R")

conn <- NULL
init_covariate_services_tests <- function() {
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
}

init_covariate_services_tests()

test_that("get_covariates works", {
    result <- get_covariates(conn)

    expect_is(result, "list")
})

test_that("get_covariate works", {
    # IF THIS FAILS TRY DIFFERENT ID (MAKE SURE IT EXISTS IN PROJECT)
    result <- get_covariate(id = 2, conn = conn)

    expect_is(result, "list")
})
