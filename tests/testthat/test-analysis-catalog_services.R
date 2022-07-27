library(dplyr)

context("test-analysis-catalog_services.R")

conn <- NULL

init_phenotype_services_test <- function() {
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
}

init_phenotype_services_test()

test_that("get_analysis_catalogs works", {
    catalogs <<- get_analysis_catalogs(conn = conn)
    expect_is(catalogs, "list")
    expect_is(catalogs[[1]], "analysis_catalog")
})


test_that("get_analysis_catalog works", {
    ac <<-get_analysis_catalog(conn=conn, names(catalogs)[1])
    expect_is(ac, "analysis_catalog")
    expect_type(ac, "list")
})

test_that("get_analysis_catalog_runs works", {
    phenotype_name <- purrr::pluck(ac,"analysis_catalog_items", 1, "phenotype_name")
    ac_runs <<- get_analysis_catalog_runs(conn=conn, phenotype_name=phenotype_name)
    expect_is(ac_runs, "list")
    expect_is(ac_runs[[1]], "analysis_catalog_run")
})

test_that("get_analysis_catalog_run works", {
    ac_run_name <- purrr::pluck(ac_runs[[1]],"name")
    ac_run <- get_analysis_catalog_run(conn=conn, analysis_catalog_name=names(catalogs)[1], analysis_catalog_run_name=ac_run_name)
    expect_is(ac_run, "analysis_catalog_run")
    expect_type(ac_run, "list")
})


test_that("create_analysis_catalog", {
    skip('Not ready')
})
