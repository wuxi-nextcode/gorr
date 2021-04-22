library(dplyr)

conn <- NULL

context("test-phenotype_category.R")

init_phenotype_category_tests <- function() {
    conn <<- phenotype_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))

    if (!grepl("platform", conn$service_root, fixed = TRUE)) {
        stop("Tests should only be run on Platform dev - please reset 'GOR_API_KEY'")
    }
}

init_phenotype_category_tests()

test_that("get_categories works", {
    categories <- get_categories(conn)

    expect_vector(categories, ptype = character())
})

test_that("create_category works", {
    name <- paste0("rPacktestCat", sample(1:1000, 1))
    test_cat <- create_category(name, conn)

    expect_equal(test_cat$name, name)
})