library(dplyr)

context("test-phenotype_services.R")

conn <- NULL
test_name <- NULL
test_phenotype <- NULL


init_phenotype_services_test <- function() {
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
}

init_phenotype_services_test()

test_that("create_phenotype works", {
    test_name <<- paste0("test_pheno", sample(1:1000,1))
    test_type <- "QT"
    test_desc <- "This is a test phenotype"
    test_phenotype <<- create_phenotype(name=test_name,
                                        result_type = test_type,
                                        description=test_desc,
                                        conn=conn)

    expect_is(test_phenotype, "phenotype")
})


test_that("get_phenotypes works", {
    lim <- 10
    result <- get_phenotypes(conn, limit=lim)
    ### ERROR here stems from naming clash - phenotype() function and `phenotype` structure - tobe fixed
    expect_is(result, "phenotype_list")
    expect_equal(length(result), lim)
})

test_that("get_phenotypes_dataframe works", {
    limit <- 3
    results <- get_phenotypes_dataframe(conn = conn, limit = limit, filtered = FALSE)
    results2 <- get_phenotypes_dataframe(pheno_names = test_name, conn = conn)
    #expect_is(class(results), "data.frame")
    expect_equal(nrow(results), limit)
    expect_equal(nrow(results2), 1)
    expect_true(ncol(results) > ncol(results2))
})


test_that("get_phenotype works", {
    result <- get_phenotype(name = test_name, conn = conn)

    expect_is(result, "phenotype")
})


test_that("phenotype_update_description works", {
    input <- "Update this"
    result <- phenotype_update_description(description=input, test_phenotype, conn=conn)

    expect_is(result, "phenotype")
})


test_that("phenotype_upload_data (list of lists) works", {
    input <- list(list("20001", "obese"), list("20002", "lean"))
    expect_error(phenotype_upload_data(data=input, test_phenotype, conn), NA) # Expect no error
})

test_that("phenotype_upload_data (data.frame) works", {
    input <- list(list("20001", "obese"), list("20002", "lean")) %>%
        do.call(rbind, . ) %>%
        as.data.frame()
    expect_error(phenotype_upload_data(data=input, test_phenotype, conn), NA) # Expect no error
})

#### Test phenotype tag services ####

test_that("phenotype_add_tags works", {
    test_phenotype <<- phenotype_add_tags(tag="test1,test2", test_phenotype)

    expect_is(test_phenotype, "phenotype")
    expect_equal(length(test_phenotype$tag_list), 2)
})

test_that("phenotype_set_tags works", {
    test_phenotype <<- phenotype_set_tags(tag="test3,test4,test5", test_phenotype)

    expect_is(test_phenotype, "phenotype")
    expect_equal(length(test_phenotype$tag_list), 3)
})

test_that("phenotype_delete_tags works", {
    test_phenotype <<- phenotype_delete_tags(tag="test4", test_phenotype)

    expect_is(test_phenotype, "phenotype")
    expect_equal(length(test_phenotype$tag_list), 2)
})

test_that("phenotype_get_tags works", {
    result <- phenotype_get_tags(test_phenotype)

    expect_is(result, "character")
    expect_equal(result, c("test3","test5"))

})


#### Test delete / clean up ####


test_that("phenotype_delete works", {
    expect_error(phenotype_delete(test_phenotype), NA) # Expect no error
})
