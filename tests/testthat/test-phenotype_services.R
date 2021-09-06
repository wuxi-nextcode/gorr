library(dplyr)

context("test-phenotype_services.R")

conn <- NULL
test_name <- NULL
test_phenotype <- NULL

test_that("phenotype_connect works", {
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
    expect_is(conn, "platform_connection")
    expect_true(!is.null(conn$header))
    expect_true(!is.null(conn$header$headers[["Authorization"]]))

})


test_that("phenotype_connect works without parameters", {
    conn <- phenotype_connect()
    expect_is(conn, "platform_connection")
    expect_true(!is.null(conn$header))
    expect_true(!is.null(conn$header$headers[["Authorization"]]))

})

test_that("create_phenotype works", {
    test_name <<- paste0("test_pheno", sample(1:100,1))
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
    expect_is(class(result), "phenotype_list")
    expect_equal(length(result), lim)
})


test_that("get_phenotype works", {
    result <- get_phenotype(name=test_name, conn=conn)

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


test_that("phenotype_get_data works", {
    data <- get_data(test_phenotype, conn) # Expect no error
    expect_equal(names(data), c("pn", test_name))
})

#### Test phenotype tag services ####


test_that("phenotype_add_tag works", {
    test_phenotype <<- phenotype_add_tag(tag="test1,test2", test_phenotype, conn=conn)

    expect_is(test_phenotype, "phenotype")
    expect_equal(length(test_phenotype$tag_list), 2)
})

test_that("phenotype_set_tags works", {
    test_phenotype <<- phenotype_set_tags(tag="test3,test4,test5", test_phenotype, conn=conn)

    expect_is(test_phenotype, "phenotype")
    expect_equal(length(test_phenotype$tag_list), 3)
})

test_that("phenotype_delete_tag works", {
    test_phenotype <<- phenotype_delete_tag(tag="test4", test_phenotype, conn=conn)

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
    expect_error(phenotype_delete(test_phenotype, conn), NA) # Expect no error
})
