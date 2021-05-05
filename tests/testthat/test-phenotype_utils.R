library(dplyr)

context("test-phenotype_utils.R")

test_that("fetch__from_lst works", {
    input <- list(a=list("help"),
                  b=list(list(name=1, do="help1"),
                         list(name=2, do="help2"),
                         list(name=3, do="help3")),
                  c="hello"
                  )

    expect_is(fetch__from_lst(input, "a"), "list")
    expect_equal(fetch__from_lst(input$b, "name"), c(1,2,3))
    expect_is(fetch__from_lst(input, "c"), "character")

})


test_that("gorr__get_endpoint works", {
    conn <- list(project="test",
                 bla = list(endpoints=list(one = "www.is/{project_name}",
                                           two = "www.com")))
    result <- gorr__get_endpoint(conn, "bla", "one")
    expect_equal(result, "www.is/test")
})



# test_that("get__project works", {
#     conn <- list(project="test",
#                  endpoints=list(one = "www.is/{project_name}",
#                                 two = "www.com"))
#     result <- get__project(conn)
#     expect_equal(result, "test")
# })

test_that("get__link works", {
    phenotype <- list(project="test",
                 links=list(self = "www.is",
                            upload = "www.com"))
    result <- get__link(phenotype, "upload")
    expect_equal(result, "www.com")
})
