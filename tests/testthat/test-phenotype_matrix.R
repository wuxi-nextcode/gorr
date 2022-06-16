library(dplyr)

context("test-phenotype_matrix.R")

initialize_phemat_tests <- function(){
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
    test_name <<- paste0("test_pheno", sample(1:1000,1))
    test_type <- "QT"
    test_desc <- "This is a test phenotype"
    phenotype <<- create_phenotype(name=test_name,
                                   result_type = test_type,
                                   description=test_desc,
                                   conn=conn)
    test_data <- list(list("20001", "obese"), list("20002", "lean"))
    phenotype_upload_data(data=test_data, phenotype, conn)

}

phenotype_matrix <- NULL
initialize_phemat_tests()

test_that("get_phenotype_matrix works", {
    phenotype_matrix <<- get_phenotype_matrix()

    expect_is(phenotype_matrix, "phenotype_matrix")
})


test_that("phemat_add_phenotype and phemat_add_phenotypes works", {
    input <- c(test_name, "dummy")
    phenotype_matrix <<- phemat_add_phenotypes(input, phenotype_matrix, missing_value = "-99")

    expect_is(phenotype_matrix, "phenotype_matrix")
    expect_equal(length(phenotype_matrix$phenotypes), 2)
})


test_that("phemat_remove_phenotype works", {
    phenotype_matrix <<- phemat_remove_phenotype("dummy",phenotype_matrix)

    expect_is(phenotype_matrix, "phenotype_matrix")
    expect_equal(length(phenotype_matrix$phenotypes), 1)
})


test_that("get_data works", {
    result <- get_data(phenotype_matrix, conn=conn)

    expect_equal(names(result), c('pn',test_name))
    expect_equal(dim(result), c(2,2))

})

clean_up_tests <- function() {
    phenotype_delete(phenotype)
}

clean_up_tests()
