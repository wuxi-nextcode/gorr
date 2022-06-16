library(dplyr)

context("test-phenotype_get_data.R")

initialize_phemat_tests <- function() {
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
    test_name <<- paste0("test_pheno", sample(1:100, 1))
    test_name2 <<- paste0("test_pheno", sample(1:100, 1))
    test_type <- "CATEGORY"
    phenotype <<- create_phenotype(name = test_name,
                                   result_type = test_type,
                                   conn = conn)
    phenotype2 <<- create_phenotype(name = test_name2,
                                   result_type = test_type,
                                   conn = conn)

    test_data <- list(list("20001", "obese"), list("20002", "lean"))
    test_data2 <- list(list("20001", 1), list("20002", 2))
    phenotype_upload_data(data = test_data, phenotype, conn)
    phenotype_upload_data(data = test_data2, phenotype2, conn)

    phenotype_mat<- get_phenotype_matrix()
    phenotype_mat<<- phemat_add_phenotypes(c(test_name, test_name2), phenotype_mat, missing_value = "-99")

    phenotype_pl <<- create_playlist(name= paste0("test_pl", sample(1:100, 1)), conn = conn, phenotypes = c(test_name, test_name2))

}


# Upload data

phenotype <- NULL
phenotype2 <- NULL
phenotype_mat <- NULL
initialize_phemat_tests()

test_that("get_data works for phenotype", {
    result <- get_data(phenotype)

    expect_equal(names(result), c("pn", test_name))
    expect_equal(dim(result), c(2, 2))

})

test_that("get_data works for phenotype_matrix", {
    result <- get_data(phenotype_mat, conn = conn)

    expect_equal(names(result), c("pn", test_name, test_name2))
    expect_equal(dim(result), c(2, 3))

})

test_that("get_data works for phenotype_playlist", {
    result <- get_data(phenotype_pl)

    phenos <- names(phenotype_pl$phenotypes)
    cols <- colnames(result)
    expect_true(all(colnames(result) %in% c("pn", test_name, test_name2)))
    expect_equal(dim(result), c(2, 3))

})

test_that("get_phenotypes_data", {
    result <- get_phenotypes_data(pheno_names = test_name, conn = conn)

    expect_equal(names(result), c("pn", test_name))
    expect_equal(dim(result), c(2, 2))

})

clean_up_tests <- function() {
    phenotype_delete(phenotype)
    phenotype_delete(phenotype2)
    playlist_delete(phenotype_pl)
}

clean_up_tests()
