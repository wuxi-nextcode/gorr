context("test-phenotype-catalog_services.R")

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
    expect_is(result, "phenotype_list")
    expect_equal(length(result), lim)
})

test_that("get_phenotypes_dataframe works", {
    limit <- 3
    results <- get_phenotypes_dataframe(conn = conn, limit = limit, filtered = FALSE)
    results2 <- get_phenotypes_dataframe(pheno_names = test_name, conn = conn)
    expect_is(results, "data.frame")
    expect_equal(nrow(results), limit)
    expect_equal(nrow(results2), 1)
    expect_true(ncol(results) > ncol(results2))
})


test_that("get_phenotype works", {
    result <- get_phenotype(name = test_name, conn = conn)

    expect_is(result, "phenotype")
})
