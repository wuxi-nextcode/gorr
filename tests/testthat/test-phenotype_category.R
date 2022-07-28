context("test-phenotype_category.R")

test_that("get_categories works", {
    categories <- get_categories(conn)

    expect_vector(categories, ptype = character())
})

test_that("create_category works", {
    name <<- paste0("rpacktestcat", sample(1:1000, 1))
    test_cat <- create_category(name, conn)

    expect_equal(test_cat$name, name)
})

#### Test delete / clean up ####

test_that("category_delete works", {
    expect_error(category_delete(name, conn), NA) # Expect no error
})
