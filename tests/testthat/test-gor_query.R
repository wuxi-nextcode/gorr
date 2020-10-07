library(dplyr)

context("test-gor_query.R")

conn <- NULL


test_that("gor_connect works", {
    conn <<- gor_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
    expect_is(conn, "gor_connection")
    expect_true(!is.null(conn$header))
    expect_true(!is.null(conn$header$headers[["authorization"]]))

})


test_that("gor_connect works without parameters", {
    conn <- gor_connect()
    expect_is(conn, "gor_connection")
    expect_true(!is.null(conn$header))
    expect_true(!is.null(conn$header$headers[["authorization"]]))

})

test_that("gor_query works", {
    result <-
        "gor #dbsnp# | top 100" %>%
        gor_query(conn)

    expect_is(result, "data.frame")
    cols <- sapply(colnames(result),tolower,USE.NAMES=F) # GOR is not case insensitive so we need to convert to lowercase so the tests won't brake between ref versions
    expect_equal(cols, c("chrom", "pos", "reference", "allele", "rsids"))
    expect_equal(dim(result), c(100,5), info = "Expected dimensions of this dataframe are 100rows x 5 columns")
})



test_that("gor_query paging works", {
    result <-
        "gor #dbsnp# | where chrom='chr1' | top 1000" %>%
        gor_query(conn, page_size = 100)

    expect_is(result, "data.frame")
    cols <- sapply(colnames(result),tolower,USE.NAMES=F) # GOR is not case insensitive so we need to convert to lowercase so the tests won't brake between ref versions
    expect_equal(cols, c("chrom", "pos", "reference", "allele", "rsids"))
    expect_equal(dim(result), c(1000,5), info = "Expected dimensions of this dataframe are 1000rows x 5 columns")

    # Catch e.g. header repititions due to paging in the next 2 steps

    result %>%
        pull(1) %>%
        unique() %>%
        expect_equal("chr1", "We only asked for chr1 in query, so we expect nothing else")

    result$reference %>%
        unique() %>%
        paste(collapse = "") %>%
        strsplit("") %>%
        unlist() %>%
        unique() %>%
        setequal(c("C", "G", "A", "T")) %>%
        expect_true(info = "Unique reference column set values should equal the set GCAT")

    result %>% pull(1)
})


test_that("gor_query tsv text output works (parse = F)", {
    result <-
        "gor #dbsnp# | top 100" %>%
        gor_query(conn, parse = F)

    expect_is(result, "character")
    expect_equal(stringr::str_count(result, "\n"), 101) # should be 101 new lines including the header
})


test_that("long query string works - GORR-28", {
    nor_junk <- paste0("create #", LETTERS, "# = norrows 10 | select 1 | top 100 | distinct | select rownum | calc odd mod(rownum,2) | calc A 1 | calc helloworld 214354678432456 | calc rusl 4235425 | signature -timeres 1", collapse = ";\n")

    result <- "{nor_junk};
    norrows 1000 | select RowNum | signature -timeres 1" %>%
        stringr::str_glue() %>%
        gor_query(conn)

    expect_is(result, "data.frame")
})
