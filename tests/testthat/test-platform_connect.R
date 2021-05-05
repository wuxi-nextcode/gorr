context("test-platform_connect.R")

test_that("platform_connect works", {
    conn <- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))
    expect_is(conn, "platform_connection")
    expect_true(!is.null(conn$header))
    expect_true(!is.null(conn$header$headers[["Authorization"]]))

})

test_that("platform_connect works without parameters", {
    conn <- platform_connect()
    expect_is(conn, "platform_connection")
    expect_true(!is.null(conn$header))
    expect_true(!is.null(conn$header$headers[["Authorization"]]))

})
