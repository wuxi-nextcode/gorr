library(dplyr)

conn <- NULL

context("test-phenotype_playlist.R")

if (FALSE) {

init_phenotype_playlist_tests <- function() {
    conn <<- platform_connect(
        api_key = Sys.getenv("GOR_API_KEY"),
        project = Sys.getenv("GOR_API_PROJECT"))

    test_pl_name <<- paste0("rtestpack_pl", sample(1:1000, 1))
    test_pheno_name <<- paste0("rtestpack_pheno", sample(1:1000, 1))

    test_pl <<- create_playlist(test_pl_name, conn)
    test_pheno <<- create_phenotype(test_pheno_name, "set", conn)

    if (!grepl("platform", conn$service_root, fixed = TRUE)) {
        stop("Tests should only be run on Platform dev - please reset 'GOR_API_KEY'")
    }
}

clean_up_tests <- function() {
    playlist_delete(test_pl, conn)
    phenotype_delete(test_pheno, conn)
}

init_phenotype_playlist_tests()

test_that("get_playlists works", {
    playlists <- get_playlists(conn)

    expect_true(test_pl_name %in% playlists)
})


test_that("get_playlist works", {
    playlist <- get_playlist(name = test_pl_name, conn = conn)
    expect_is(playlist, "playlist")
    expect_equal(test_pl_name, playlist$name)
})


test_that("create_playlist works", {
    name <- paste0("rpacktestpl1", sample(1:1000, 1))
    pl <- create_playlist(name, conn)
    expect_is(pl, "playlist")
    expect_equal(pl$name, name)
    # Clean up
    playlist_delete(pl, conn)
})


test_that("playlist_add_phenotype works", {
    pl <- playlist_add_phenotype(test_pheno_name, test_pl, conn)
    expect_is(pl, "playlist")
    expect_true(test_pheno_name %in% fetch__from_lst(pl$phenotypes, "name"))
})

test_that("playlist_delete works", {
    name <- paste0("rpacktestpl2", sample(1:1000, 1))
    pl <- create_playlist(name, conn)
    expect_is(pl, "playlist")
    playlist_delete(pl, conn)
    pls <- get_playlists(conn)
    expect_false(name %in% pls)
})


clean_up_tests()

}
