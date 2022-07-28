context("test-phenotype_playlist.R")


init_phenotype_playlist_tests <- function() {
    test_pl_name <<- paste0("rtestpack_pl", sample(1:1000, 1))
    test_pheno_name <<- paste0("rtestpack_pheno", sample(1:1000, 1))
    test_pheno_name2 <<- paste0("rtestpack_pheno", sample(1:1000, 1))

    test_pl <<- create_playlist(test_pl_name, conn)
    test_pheno <<- create_phenotype(test_pheno_name, "set", conn)
    test_pheno2 <<- create_phenotype(test_pheno_name2, "set", conn)
    }


clean_up_tests <- function() {
    playlist_delete(test_pl)
    phenotype_delete(test_pheno)
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

})

test_that("playlist_add_phenotype works", {
    pl <- playlist_add_phenotype(test_pheno_name, test_pl)
    expect_is(pl, "playlist")
    expect_true(test_pheno_name %in% fetch__from_lst(pl$phenotypes, "name"))
})

test_that("playlist_delete_phenotypes works", {
    pl <- playlist_delete_phenotypes(test_pheno_name, test_pl)
    expect_is(pl, "playlist")
    expect_equal(length(pl$phenotypes),0)
})

test_that("playlist_add_phenotypes works", {
    pl <- playlist_add_phenotypes(c(test_pheno_name,test_pheno_name2), test_pl)
    expect_is(pl, "playlist")
    expect_true(all(c(test_pheno_name,test_pheno_name2) %in% names(pl$phenotypes)))
})

test_that("playlist_update_description works", {
    new_description <- "new desc"
    pl <- playlist_update_description(new_description, test_pl)
    expect_is(pl, "playlist")
    expect_true(new_description == pl$description)
})

test_that("playlist_delete works", {
    name <- paste0("rpacktestpl2", sample(1:1000, 1))
    pl <- create_playlist(name, conn)
    expect_is(pl, "playlist")
    playlist_delete(pl)
    pls <- get_playlists(conn)
    expect_false(name %in% pls)
})


clean_up_tests()
