

fetch__from_lst <- function(lst, item) {
    assertthat::assert_that(is.list(lst))
    assertthat::assert_that(assertthat::is.string(item))

    if (length(lst) == 0) {
        return(NULL)
    }

    if (!is.null(lst[[item]])) {
        return(get(item, lst))
    } else if (is.list(lst[[1]])) {
        return(sapply(lst, function(x)
            get(item, x)))
    }

}



get__url_from_conn <- function(conn, endpoint) {
    stringr::str_glue(conn$endpoints[[endpoint]], project_name = get__project(conn))
}


get__project <- function(conn) {
    conn$project
}


get__link <- function(phenotype, type) {
    phenotype$links[[type]]
}
