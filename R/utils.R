

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


gorr__get_endpoint <- function(conn, api, endpoint) {
    stringr::str_glue(conn[[api]]$endpoints[[endpoint]], project_name = conn$project)
}


get__link <- function(object, type) {
    object$links[[type]]
}


# Convert tags to character vector of tags
parse__tags <- function(tags) {
    purrr::map(tags, ~base::strsplit(.x, ",", fixed = TRUE)) %>% unlist()
}
