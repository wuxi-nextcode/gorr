# Internal method to be called by `get_phenotypes`, `get_phenotypes_matrix` and `get_phenotypes_dataframe`
# See documentation of those methods for more details
query__phenotypes <- function(conn, all_tags, any_tags, pn_count, categories, limit, states, search, updated_at, result_types, names) {
    assertthat::assert_that(class(conn) == "platform_connection")
    assertthat::assert_that(is.numeric(limit))
    assertthat::assert_that(is.list(all_tags) | is.character(all_tags))
    assertthat::assert_that(is.list(any_tags) | is.character(any_tags))
    assertthat::assert_that(is.null(pn_count) | is.character(pn_count))
    assertthat::assert_that(is.list(categories) | is.character(categories))
    assertthat::assert_that(is.list(states) | is.character(states))
    assertthat::assert_that(is.null(search) | is.character(search))
    assertthat::assert_that(is.null(updated_at) | is.character(updated_at))
    assertthat::assert_that(is.list(result_types) | is.character(result_types))
    assertthat::assert_that(is.list(names) | is.character(names))

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes")

    content <- list(with_all_tags = paste(all_tags, collapse = ","),
                    with_any_tags = paste(any_tags, collapse = ","),
                    pn_count = pn_count,
                    category = paste(categories, collapse = ","),
                    limit = limit,
                    state = paste(states, collapse = ","),
                    search = search,
                    updated_at = updated_at,
                    result_type = paste(result_types, collapse = ","),
                    names = paste(names, collapse = ","))

    resp <- gorr__api_request("GET",
                              url = url,
                              query = content,
                              conn = conn)
    resp$phenotypes
}


# Temporary deprecated argument handler
# msg of form "[arg_name] argument deprecated [- optional custom message]"
deprecated_argument_msg <- function(arg, custom=NULL) {
    deparse(substitute(arg)) %>%
        paste("argument deprecated", if (!is.null(custom)) paste("-", custom))
}
