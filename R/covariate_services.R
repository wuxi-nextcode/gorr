get_paginated_results <- function(method, limit) {
    offset <- 0
    # Loop to fetch the entire results, combining the paginated results
    combined_data <- list()
    while (TRUE) {
        data <- method(offset)
        results <- length(data)
        combined_data <- c(combined_data, data)
        offset <- offset + results

        if (results < limit) {
            break
        }
    }
    combined_data
}


#' Get all covariates in current project.
#'
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#'
#' @return a list of covariates
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' covariates <- get_covariates(conn)
#' }
get_covariates <- function(conn) {
    # Number of covariates to be fetched in each api-call (default: 100)
    limit <- 100

    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "self"), 'covariates', sep = "/")

    do_get <- function(offset = 0) {
        content <- list(limit = limit, offset = offset)
        resp <- gorr__api_request("GET", url = url, conn = conn, query = content)
        resp$covariates
    }

    combined_data <- get_paginated_results(do_get, limit)
    combined_data
}


#' Get a single covariate by its id.
#'
#' @param id Unique covariate id in the project
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#'
#' @return a covariate
#' @export
get_covariate <- function(id, conn) {
    assertthat::assert_that(class(conn) == "platform_connection")

    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "self"), 'covariates', as.character(id), sep = "/")
    resp <- gorr__api_request("GET", url = url, conn = conn)

    resp$covariate
}
