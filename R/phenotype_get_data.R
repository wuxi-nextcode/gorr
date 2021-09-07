#' Retrieve phenotype data or phenotype matrix data from the server.
#'
#' If input is phenotype matrix object then phenotypes need te have been added to the phenotype matrix object. Create it
#' using \code{\link{get_phenotype_matrix}} and add to it
#' using \code{\link{phemat_add_phenotype}} or \code{\link{phemat_add_phenotypes}}
#'
#' @param pheno_obj phenotype names, phenotype, phenotype matrix or phenotype playlist structure.
#' @param ... additional arguments
#'
#'
#' @return tibble from server
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotype_mat <- get_phenotype_matrix()
#' phenotype_mat <-  phemat_add_phenotypes(...)
#' phenotype_data <- get_data(phenotype_mat, conn)
#' }
get_data <- function(pheno_obj, ...) {
    UseMethod("get_data", pheno_obj)
}

#' @describeIn get_data Get phenotype data
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#' @export
get_data.phenotype <- function(pheno_obj, conn = NULL) {
    if (!missing(conn)) {
        warning("phenotype structure provided - conn argument ignored")
    }

    content <- list(
            base = NULL,
            phenotypes = list(list(name = pheno_obj$name))
    )

    query__phemat_data(conn = attr(pheno_obj, which = "conn"), content = content)

}

#' @describeIn get_data Get data for phenotypes in phenotype matrix
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#' @export
get_data.phenotype_matrix <- function(pheno_obj, conn) {
    conn_obj <- attr(pheno_obj, which = "conn") %||% conn
    if (is.null(conn_obj)) {
        stop("platform connector object missing - please provide valid conn input argument ")
    } else if (!is.null(attr(pheno_obj, which = "conn")) & !missing(conn)) {
        warning("phenotype_matrix structure with platform connector object provided - conn argument ignored")
    }

    content <- list(
            base = pheno_obj$base,
            phenotypes = unname(pheno_obj$phenotypes)
    )

    query__phemat_data(conn = conn_obj, content = content)
}


#' @describeIn get_data Get data phenotypes in phenotype playlist
#' @param missing_value The string to substitute for a missing value in the data
#' @param base Optional name of base set
#' @export
get_data.playlist <- function(pheno_obj, missing_value = NULL, base = NULL) {
    content <- list(base = base,
                    phenotypes = purrr::map(names(pheno_obj$phenotypes), ~list(name = .x, missing_value = missing_value))
                    )
    query__phemat_data(conn = attr(pheno_obj, which = "conn"), content = content)
}


#' @describeIn get_data Get data phenotype/s by phenotype names
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#' @param missing_value The string to substitute for a missing value in the data
#' @param base Optional name of base set
#' @export
get_data.default <- function(pheno_obj, conn, missing_value = NULL, base = NULL) {
    assertthat::assert_that(is.list(pheno_obj) | is.character(pheno_obj))

    pheno_obj <- purrr::map(pheno_obj, ~base::strsplit(.x, ",", fixed = TRUE)) %>% unlist()

    content <- list(base = base,
                phenotypes = purrr::map(pheno_obj, ~list(name = .x, missing_value = missing_value))
    )
    query__phemat_data(conn = conn, content = content)
}


#' Retrieve phenotype data from the server.
#'
#' @param pheno_names Fetch phenotypes that are in a given list of phenotype names, character and character vector also allowed
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#' @param missing_value The string to substitute for a missing value in the data
#' @param base Optional name of base set
#'
#' @return tibble from server
#' @export
get_phenotypes_data <- function(pheno_names, conn, missing_value = NULL, base = NULL) {
    assertthat::assert_that(is.list(pheno_names) | is.character(pheno_names))
    assertthat::assert_that(is.list(conn) | is.character("platform_connection"))
    get_data(as.list(pheno_names), conn = conn, missing_value = missing_value, base = base)
}


query__phemat_data <- function(conn, content) {
    url <-  gorr__get_endpoint(conn, "phenotype-catalog", "get_phenotype_matrix")

    withCallingHandlers({
        resp <- gorr__api_request("POST", url = url, body = content, conn = conn)
    }, warning = function(w) {
        if (startsWith(conditionMessage(w), "Unknown or uninitialised column: `status`"))
            invokeRestart("muffleWarning")

    })
    resp
}
