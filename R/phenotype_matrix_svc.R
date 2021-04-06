validate_phenotype_matrix <- function(phenotype_matrix) {
    values <- unclass(phenotype_matrix)

    req_names <- c("base", "phenotypes")

    if (!is.list(values)) {
        stop(
            "phenotype_matrix should be a list type object",
            call. = FALSE
            )
        }

    if (!all(req_names %in%  names(values))) {
          stop(
              "phenotype_matrix must include the following properties: ", req_names,
              call. = FALSE
              )
        }

    phenotype_matrix
}

new_phenotype_matrix <- function(base, phenotypes) {
    phenotype_matrix <- list(base = base,
                            phenotypes = phenotypes
                            )
    structure(phenotype_matrix, class = "phenotype_matrix")

}

phenotype_matrix <- function(base, phenotypes = list()) {
    new_phenotype_matrix(base = base, phenotypes = phenotypes) %>%
        validate_phenotype_matrix()
}


#' Get a phenotype matrix object.
#'
#' @param base Optional name of base set
#'
#' @return a phenotype matrix object
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' }
get_phenotype_matrix <- function(base=NULL) {
    phenotype_matrix(base = base, phenotypes=list())
}

#' Add a new phenotype to the matrix request.
#'
#' @param name Phenotype name
#' @param phenotype_matrix A phenotype matrix structure
#' @param missing_value The string to substitute for a missing value in the data
#' @param label Optional label to apply to the phenotype
#'
#' @return a phenotype matrix object
#'
#' @importFrom assertthat is.string
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' phenotype_mat <- phemat_add_phenotype(name="PT1", phenotype_matrix = phenotype_mat)
#' }
phemat_add_phenotype <- function(name,
                            phenotype_matrix,
                            missing_value=NULL,
                            label=NULL) {
    assertthat::assert_that(is.string(name))
    assertthat::assert_that(class(phenotype_matrix) == "phenotype_matrix")

    if (!is.null(missing_value)) {
        assertthat::assert_that(is.string(missing_value))
        }

    if (!is.null(label)) {
        assertthat::assert_that(is.string(label))
        }

    phenotype_matrix$phenotypes[[name]] <- list(name = name,
                                                missing_value = missing_value,
                                                label = label)
    phenotype_matrix
}

#' Add a list of phenotypes to the matrix request.
#'
#' @param names List of phenotype names
#' @param phenotype_matrix A phenotype matrix structure
#' @param missing_value The string to substitute for a missing value in the data
#'
#' @return a phenotype matrix object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' phenotype_mat <- phemat_add_phenotypes(name=list("PT1", "PT2"), phenotype_matrix = phenotype_mat)
#' }
phemat_add_phenotypes <- function(names, phenotype_matrix, missing_value=NULL) {
    assertthat::assert_that(is.character(names))

    for (name in names) {
        phenotype_matrix <- phemat_add_phenotype(name = name,
                                            phenotype_matrix = phenotype_matrix,
                                            missing_value = missing_value)
    }

    phenotype_matrix
}

#' Remove a phenotype from the matrix request.
#'
#' Does not fail if the phenotype is not present.
#'
#' @param name phenotype names
#' @param phenotype_matrix A phenotype matrix structure
#'
#' @return a phenotype matrix object
#'
#' @importFrom assertthat is.string
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' phenotype_mat <- phemat_add_phenotypes(name=list("PT1", "PT2"), phenotype_matrix = phenotype_mat)
#' phenotype_mat <- phemat_remove_phenotype(name= "PT1", phenotype_matrix = phenotype_mat)
#' }
phemat_remove_phenotype <- function(name, phenotype_matrix) {
##    Remove a phenotype from the matrix request.
##
##    Does not fail if the phenotype is not present.
##
##    param name: Phenotype name
    assertthat::assert_that(is.string(name))
    assertthat::assert_that(class(phenotype_matrix) == "phenotype_matrix")

    phenotype_matrix$phenotypes[[name]] <- NULL
    phenotype_matrix
}


#' Retrieve a phenotype matrix from the server.
#'
#' Can be called after phenotypes have been added to the request. Create it
#' using \code{\link{get_phenotype_matrix}} and add to it
#' using \code{\link{phemat_add_phenotype}} or \code{\link{phemat_add_phenotypes}}
#'
#' @param phenotype_matrix phenotype_matrix structure.
#' @param conn gor connection structure, create it using
#' \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return phenotype matrix tibble from server
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- phenotype_connect(api_key, project)
#' phenotype_mat <- get_phenotype_mat()
#' phenotype_mat <-  phemat_add_phenotypes(...)
#' phenotype_data <- get_data(phenotype_mat, conn)
#' }
get_data <- function(phenotype_matrix, conn) {
## `ServerError` if phenotypes are not found on the server
    assertthat::assert_that(class(phenotype_matrix) == "phenotype_matrix")

    content <- list(
            base =  phenotype_matrix$base,
            phenotypes = unname(phenotype_matrix$phenotypes)
    )

    url <-  get__url_from_conn(conn, "get_phenotype_matrix")


    # Suppress column name warnings
    dup_col_warn <- function(warn) {
        if(any(grepl("Duplicated column names", warn, fixed = TRUE),
               grepl("Unknown or uninitialised column", warn, fixed = TRUE))) {
            invokeRestart("muffleWarning")
        }
    }

    resp <-  gorr__api_request("POST", url = url, body = content, conn = conn) %>%
        withCallingHandlers(., warning = dup_col_warn) %>%
        suppressMessages()

    names(resp)[-1] <- names(phenotype_matrix$phenotypes)

    resp
}
