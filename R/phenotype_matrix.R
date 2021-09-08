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
              "phenotype_matrix must include the following properties: ", paste0(req_names, collapse = ", "),
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


# Phenotype matrix class constructor
# A builder to create a phenotype matrix request.
#
# You start by using phemat_add_phenotype() or phemat_add_phenotypes()
# to add a list of phenotypes and then call get_data()
# to retrieve the phenotype matrix from the server.
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
get_phenotype_matrix <- function(base = NULL) {
    phenotype_matrix(base = base, phenotypes = list())
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
                            missing_value=NA,
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
                                                label = if (is.null(label)) name else label)
    phenotype_matrix
}


#' Add multiple phenotypes to a phenotype matrix request.
#'
#' @param names a comma seperated string or character vector of phenotype/s to be added. E.g. "height,weight" or c("height", "weight")
#' @param phenotype_matrix A phenotype matrix structure
#' @param missing_value The string to substitute for missing values in the data
#'
#' @return a phenotype matrix object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' phenotype_mat <- phemat_add_phenotypes(names=c("PT1", "PT2"), phenotype_matrix = phenotype_mat)
#' }
phemat_add_phenotypes <- function(names, phenotype_matrix, missing_value=NA) {
    assertthat::assert_that(is.character(names))

    names <- purrr::map(names, ~base::strsplit(.x, ",", fixed = TRUE)) %>% unlist()

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