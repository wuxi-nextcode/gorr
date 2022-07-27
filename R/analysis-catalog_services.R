#' A list of all the analysis catalogs in the current project. Optionally scope results to a given phenotype name.
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param phenotype_name optional: Only fetch analysis catalogs that relate to a given phenotype name
#'
#' @return List of analysis catalogs
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' catalogs <- get_analysis_catalogs(conn, limit=5)
#' }
get_analysis_catalogs <- function(conn, phenotype_name = NULL) {
    assertthat::assert_that(class(conn) == "platform_connection")

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
            paste("analysis_catalogs", sep = "/")

    if (!is.null(phenotype_name)) {
        assertthat::assert_that(is.character(phenotype_name))
        url <- gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes") %>%
            paste(phenotype_name, "analysis_catalogs", sep = "/")
    }

    resp <- gorr__api_request("GET",
                              url = url,
                              conn = conn)

    analysis_catalogs <- resp$analysis_catalogs %>%
        purrr::set_names(purrr::map_chr(.,~.x$name)) %>%
        purrr::map(~AnalysisCatalog(.x, conn = conn))

    analysis_catalogs
}


#' Get an analysis catalog in the current project by name.
#'
#' @param analysis_catalog_name The name of the Analysis Catalog
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return analysis_catalog structure
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' catalog <- get_analysis_catalog(analysis_catalog_name="test_ac_1", conn = conn)
#' }
get_analysis_catalog <- function(analysis_catalog_name, conn) {
    assertthat::assert_that(class(conn) == "platform_connection")

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
        paste("analysis_catalogs", analysis_catalog_name, sep = "/")

    resp <- gorr__api_request("GET",
                              url = url,
                              conn = conn)

    AnalysisCatalog(resp$analysis_catalog, conn = conn)
}


#' Create a new analysis catalog in the current project
#'
#' @param playlist_id The id of the playlist that contains the phenotypes to use for analysis
#' @param name Analysis Catalog name
#' @param recipe_name The name of the recipe to use
#' @param recipe_parameters: The parameters required to run the recipe
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param covariate_phenotypes: The names of phenotypes to use as covariates, e.g. ['Pheno1','Pheno2'] (optional)
#' @param name: excluded_pns: the PNs to exclude from the analysis, e.g. ['PN1','PN2'] (optional)
#'
#' @return analysis_catalog structure
create_analysis_catalog  <- function(playlist_id,
                                     name,
                                     recipe_name,
                                     recipe_parameters,
                                     conn,
                                     covariate_phenotypes=NULL,
                                     excluded_pns=NULL){
    assertthat::assert_that(class(conn) == "platform_connection")
    assertthat::assert_that(is.null(covariate_phenotypes) || is.list(covariate_phenotypes))
    assertthat::assert_that(is.null(excluded_pns) || is.list(excluded_pns))


    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
            paste("playlists", playlist_id, "analysis_catalogs", sep = "/")

    content = list(
            analysis_catalog = list(
                                    name = name,
                                    recipe_name = recipe_name,
                                    recipe_parameters = recipe_parameters,
                                    covariate_phenotypes = covariate_phenotypes,
                                    excluded_pns = excluded_pns
                                    )
    )

    resp <-  gorr__api_request("POST",
                          url = url,
                          body = content,
                          conn = conn)

    AnalysisCatalog(resp$analysis_catalog, conn=conn)
}


#' A list of all the analysis catalog runs in the current project for a given phenotype.
#'
#' @param phenotype_name Only list analysis catalog runs for a specific phenotype name
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return List of analysis catalog runs
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' catalog <- get_analysis_catalog_runs(phenotype_name = "test_pheno", conn = conn)
#' }
get_analysis_catalog_runs <- function(phenotype_name, conn) {
    assertthat::assert_that(class(conn) == "platform_connection")
    assertthat::is.string(phenotype_name)

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes") %>%
        paste(phenotype_name, "analysis_catalog_runs", sep = "/")

    resp <-  gorr__api_request("GET",
                               url = url,
                               conn = conn)

    AnalysisCatalog(resp$analysis_catalog_runs, conn=conn)

    analysis_catalog_runs <- resp$analysis_catalog_runs %>%
        purrr::set_names(purrr::map_chr(.,~.x$name)) %>%
        purrr::map(~AnalysisCatalogRun(.x, conn = conn))

    analysis_catalog_runs
}


#' Get an analysis catalog run in the current project by name.
#'
#' @param analysis_catalog_name The name of the Analysis Catalog
#' @param analysis_catalog_run_name The name of the Analysis Catalog run
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return analysis_catalog_run structure
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' catalog <- get_analysis_catalog_run(analysis_catalog_name="test_ac_1", analysis_catalog_run_name = "test_ac_run_1", conn = conn)
#' }
get_analysis_catalog_run <- function(analysis_catalog_name, analysis_catalog_run_name, conn) {
    assertthat::assert_that(class(conn) == "platform_connection")
    assertthat::is.string(analysis_catalog_name)
    assertthat::is.string(analysis_catalog_run_name)


    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
        paste("analysis_catalogs", analysis_catalog_name, "runs",
              analysis_catalog_run_name, sep = "/")

    resp <-  gorr__api_request("GET",
                               url = url,
                               conn = conn)

    AnalysisCatalogRun(resp$analysis_catalog_run, conn=conn)
}

