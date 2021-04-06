#' Get all tags
#'
#' Get all tags available in the catalog
#'
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return A list of phenotype tags
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- phenotype_connect(api_key, project)
#' tags <- get_tags(conn)
#' }
get_tags <- function(conn) {
    assertthat::assert_that(class(conn) == "gor_connection")

    url <-  get__url_from_conn(conn, "tags")
    resp <- gorr__api_request("GET", url, conn = conn)

    fetch__from_lst(resp$tags, "name")
}
