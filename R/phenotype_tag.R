#' Get all tags
#'
#' Get all tags available in the catalog
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return A list of phenotype tags
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' tags <- get_tags(conn)
#' }
get_tags <- function(conn) {
    assertthat::assert_that(class(conn) == "platform_connection")

    url <-  gorr__get_endpoint(conn, "phenotype-catalog", "tags")
    resp <- gorr__api_request("GET", url, conn = conn)

    fetch__from_lst(resp$tags, "name")
}
