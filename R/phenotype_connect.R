#' Set up a connection object, and call the phenotype-catalog endpoint to make sure everything is up and running on the
#' server side
#'
#' This method is depricated and will be removed in future versions
#'
#' @param api_key the api key from the /api-key-service endpoint of your CSA host through a browser
#' @param project project name
#' @param root_url root_url of the Query API to use, e.g. "http://localhost:1337". If left as NULL the function will try to get it from the environment variable GOR_API_ROOT_URL, otherwise it will take it from the iss part of the decoded JWT (api_key)
#'
#' @return returns a list with the connection data
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- "...paste from your /api-key-service/token endpoint..."
#' conn <- phenotype_connect(api_key, "test_project")
#'
#' # or have phenotype_connect take the api_key from the environment variable GOR_API_KEY:
#' conn <- phenotype_connect(project = "test_project")
#'
#' }
phenotype_connect <- function(api_key = NULL, project = NULL, root_url = NULL) {
    .Deprecated(new = "platform_connect")
    platform_connect(api_key = api_key,
                project = project,
                root_url = root_url)
}
