#' Set up a connection object, and call the health endpoint to make sure everything is up and running on the
#' server side
#'
#' @param api_key the api key from the /api-key-service endpoint of your CSA host through a browser
#' @param project project name
#' @param root_url root_url of the Query API to use, e.g. "http://localhost:1337". If left as NULL the function will try to get it from the environment variable GOR_API_ROOT_URL, otherwise it will take it from the iss part of the decoded JWT (api_key)
#' @param api_endpoint the api endpoint path on the server. If left as NULL the function will try to get it from the environment variable GOR_API_ENDPOINT, otherwise defaults to set up connection to /api/query and /api/phenotype-catalog
#'
#' @return returns a list with the connection data
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @examples
#' \dontrun{
#' api_key <- "...paste from your /api-key-service/token endpoint..."
#' conn <- gor_connect(api_key, "test_project")
#'
#' # or have platform_connect take the api_key from the environment variable GOR_API_KEY:
#' conn <- gor_connect(project = "test_project")
#' }
gor_connect <- function(api_key = NULL, project = NULL, root_url = NULL, api_endpoint = NULL){
    .Deprecated(new = "platform_connect")
    platform_connect(api_key = api_key,
                project = project,
                root_url = root_url,
                api_endpoint = api_endpoint)
}

`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
}


#' @export
print.platform_connection <- function(x, ...) {
    cli::cat_rule(left = crayon::green("GOR API service connection"))
#                  right = paste("Version:", x$build_info$version))
    cli::cat_bullet(crayon::blue("Service Root/s: "), paste(x$service_root, collapse=", "))
#    cli::cat_bullet(crayon::blue("Build Date: "), x$build_info$build_timestamp)
#    cli::cat_bullet(crayon::blue("Commit: "), x$build_info$commit)
    cli::cat_bullet(crayon::blue("Project: "), x$project)
    cli::cat_bullet(crayon::blue("API key issued at: "), as.character(x$api_key_iat %||% "Never"))
    cli::cat_bullet(crayon::blue("API key expires at: "), as.character(x$api_key_exp %||% "Never"))
    cli::cat_bullet(crayon::blue("Access token issued at: "), as.character(x$access_token_iat %||% "Never"))
    cli::cat_bullet(crayon::blue("Access token expires at: "), as.character(x$access_token_exp %||% "Never"))

}


#' Reconnect using a connection object from \code{\link{platform_connect}}
#'
#' @param conn connection object from \code{\link{platform_connect}}
#'
#' @return a new gor connection object (list)
gorr__reconnect <- function(conn) {
    platform_connect(conn$api_key, conn$project)
}



#' Decode a JWT token and extract payload
#'
#' @param jwt_token json web token string
#'
#' @return payload json structure as R structure, created with \code{\link[jsonlite]{fromJSON}}
get_jwt_token_payload <- function(jwt_token) {
    # return NULL for empty or null tokens
    if(is.null(jwt_token) || jwt_token == "") return(NULL)

    tryCatch({
        if (stringr::str_count(jwt_token, "\\.") != 2) {
            stop("Refresh tokens (JWT) should consist of 3 parts separated by dots", call. = F)
        }
        fix_padding <- function(jwt) {
            missing_padding <- stringr::str_length(jwt) %% 4

            if (missing_padding > 0)
                paste(c(jwt, rep("=", 4 - missing_padding)), collapse = "" )
            else
                jwt
        }
        jwt_to_list <- function(jwt) {
            jwt %>%
                fix_padding() %>%
                openssl::base64_decode() %>%
                rawToChar() %>%
                jsonlite::fromJSON()
        }

        parts <- stringr::str_split(jwt_token, "\\.", simplify = T)
        # parts[1] is header, parts[2] is the payload, and parts[3] is signature verification
        jwt_to_list(parts[2])

    },
    error = function(x) gorr__failure("Invalid refresh token", x$message))

}


