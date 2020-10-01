#' Set up a connection object, and call the health endpoint to make sure everything is up and running on the
#' server side
#'
#' @param api_key the api key from the /api-key-service endpoint of your CSA host through a browser
#' @param project project name
#' @param root_url root_url of the Query API to use, e.g. "http://localhost:1337". If left as NULL the function will try to get it from the environment variable GOR_API_ROOT_URL, otherwise it will take it from the iss part of the decoded JWT (api_key)
#' @param api_endpoint the api endpoint path on the server. If left as NULL the function will try to get it from the environment variable GOR_API_ENDPOINT, otherwise defaults to /api/query
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
#' # or have gor_connect take the api_key from the environment variable GOR_API_KEY:
#' conn <- gor_connect(project = "test_project")
#'
#' }
gor_connect <- function(api_key = NULL, project = NULL, root_url = NULL, api_endpoint = NULL) {
    if (is.null(project)) {
        project <- Sys.getenv("GOR_API_PROJECT")
        if (project == "") gorr__failure("project not provided, the alternative GOR_API_PROJECT environment variable is not set")
    }

    if (is.null(api_key)) {
        api_key <- Sys.getenv("GOR_API_KEY")
        if (api_key == "") {
            if (is.null(root_url)) gorr__failure(
                 "api_key not provided, the alternative GOR_API_KEY environment variable is not set and the optional root_url parameter is not provided")

            message("api_key not provided, the alternative GOR_API_KEY environment variable is not, proceeding without one since root_url is provided")
        }

    }

    if (is.null(api_endpoint)) {
        api_endpoint_env <- Sys.getenv("GOR_API_ENDPOINT")
        api_endpoint <- if (api_endpoint_env == "") "/api/query" else api_endpoint_env
    }

    token_payload <- get_jwt_token_payload(api_key)
    expiry_date <- if (is.null(token_payload) || is.null(token_payload$exp)) NULL else lubridate::as_datetime(token_payload$exp)
    if (is.null(root_url)) {
        root_url_env <- Sys.getenv("GOR_API_ROOT_URL")
        root_url <- if (root_url_env == "") token_payload$iss else root_url_env
    }

    service_url_parts <-
        httr::parse_url(root_url)
    service_url_parts$path <- api_endpoint
    service_root <- httr::build_url(service_url_parts)

    if (!is.null(expiry_date) && (expiry_date <= lubridate::now())) {
        gorr__failure("Authentication error", paste(
            "API key expired at",
            as.character(expiry_date),
            if (token_payload$azp == "api-key-client") {
                key_service_url <- httr::parse_url(token_payload$iss)
                key_service_url$path <- "api-key-service/token"
                key_service_url <- httr::build_url(key_service_url)
                stringr::str_glue("\n     Please get a new one at: {key_service_url}")
            }
        ))
    }


    header <- NULL
    access_token_payload <- NULL
    if (!is.null(token_payload)) {
        url_parts <- httr::parse_url(token_payload$iss)
        url_parts$path <- paste0(url_parts$path, "/protocol/openid-connect/token")
        url <- httr::build_url(url_parts)
        header <- get_access_token(api_key, url)
        access_token_payload <- get_jwt_token_payload(header$headers[["authorization"]])
    }

    payload_date <- function(d) {
        if (is.null(d) || d == 0) NULL else lubridate::as_datetime(d)
    }

    conn_data <- list(
        api_key = api_key,
        service_root = service_root,
        project = project,
        header = header,
        api_key_exp = payload_date(token_payload$exp),
        api_key_iat = payload_date(token_payload$iat),
        access_token_exp = payload_date(access_token_payload$exp),
        access_token_iat = payload_date(access_token_payload$iat))


    response <- gorr__api_request(
        "GET", conn_data$service_root, conn = conn_data, parse.body = T)


    if (!assertthat::has_name(response, "endpoints"))
        gorr__failure("Unexpected response from host", "Is this a GOR Query API?")

    conn_data$endpoints <- response$endpoints
    conn_data$build_info <- response$build_info
    conn_data$service_name <- response$service_name
    structure(conn_data, class = "gor_connection")
}


`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) rhs else lhs
}


#' @export
print.gor_connection <- function(x, ...) {
    cli::cat_rule(left = crayon::green("GOR query service connection"),
                  right = paste("Version:", x$build_info$version))
    cli::cat_bullet(crayon::blue("Service Root: "), x$service_root)
    cli::cat_bullet(crayon::blue("Build Date: "), x$build_info$build_timestamp)
    cli::cat_bullet(crayon::blue("Commit: "), x$build_info$commit)
    cli::cat_bullet(crayon::blue("Project: "), x$project)
    cli::cat_bullet(crayon::blue("API key issued at: "), as.character(x$api_key_iat %||% "Never"))
    cli::cat_bullet(crayon::blue("API key expires at: "), as.character(x$api_key_exp %||% "Never"))
    cli::cat_bullet(crayon::blue("Access token issued at: "), as.character(x$access_token_iat %||% "Never"))
    cli::cat_bullet(crayon::blue("Access token expires at: "), as.character(x$access_token_exp %||% "Never"))

}


#' Reconnect using a connection object from \code{\link{gor_connect}}
#'
#' @param conn connection object from \code{\link{gor_connect}}
#'
#' @return a new gor connection object (list)
gorr__reconnect <- function(conn) {
    gor_connect(conn$api_key, conn$project)
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


