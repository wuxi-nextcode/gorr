#' Set up a connection object, and call the health endpoint to make sure everything is up and running on the
#' server side
#'
#' @param api_key the api key from the /api-key-service endpoint of your CSA host through a browser
#' @param project project name
#' @param api_endpoint the api endpoint path on the server, defaults to /api/query
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
#' }
gor_connect <- function(api_key, project, api_endpoint = "/api/query") {
    assert_that(is.string(api_key))

    token_payload <- get_jwt_token_payload(api_key)
    expiry_date <- lubridate::as_datetime(token_payload$exp)

    service_url_parts <-
        token_payload %$%
        httr::parse_url(iss) %$%
        list(scheme = scheme, hostname = hostname, path = path)

    service_root <- with(service_url_parts, stringr::str_glue("{scheme}://{hostname}{api_endpoint}"))

    if (token_payload$exp > 0 && expiry_date <= lubridate::now()) {
        gorr__failure("Authentication error", paste(
            "API key expired at",
            as.character(expiry_date),
            if (token_payload$azp == "api-key-client")
                with(service_url_parts, stringr::str_glue("\n     Please get a new one at: {scheme}://{hostname}/api-key-service/token"))
        ))
    }



    header <- get_access_token(
        api_key,
        with(service_url_parts,
             stringr::str_glue("{scheme}://{hostname}/{path}/protocol/openid-connect/token")))

    access_token_payload <- get_jwt_token_payload(header$headers[["authorization"]])

    payload_date <- function(d) if (d == 0) "Never" else lubridate::as_datetime(d)
    conn_data <- list(
        api_key = api_key,
        service_root = service_root,
        project = project,
        header = header,
        api_key_exp = payload_date(token_payload$exp),
        api_key_iat = payload_date(token_payload$iat),
        access_token_exp = payload_date(access_token_payload$exp),
        access_token_iat = payload_date(access_token_payload$iat))



    response <-
        conn_data$service_root %>%
        httr::GET() %>%
        httr::content()

    if (!assertthat::has_name(response, "endpoints"))
        gorr__failure(response)

    conn_data$endpoints <- response$endpoints
    conn_data$build_info <- response$build_info
    conn_data$service_name <- response$service_name
    structure(conn_data, class = "gor_connection")
}


#' @export
print.gor_connection <- function(x, ...) {
    cli::cat_rule(left = crayon::green("GOR query service connection"),
                  right = paste("Version:", x$build_info$version))
    cli::cat_bullet(crayon::blue("Service Root: "), x$service_root)
    cli::cat_bullet(crayon::blue("Build Date: "), x$build_info$build_timestamp)
    cli::cat_bullet(crayon::blue("Commit: "), x$build_info$commit)
    cli::cat_bullet(crayon::blue("Project: "), x$project)
    cli::cat_bullet(crayon::blue("API key issued at: "), as.character(x$api_key_iat))
    cli::cat_bullet(crayon::blue("API key expires at: "), as.character(x$api_key_exp))
    cli::cat_bullet(crayon::blue("Access token issued at: "), as.character(x$access_token_iat))
    cli::cat_bullet(crayon::blue("Access token expires at: "), as.character(x$access_token_exp))

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
    if (stringr::str_count(jwt_token, "\\.") != 2) {
        gorr__failure(
            "Invalid refresh token",
            "Refresh tokens (JWT) should consist of 3 parts separated by dots")
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
    #header <- jwt_to_list(parts[1])
    #if (header$typ != "JWT")
    #    gorr__failure("Token is not a JWT token", stringr::str_glue("Token header was: {header}"))
    jwt_to_list(parts[2])
}


