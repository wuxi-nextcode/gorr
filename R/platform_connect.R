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
#' conn <- platform_connect(api_key, "test_project")
#'
#' # or have platform_connect take the api_key from the environment variable GOR_API_KEY:
#' conn <- platform_connect(project = "test_project")
#'
#' }
platform_connect <- function(api_key = NULL, project = NULL, root_url = NULL, api_endpoint = NULL) {
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
        api_endpoint <- if (api_endpoint_env == "") c("/api/query","/api/phenotype-catalog", "/queryserver") else api_endpoint_env
    }

    token_payload <- get_jwt_token_payload(api_key)
    expiry_date <- if (is.null(token_payload) || is.null(token_payload$exp)) NULL else lubridate::as_datetime(token_payload$exp)
    if (is.null(root_url)) {
        root_url_env <- Sys.getenv("GOR_API_ROOT_URL")
        root_url <- if (root_url_env == "") token_payload$iss else root_url_env
    }

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
        access_token_payload <- get_jwt_token_payload(header$headers[["Authorization"]])
    }

    payload_date <- function(d) {
        if (is.null(d) || d == 0) NULL else lubridate::as_datetime(d)
    }

    conn_data <- list(
        api_key = api_key,
        service_root = NULL,
        project = project,
        user = access_token_payload$email,
        header = header,
        api_key_exp = payload_date(token_payload$exp),
        api_key_iat = payload_date(token_payload$iat),
        access_token_exp = payload_date(access_token_payload$exp),
        access_token_iat = payload_date(access_token_payload$iat))

    for (endpoint in api_endpoint) {
        service_url_parts <-
            httr::parse_url(root_url)
        service_url_parts$path <- endpoint
        service_root <- httr::build_url(service_url_parts)

        tryCatch({
            response <- gorr__api_request(
                "GET", service_root, conn = conn_data, parse.body = T)

            conn_data[[response$service_name]] <- list(service_root = service_root,
                                                       endpoints = response$endpoints,
                                                       build_info = response$build_info,
                                                       service_name = response$service_name)

            # Create and add 'self' endpoint if not exist
            if (!"self" %in% names(conn_data[[response$service_name]]$endpoints)) {
                conn_data[[response$service_name]]$endpoints$self <- generate_self_link(response)
            }

            if (!assertthat::has_name(response, "endpoints"))
                gorr__failure("Unexpected response from host", "Is this a valid API?")

            conn_data$service_root <- append(conn_data$service_root, service_root)
        },
        error = function(err) {
            gorr__warning("Service Unavailable",
                          detail = paste0("Cannot connect to api-endpoint -> '", endpoint, "' - Skipping"))
            }
        )
    }

    structure(conn_data, class = "platform_connection")
}

generate_self_link <- function(response) {
    root <- response$endpoints$root
    paste0(root, "projects/{project_name}")
}
