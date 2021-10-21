#' Get an access token from a refresh token
#'
#' @param api_key the api key
#' @param url the full url to the authentication service
#'
#' @return returns http headers, constructed with \code{\link[httr]{add_headers}}
#' @export
get_access_token <- function(api_key, url) {
    assertthat::assert_that(assertthat::is.string(api_key))
    assertthat::assert_that(assertthat::is.string(url))

    token_parts <- get_jwt_token_payload(api_key)

    body <- list(
        client_id = token_parts$azp,
        grant_type = "refresh_token",
        refresh_token = api_key
    )

    tryCatch({
        response <- httr::POST(url, body = body, encode = "form", httr::timeout(30))
    }, error = function(x) { if (grepl("Timeout", x, fixed=TRUE)) gorr__failure(paste("Connection timeout:", url)) else stop(x)
    })

    if (response$status_code != 200) {
        content <- httr::content(response)
        error_message <- if (!is.null(content$error_description))  {
            content$error_description
        } else {
            "Unable to connect to authentication service"
        }

        gorr__failure(error_message,
                      paste("HTTP Status Code", response$status_code, httr::message_for_status(response$status_code)))
    }

    response_content <- httr::content(response)


    if (!is.null(response_content$error_description))
        gorr__failure(response_content$error_description)

    httr::add_headers(Authorization = paste(stringr::str_to_title(response_content$token_type), response_content$access_token, sep = " "))
}




