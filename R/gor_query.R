gorr__api_request <- function(request.fun = c("POST", "GET", "DELETE"),
                              url, conn, body = list(), parse.body = T) {
    request.fun <- match.arg(request.fun)
    request.fun <- switch(request.fun, POST = httr::POST, GET = httr::GET, DELETE = httr::DELETE)

    if (conn$access_token_exp - lubridate::now() < lubridate::minutes(1)) {
        conn <- gorr__reconnect(conn)
    }
    response <- request.fun(url = url, body = body, conn$header)

    if (parse.body) {
        gorr__get_response_body(response)
    } else {
        response
    }
}


#' Run a GOR query
#'
#' @param query gor query string
#' @param conn gor connection structure, create it using \code{\link{gor_connect}}
#' @param timeout timeout in seconds, default to 0 (none), uses \code{\link[base]{setTimeLimit}} to interrupt, note that setting any limit has a small overhead – well under 1\% on the systems measured.
#' @param page_size large results are returned in paged responses, this parameter controls the page size (e.g. 1000 lines at a time), default is 0 (everything is fetched in one request)
#' @param parse should the TSV output be parsed into a dataframe? False will make the function return the results as text object
#'
#' @return data.frame of gor results, i.e. gor results are passed to \code{\link[readr]{read_tsv}}
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- gor_connect(api_key, project)
#' "gor #dbsnp# | top 100" %>%
#'     gor_query(conn)
#' }
gor_query <- function(query, conn, timeout = 0, page_size = 100e3, parse = T) {
    assertthat::assert_that(is.string(query))
    assertthat::assert_that(class(conn) == "gor_connection")

    if (page_size == 0)
        warning("Setting page_size to 0 could crash the API server for very large results")

    if (timeout > 0)
        setTimeLimit(elapsed = timeout)

    start_time <- lubridate::now()

    spinner <- if (interactive()) gorr__spinner else invisible
    if (interactive()) {
        cli::cat_rule(left = "Executing Query", right = "GOR Query Service", col = "blue")
        cli::cat_line(query, col = "silver")
        cli::cat_rule("", col = "blue")
    }
    spinner("Submitting Query")
    query_response <- gorr__post_query(query, conn)
    if (interactive()) {
        cli::cat_line("")
        cli::cat_line(paste0(" Server assigned query ID ", crayon::green(paste0("#",query_response$id))))
    }

    tryCatch({
        repeat {
            elapsed <- lubridate::now() - start_time
            Sys.sleep(.5) # 500 ms
            status_response <- gorr__get_query_status(query_response$links$self, conn)
            spinner(sprintf("%s (elapsed: %.1f %s)", status_response$status, elapsed, attr(elapsed, "units")))
            if (status_response$status == "FAILED")
                gorr__failure("Query Execution Failed", status_response)

            if (status_response$status == "DONE")
                break
        }
        if (interactive()) {
            cli::cat_line("")
            cli::cat_line(" Result details: ",
                          status_response$stats$line_count, " rows by ",
                          status_response$stats$column_count, " columns, total size: ",
                          fs::fs_bytes(status_response$stats$size_bytes), "bytes")
        }

        result <- gorr__get_query_results(query_response$links$result, conn,
                                          spinner = spinner, query_limit = page_size, parse = parse)

        if (parse && !is.data.frame(result)) {
            gorr__failure("Unexpected result:", result)
        }

        if (interactive()) {
            cli::cat_line("")
            cli::cat_rule("Done", col = "green")
        }


        result
    },
    interrupt = function(err) gorr__kill_query(query_id, conn),
    error = stop)
}


#' Post query to api. This is not a public function, but is called from \code{\link{gor_query}}
#'
#' @param query GOR query
#' @param conn connection object, see \code{\link{gor_connect}}
#'
#' @return response content object, see \code{\link[httr]{content}}
gorr__post_query <- function(query, conn) {
    gorr__api_request("POST",
        url = conn$endpoints$query,
        body = list(query = query, project = conn$project),
        conn)
}


#' Get query status from api. This is not a public function, but is called from \code{\link{gor_query}}
#'
#' @param query_url query url
#' @param conn connection object, see \code{\link{gor_connect}}
#'
#' @return response content object, see \code{\link[httr]{content}}
gorr__get_query_status <- function(query_url, conn) {
    gorr__api_request("GET",
        url = query_url,
        conn = conn
    )
}


#' Get server query results
#'
#' @param query_result_url query result url
#' @param conn connection object, see \code{\link{gor_connect}}
#' @param spinner spinner function (e.g. some ascii animation for long running queries)
#' @param query_limit limit to how many rows to fetch in each page
#' @param offset query offset (skip rows)
#' @param parse should output be parsed into a data.frame
#'
#' @return data.frame of results
gorr__get_query_results <- function(query_result_url, conn, spinner = invisible, query_limit = 10000, offset = 0, parse = T) {
    url <- stringr::str_glue("{query_result_url}?offset={format(offset,scientific=F)}&limit={format(query_limit,scientific=F)}")
    results <- NULL
    page <- 0
    repeat {
        page <- page + 1
        Sys.sleep(.5) # seconds to wait between status polls
        # parse the query string part of the url into a human readable format
        url_query_details <- url %>%
            httr::parse_url() %$%
            paste(stringr::str_to_title(names(query)), query, sep = ": ", collapse = ", ")

        spinner(stringr::str_glue("Fetching results (Page #{page}, {url_query_details})"))
        response <- gorr__api_request("GET",
            url = url,
            conn = conn,
            parse.body = F)

        # Stop with an error in case the response has http error codes
        httr::stop_for_status(response)

        #Extract headers and body
        headers <- httr::headers(response)
        body <- suppressMessages({#suppress messages needed to suppress unnecessary parsing messages
            gorr__get_response_body(response, content.fun = httr::text_content)
        })


        # if we're not on the first page, we need to remove the repeating header before we append the strings
        # str_replace stops replacing after first match.
        if (page > 1)
            results <- stringr::str_replace(results, ".*\n", "")
        results <- paste0(body, results)
        if (is.null(headers$link))
            break

        url <- stringr::str_match(headers$link, "<(?<path>.*)>; rel=\"next\"")[,2]
        if (is.na(url))
            gorr__failure("Could not parse header link to rel next:", headers$link)

    }

    if (parse) {
        gorr__read_tsv(text = results)
    } else {
        results
    }
}


#' Kill remote query
#' @param query_url query url
#' @param conn connection object, see \code{\link{gor_connect}}
gorr__kill_query <- function(query_url, conn) {
    if (interactive()) {
        cli::cat_line("")
        cli::cat_line("Killing Query", col = "red")
    }

    response <- gorr__api_request("DELETE",
        url = query_url,
        conn = conn,
        parse.body = F
    )
    gorr__failure("Killed query")
}


#' Get response body, fail with an error if it has one
#'
#' @param response from e.g.  \code{\link[httr]{POST}}, \code{\link[httr]{GET}}, \code{\link[httr]{DELETE}}
#' @param content.fun content function used to extract content from the response, e.g. \code{\link[httr]{text_content}}
#'
#' @return response body from (\code{\link[httr]{content}})
gorr__get_response_body <- function(response, content.fun = httr::content) {
    # Conveniently converts HTTP errors to R errors
    httr::stop_for_status(response$status)
    response_body <- content.fun(response)
    # Check to see if the query response has an error in it
    if (!is.null(response$error)) {
        gorr__failure(httr::http_status(response$code)$message,
                      response$error$description)
    }

    response_body
}


gorr__read_tsv <- purrr::compose(dplyr::tbl_df, data.table::fread)

# from cli:::rpad
rpad <- function(x, width = NULL) {
    if (!length(x))
        return(x)
    w <- nchar(x, type = "width")
    if (is.null(width))
        width <- max(w)
    paste0(x, strrep(" ", pmax(width - w, 0)))
}

gorr__spinner <- function(msg) {
    cat("\r", rpad(msg, cli::console_width() - 1))
}

#' Custom wrapper for stop() with formated error messages
#'
#' @param msg exception message
#' @param detail exception details (chr or chr vector)
gorr__failure <- function(msg, detail = NULL) {
    cli::cat_line()
    cli::cat_rule(col = "red")
    if (length(detail) > 0) {
        if (is.null(names(detail)))
            detail <- paste(detail, collapse = "\n")
        else
            detail <- paste(names(detail), detail, sep = ": ", collapse = "\n    ")

        stop(paste(crayon::red(msg), "\nDetails: \n    ", detail), call. = F)
    }

    stop(crayon::red(msg), call. = F)
}

