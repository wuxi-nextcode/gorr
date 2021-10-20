
#' Get query from queryservice api. This is not a public function, but is called from \code{\link{gor_query}}
#'
#' @param query gor query string
#' @param conn connection object, see \code{\link{platform_connect}}
#' @param page_size large results are returned in paged responses, this parameter controls the page size
#' @param parse should output be parsed into a data.frame
#' @param relations list of tables to upload and make available in the query
#' @param persist remote path to file for saving results of the query into. Query results will not be fetched if this parameter is set.
#' @param spinner spinner function (e.g. some ascii animation for long running queries)
#' @param ... additional arguments to be passed to \code{\link{gorr__post_query}}
#'
#' @return query results on text format
gorr__queryservice <- function(query, conn, page_size, parse, relations, persist, spinner = invisible, ...) {
    if (page_size == 0)
        warning("Setting page_size to 0 could crash the API server for very large results")

    start_time <- lubridate::now()

    query_response <- gorr__post_query(query = query, conn = conn, relations = relations, persist = persist, parse.body = T, ...)
    if (interactive()) {
        cli::cat_line("")
        cli::cat_line(paste0(" Server assigned query ID ", crayon::green(paste0("#",query_response$id))))
    }

    tryCatch({
        repeat {
            elapsed <- lubridate::now() - start_time
            Sys.sleep(.5) # 500 ms
            status_response <- gorr__get_query_status(query_response$links$self, conn)
            spinner(gorr__elapsed_time(elapsed, status = status_response$status))
            if (status_response$status == "FAILED")
                gorr__failure(status_response$error$type,  status_response$error$description)

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

        # If we are persisting the results, then don't also fetch them over the wire
        if (is.null(persist)) {
            result <- gorr__get_query_results(query_response$links$result, conn, spinner = spinner, query_limit = page_size)

        } else {
            if (interactive()) {
                cli::cat_line(" Results saved to remote file: ", crayon::green(crayon::italic(persist)))
            }
            result <- NULL
        }

        result
    },
    interrupt = function(err) gorr__kill_query(query_response$links$self, conn),
    error = stop)
}


#' Get query status from api. This is not a public function, but is called from \code{\link{gorr__queryservice}}
#'
#' @param query_url query url
#' @param conn connection object, see \code{\link{platform_connect}}
#'
#' @return response content object, see \code{\link[httr]{content}}
gorr__get_query_status <- function(query_url, conn) {
    gorr__api_request("GET",
                      url = query_url,
                      conn = conn
    )
}


#' Get server query results This is not a public function, but is called from \code{\link{gorr__queryservice}}
#'
#' @param query_result_url query result url
#' @param conn connection object, see \code{\link{platform_connect}}
#' @param spinner spinner function (e.g. some ascii animation for long running queries)
#' @param query_limit limit to how many rows to fetch in each page
#' @param offset query offset (skip rows)
#'
#' @return data.frame of results
gorr__get_query_results <- function(query_result_url, conn, spinner = invisible, query_limit = 10000, offset = 0) {
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

    results
}


#' Kill remote query
#' @param query_url query url
#' @param conn connection object, see \code{\link{platform_connect}}
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
