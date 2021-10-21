# Endpoint definitions
ENDPOINTS <- list(queryservice = list(name = "gor-query-api",
                                      endpoint="/api/query",
                                      display_name = "GOR Query Service"),
                  queryserver = list(name = "QueryServer",
                                     endpoint="/queryserver",
                                     display_name = "GOR Query Server"),
                  phenotype_catalog = list(name="`phenotype-catalog`",
                                           endpoint="/api/phenotype-catalog",
                                           display_name="Phenotype Catalog"))



gorr__api_request <- function(request.fun = c("POST", "GET", "DELETE", "PATCH"),
                              url, conn, body = list(), query=list(), parse.body = T, stream.handler = NULL) {
    request.fun <- match.arg(request.fun)
    request.fun <- switch(request.fun, POST = httr::POST, GET = httr::GET, DELETE = httr::DELETE, PATCH = httr::PATCH)

    if (!is.null(conn$access_token_exp) && conn$access_token_exp - lubridate::now() < lubridate::minutes(1)) {
        conn <- gorr__reconnect(conn)
    }
    debug <- getOption("gor.debug", default = F)
    response <- request.fun(url = url,
                            body = body[!sapply(body,is.null)],  # Remove NULLs from request
                            query = query,
                            conn$header,
                            encode = "json",
                            if (debug) httr::verbose(),
                            if (!is.null(stream.handler)) httr::write_stream(stream.handler)
                            )
    if (parse.body) {
            response <- gorr__get_response_body(response)
    }

    response
}


#' Run a GOR query
#'
#' @param query gor query string
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param timeout timeout in seconds, default to 0 (none), uses \code{\link[base]{setTimeLimit}} to interrupt, note that setting any limit has a small overhead – well under 1% on the systems measured.
#' @param page_size large results are returned in paged responses, this parameter controls the page size (e.g. 1000 lines at a time), default is 100k. A value of 0 means everything is fetched in one request
#' @param parse should the TSV output be parsed into a dataframe? False will make the function return the results as text object
#' @param relations list of tables to upload and make available in the query, e.g. \code{list(cars = cars, letters = data.frame(letter = letters))}, refer to them in the query using [] around their names, e.g. `nor -h [cars]`
#' @param persist remote path to file for saving results of the query into. Query results will not be fetched if this parameter is set.
#' @param query.service query service to use - either 'queryservice' (old) or 'queryserver' (new). Default: queryservice
#'
#' @return data.frame of gor results, i.e. gor results are passed to \code{\link[readr]{read_tsv}}
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' "gor #dbsnp# | top 100" %>%
#'     gor_query(conn)
#' }
gor_query <- function(query, conn, timeout = 0, page_size = 100e3, parse = T, relations = NULL, persist = NULL, query.service = "queryservice") {
    assertthat::assert_that(is.string(query))
    assertthat::assert_that(class(conn) == "platform_connection")

    if (!is.null(persist) && query.service == "queryserver")
        error("Persisting results not allowed using 'queryserver' in R-sdk. Please add 'write' statement to the GOR query or switch to using 'queryservice'")

    query.fun <- switch(query.service, queryservice = gorr__queryservice, queryserver = gorr__queryserver)

    if (timeout > 0)
        setTimeLimit(elapsed = timeout)

    spinner <- if (interactive()) gorr__spinner else invisible
    if (interactive()) {
        cli::cat_rule(left = "Executing Query", right = ENDPOINTS[[query.service]]$display_name, col = "blue")
        cli::cat_line(query, col = "silver")
        cli::cat_rule("", col = "blue")
    }
    spinner("Submitting Query\n")

    result <- query.fun(query = query,
              conn = conn,
              parse = parse,
              relations = relations,
              query.service = query.service,
              persist = persist,
              page_size = page_size,
              spinner = spinner)


    if (parse && !is.null(result)) {
        result <- gorr__read_tsv(text = result)
        if (!is.data.frame(result)) gorr__failure("Unexpected result:", result)
    }

    if (interactive()) {
        cli::cat_rule("Done", col = "green")
    }

    if (is.null(result)) invisible(result) else result
}


#' Post query to api. This is not a public function, but is called from \code{\link{gor_query}}
#'
#' @param query GOR query
#' @param conn connection object, see \code{\link{platform_connect}}
#' @param relations data.frames to include with the query in the format \code{list(list(table_name = data.frame() ))}
#' @param parse.body logical, should the response body be fetched
#' @param query.service qquery service to use - either 'queryservice' (old) or 'queryserver' (new)
#' @param persist remote path to file for saving results of the query into. Query results will not be fetched if this parameter is set.
#' @param stream.handler function to be passed as an input to POST request stream httr::write_stream(fcn), Default: NULL
#' @param ... placeholder for unused arguments
#'
#'
#' @return response content object, see \code{\link[httr]{content}}
gorr__post_query <- function(query,
                             conn,
                             relations,
                             parse.body,
                             query.service = c("queryservice", "queryserver"),
                             persist = NULL,
                             stream.handler = NULL, ...) {

    query.service <- match.arg(query.service)

    if (!is.null(relations)) {
        if (!all(purrr::map_lgl(relations, is.data.frame)))
            gorr__failure("Invalid relations parameter", "All relations must be dataframes")
        if (any(stringr::str_length(names(relations)) == 0))
            gorr__failure("All relations need to be named", "Valid: list(x = cars, y = faithful, z = airquality ), Invalid: list(cars, y = faithful, z = airquality) ")

        relations <-
            relations %>%
            purrr::imap(function(df,name) {
                list(name = name,
                     fingerprint = digest::digest(df, algo = "md5"),
                     data = paste0("#", readr::format_tsv(df)), # # is added to indicate header for GOR
                     extension = ".tsv")
                }) %>%
            unname()

        # make sure all relation keys are in the format [key]
        if (!all(stringr::str_detect(names(relations), "\\[.*\\]"))) {
            names(relations) <- paste0("[", stringr::str_replace_all(names(relations), c("[" = "")), "]")
        }
    }

    body <- list(query = query, project = conn$project)
    if (query.service == "queryservice") {
        body <- append(body,
                       list(relations = relations,
                            persist = persist)
                       )
    } else if (query.service == "queryserver") {
        body <- append(body,
                       list(virtualRelations = relations,
                       user = "r-sdk",
                       useGzip = FALSE,
                       sendTerm = TRUE,
                       sendProgress = TRUE,
                       sendAlive = TRUE)
                        )
    }

    gorr__api_request("POST",
        url = gorr__get_endpoint(conn, ENDPOINTS[[query.service]]$name, "query"),
        body = body,
        conn = conn,
        parse.body = parse.body,
        stream.handler = stream.handler)
}


#' Get response body, fail with an error if it has one
#'
#' @param response from e.g.  \code{\link[httr]{POST}}, \code{\link[httr]{GET}}, \code{\link[httr]{DELETE}}
#' @param content.fun content function used to extract content from the response, e.g. \code{\link[httr]{text_content}}
#'
#' @return response body from (\code{\link[httr]{content}})
gorr__get_response_body <- function(response, content.fun = purrr::partial(httr::content, encoding = "UTF-8")) {
    response_body <- content.fun(response)

    # Conveniently converts HTTP errors to R errors
   #  httr::stop_for_status(response$status_code) # Isn't this unnecessary because of the chunk that follows

    # Check to see if the query response has an error in it
    if (response$status_code != 200 && !is.null(response_body$error)) {
        message <- httr::http_status(response$status_code)$message
        details <- response_body$error$description
        if (!is.null(response_body$error$virtual_relations)) {
            virtual_relations <- purrr::map_chr(response_body$error$virtual_relations, function(x) {
              if (is.null(x$fingerprint)) x$fingerprint <- "NULL"
              stringr::str_c(x$name, " - fingerprint: ", x$fingerprint)
            })

            virtual_relations <- paste(virtual_relations, collapse = "\n\t\t - ")

            details <- paste0(details, "\n",  virtual_relations)
        }

        gorr__failure(message, details, url = response$url)
    }

    if (!is.character(response_body) && !is.null(response_body$status) && response_body$status == "FAILED" ) {
        message <- response_body$status
        details <- response_body$status_message

        gorr__failure(message, details)
    }

    response_body
}


gorr__read_tsv <- purrr::compose(tibble::as_tibble, data.table::fread)

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

#' Custom wrapper for formatted query progress messages
#' 
#' @param elapsed elapsed time
#' @param status optional query status message
#' @param info optional additional info message
gorr__elapsed_time <- function(elapsed, status = "", info = ""){
    sprintf("%s (elapsed: %.1f %s) %s", status, elapsed, attr(elapsed, "units"), info)
}


#' Custom wrapper for stop() with formated error messages
#'
#' @param msg exception message
#' @param detail exception details (chr or chr vector)
#' @param url query url
gorr__failure <- function(msg, detail = NULL, url=NULL) {
    cli::cat_line()
    cli::cat_rule(col = "red")
    if (length(detail) > 0) {
        if (is.null(names(detail)))
            detail <- paste(detail, collapse = "\n")
        else
            detail <- paste(names(detail), detail, sep = ": ", collapse = "\n    ")

        url_msg <- paste(crayon::red(paste("Failure while querying", url)), "\nInfo: \n    ")

        stop(paste(if (!is.null(url)) url_msg, crayon::red(msg), "\nDetails: \n    ", crayon::red(detail)), call. = F)
    } else {
        stop(crayon::red(msg), call. = F)
    }
}

#' Custom wrapper for warning() with formatted error messages
#'
#' @param msg warning message
#' @param detail warning details (chr or chr vector)
#' @param url query url
gorr__warning <- function(msg, detail = NULL, url=NULL) {
    cli::cat_line()
    cli::cat_rule(col = "yellow")
    if (length(detail) > 0) {
        if (is.null(names(detail)))
            detail <- paste(detail, collapse = "\n")
        else
            detail <- paste(names(detail), detail, sep = ": ", collapse = "\n    ")

        url_msg <- paste(crayon::white(paste("Failure while requesting", url)), "\nInfo: \n    ")

        stop(paste(if (!is.null(url)) url_msg, crayon::white(msg), "\nDetails: \n    ", crayon::white(detail)), call. = F)
    } else {
        stop(crayon::white(msg), call. = F)
    }
}
