
#' Get query from queryserver api. This is not a public function, but is called from \code{\link{gor_query}}
#'
#' @param query gor query string
#' @param conn connection object, see \code{\link{platform_connect}}
#' @param parse should output be parsed into a data.frame
#' @param relations list of tables to upload and make available in the query
#' @param spinner spinner function (e.g. some ascii animation for long running queries)
#' @param ... additional arguments to be passed to \code{\link{gorr__post_query}}
#'
#' @return query results on text format
gorr__queryserver <- function(query, conn, parse, relations, spinner = invisible, ...) {
    start_time <- lubridate::now()

    # Function that handles stream - nb stream handler does not return anything hence we append to a variable
    gorr__stream_handler <- function(x) {
        elapsed <- lubridate::now() - start_time
        stream <- readBin(x, character())
        if (gorr__check_for_msg(stream)) {
            MESSAGE <<- gorr__process_msg(stream, spinner, elapsed, MESSAGE)
            stream <- gorr__remove_msg(stream)
        }
        spinner(gorr__elapsed_time(elapsed, status = MESSAGE$status, info = MESSAGE$info)) # Print progress to cli
        RESULT <<- paste0(RESULT, stream) # Append results to variable
    }

    # Initialize for stream handling
    RESULT <- ""
    MESSAGE <- list(status = "RUNNING", info = "")

    tryCatch({
        response <- gorr__post_query(query = query,
                                           conn = conn,
                                           relations = relations,
                                           parse.body = F,
                                           stream.handler = gorr__stream_handler,
                                           ...)
    },
    interrupt = function(err) gorr__kill_stream(),
    error = function(err) {closeAllConnections(); stop(err)}
    )

    RESULT
}

# Check for status messages in stream
gorr__check_for_msg <- function(msg) grepl("#>", msg, fixed=T)

# Extract status messages from stream
gorr__extract_msg <- purrr::compose(unlist, purrr::partial(stringr::str_extract_all, pattern = "(?<=#>)(.*?)(?=\\n)")) # Extract message between #> and new line

# Remove status messages from stream
gorr__remove_msg <- purrr::partial(stringr::str_remove_all, pattern = "#>.*?\\n") # Remove messages between (and including) #> and new line

#' Kill remote query stream
gorr__kill_stream <- function() {
    if (interactive()) {
        cli::cat_line("")
        cli::cat_line("Killing Query", col = "red")
    }

    closeAllConnections()

    gorr__failure("Killed query")
}

# Process status messages from stream
gorr__process_msg <- function(stream, spinner, elapsed, msg) {
    # Process last message from stream
    parsed_msg <- gorr__extract_msg(stream) %>%
        dplyr::last() %>%
        stringr::str_match(pattern="^\\s?([\\w]+)\\s(.*)") %>% #Get ([STATUS]) ([REST])
        as.character()

    status <- parsed_msg[2]
    if (status == "EXCEPTION") {
        gorr__failure("Query Failure", detail = parsed_msg[3])
    } else if (status == "GOR") {
        msg <- list(status = "RUNNING", info = paste0(msg$info, "."))
    } else if (status == "DONE") {
        stats <- jsonlite::fromJSON(parsed_msg[3])

        msg <- list(status = status, info = paste0(" ", " \n Result details: ",
                                                   stats$lineCount, " rows, total size: ",
                                                   fs::fs_bytes(stats$bytesCount), "bytes \n"))
    } else {
        msg <- list(status="UNKOWN", info = parsed_msg[1])
    }
    msg
}