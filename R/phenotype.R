# Phenotype class constructor
#
# A local object representing a phenotype response from the phenotype
# catalog service.
#
# Note that most of the attributes come directly from the phenotype
# serverside response and are therefore not documented directly here.
# Please refer to the API Documentation for the phenotype catalog service.
#
# In addition to the ones documented here, this object has at least these attributes:
#
# * name - Phenotype name
# * description - Textual description of this phenotype
# * result_type - Type of result. Cannot be changed. One of SET, QT, CATEGORY
# * created_at - Timestamp when the phenotype was first created
# * updated_at - Timestamp when the phenotype was last updated
# * created_by - Username who created the phenotype
# * versions - List of data versions available in this phenotype
Phenotype <- function(phenotype, conn) {
    structure(phenotype, class = "phenotype", conn = conn)
}

# print.phenotype <- function(x, ...) {
#
#     bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
#     cli::cat_rule(left = ("Phenotype"))
#
#     bullet("$name: ", x$name)
#     bullet("$description: ", x$description)
#     bullet("$result_type: ", x$result_type)
#     bullet("$tag_list: ", paste(x$tag_list, collapse = ", "))
#     bullet("$pn_count: ", x$pn_count)
#     bullet("$query: ", x$query)
# }

#' @export
print.phenotype <- function(x, ...) {
    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    item_name <- function(x) paste0("$", x, ": ")
    cli::cat_rule(left = "Phenotype")
    bullet("$name: ", x$name)

    excl <- c("name", "links")
    conv <- c("tag_list")
    x <- x[(!(names(x)  %in% excl))]
    purrr::modify_at(x, conv, list) %>%
        purrr::walk2(., names(x), ~bullet(item_name(.y), .x))
}



#' Delete a phenotype, including all data from a project
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' phenotype_delete(phenotype, conn)
#' }
phenotype_delete <- function(phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
            warning()
    }
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(attr(phenotype, which = "conn")) == "platform_connection")

    url <- get__link(phenotype, "self")

    gorr__api_request("DELETE",
                      url = url,
                      conn = attr(phenotype, which = "conn"),
                      parse.body = F)
}


#' Refresh phenotype
#'
#' Fetches current (up-to-date) phenotype version from project.
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return phenotype structure
#' @export
phenotype_refresh <- function(phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
            warning()
    }
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(attr(phenotype, which = "conn")) == "platform_connection")

    url <- get__link(phenotype, "self")
    resp <- gorr__api_request("GET", url = url, conn = attr(phenotype, which = "conn"))

    Phenotype(resp$phenotype, conn = attr(phenotype, which = "conn"))
}


#' Retrieve all tags for phenotype
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#'
#' @return a list of phenotype tags
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotype <- get_phenotype(name="height", conn)
#' tags <- phenotype_get_tags(phenotype)
#' }
phenotype_get_tags <- function(phenotype) {
    assertthat::assert_that(class(phenotype) == "phenotype")
    phenotype$tag_list %>% unlist()
}


#' Retrieve all errors for phenotype query runs
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#'
#' @return a list of errors from query runs
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotype <- get_phenotype(name="height", conn)
#' phenotype_get_errors(phenotype)
#' }
phenotype_get_errors <- function(phenotype) {
    assertthat::assert_that(class(phenotype) == "phenotype")
    purrr::map(phenotype$events, ~ if (.x$event_type =="error") list(message = .x$message, created_at = .x$created_at)) %>%
        purrr::compact()
}


#' Retrieve latest error from phenotype query run
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#'
#' @return a formatted error message
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotype <- get_phenotype(name="height", conn)
#' phenotype_get_error(phenotype)
#' }
phenotype_get_error <- function(phenotype) {
    assertthat::assert_that(class(phenotype) == "phenotype")
    format_error <- purrr::compose(purrr::partial(utils::capture.output, split=TRUE),
                                   cat,
                                   purrr::partial(gsub, pattern = "\\\\n", replacement = "\\\n", ... =),
                                   purrr::partial(gsub, pattern = "\\\\t", replacement = "\\\t", ... =)
    )

    phenotype_get_errors(phenotype) %>%
        dplyr::first() %>%
        purrr::pluck("message") %>%
        format_error()
}


#' Update phenotype attributes
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#' @param description phenotype description
#' @param tags comma separated string of tags eg. "height,weight", list or character vector "c("height", "weight")
#' @param query NOR query that defines this phenotype
#' @param category category for the phenotype (must be defined in the project - see get_categories)
#' @param url reference URL for the phenotype (to dataset or other reference)
#'
#' @return phenotype object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' description <- "individual height"
#' phenotype <- update_phenotype(phenotype, description=decription)
#' }
phenotype_update <-
    function(phenotype, description=NULL, tags=list(), query=NULL, category=NULL, url=NULL) {
        assertthat::assert_that(class(phenotype) == "phenotype")
        assertthat::assert_that(is.null(description) | is.character(description))
        assertthat::assert_that(is.list(tags) | is.character(tags))
        assertthat::assert_that(is.null(query) | is.character(query))
        assertthat::assert_that(is.null(category) | is.character(category))
        assertthat::assert_that(is.null(url) | is.character(url))

        if (length(tags) > 0) {
            tags <- parse__tags(tags)
        }

        url <- get__link(phenotype, "self")
        content <- list(description = description,
                        query = query,
                        tag_list = as.list(tags),
                        category = category,
                        url = url)
        resp <-
            gorr__api_request("PATCH",
                              url = url,
                              body = content,
                              conn = attr(phenotype, which = "conn"))

        Phenotype(resp$phenotype, conn = attr(phenotype, which = "conn"))
    }


#' Update the phenotype with a new description
#'
#' @param description phenotype description
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{platform_connect}}
#'
#' @return phenotype structure
#' @export
phenotype_update_description <- function(description, phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }

    phenotype_update(phenotype, description = description)
}


#' Update the phenotype with new query
#'
#' @param query NOR query that defines this phenotype
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#'
#' @return phenotype structure
#' @export
phenotype_update_query <- function(query, phenotype) {
    phenotype_update(phenotype, query = query)
}


#' Add new tag/s phenotype.
#'
#' @param tags string or character vector of tag/s to be added. Tags should be seperated by comma e.g. "height,weight" or as vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#'
#' @return phenotype structure
#' @export
phenotype_add_tags <- function(tags, phenotype) {
    assertthat::assert_that(is.list(tags) | is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")

    pheno_tags <- phenotype_get_tags(phenotype)
    tags <- parse__tags(tags)

    if (all(tags %in% pheno_tags)) {
        warning(paste("Tags:", tags, "already in phenotype tag list:", pheno_tags))
        return(phenotype)
    }

    tag_list <- base::union(pheno_tags, tags)
    phenotype_update(phenotype, tags = tag_list)
}


#' Set the tag/s for this phenotype, overriding previous tags
#'
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#' @inheritParams phenotype_add_tags
#'
#' @return phenotype structure
#' @export
phenotype_set_tags <- function(tags, phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }
    assertthat::assert_that(is.list(tags) | is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")

    phenotype_update(phenotype, tags = tags)
}


#' Delete tag/s from phenotype.
#'
#' @inheritParams phenotype_add_tags
#'
#' @return phenotype structure
#' @export
phenotype_delete_tags <- function(tags, phenotype) {
    assertthat::assert_that(is.list(tags) | is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")

    pheno_tags <- phenotype_get_tags(phenotype)
    tags <- parse__tags(tags)
    tags_list <- pheno_tags[!(pheno_tags %in% tags)]

    phenotype_update(phenotype, tags = tags_list)
}


#' Upload phenotype data
#'
#' The data is expected to be a data.frame, tibble or list of lists. If data.frame or tibble is provided it should contain 2 columns.
#' and list of lists should be of the form `list(list(pn1, value1),list(pn2,value2))` or `list(list(pn1),list(pn2))` For phenotypes of result_type SET.
#' The `result_type` of the phenotype dictates if each sublist should contain one or two items.
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param data a data.frame or list of lists to be uploaded
#'  * For phenotypes of result_type 'SET' - data should be either a list single column data.frame.
#'  * For other phenotypes the data.frame/tibble should contain 2 columns, pn and value or if list a pn value pair for each pn.
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @export
phenotype_upload_data <- function(phenotype, data, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(is.list(data) | is.data.frame(data))
    assertthat::assert_that(class(attr(phenotype, which = "conn")) == "platform_connection")

    if ( phenotype$result_type == "SET" && is.data.frame(data) && ncol(data) > 1 ) gorr__failure(msg="Data for phenotypes of result_type 'SET' should be either a list or a single column data.frame")


    # If input is data.frame convert to list of lists
    if (is.data.frame(data)) {
        assertthat::assert_that(ncol(data) <= 2, msg = "data.frame/tibble should only contain 1 (if pheno is SET) or 2 columns, pn and value")
        data <- apply(data, 1, as.list) %>%
            lapply(unname)
    }

    url <- get__link(phenotype, "upload")
    content <- list(data = data)

    gorr__api_request("POST",
                      url,
                      body = content,
                      conn = attr(phenotype, which = "conn"),
                      parse.body = F) %>%
        httr::stop_for_status()

    gorr__info(msg="Successfully uploaded phenotype data")
}


#' Plot phenotype
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#' @param title optional plot title. Default: name of phenotype
#' @param y optional y axis label: Default: Count
#' @param x optional x axis label. Default: None
#'
#' @return ggplot2 plot
#' @export
phenotype_plot <- function(phenotype, title=NULL, y=NULL, x=NULL) {
    assertthat::assert_that(class(phenotype) == "phenotype")

    # Genuity science's colors
    GENUSCI_COL1 <- "#93C90E"
    GENUSCI_COL2 <- "#008A97"

    data <- get_data(phenotype)
    request.fun <- switch(phenotype$result_type, QT = plot__qt, CATEGORY = plot__category, SET = plot__set)

    p <- request.fun(data, colname=phenotype$name, fill=GENUSCI_COL2) +
        ggplot2::labs(title = if (is.null(title)) "Phenotype Plot" else title) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(colour = GENUSCI_COL2),
                       axis.line = ggplot2::element_line(colour = "black")) +
        ggplot2::xlab(phenotype$name)
    p
}


#' Plot quantitative phenotypes. This is not a public function, but is called from \code{\link{phenotype_plot}}
#'
#' @param df phenotype data
#' @param colname name of column to plot
#' @param fill fill color
#'
#' @return ggplot2 object
plot__qt <- function(df, colname, fill) {
    p <- ggplot2::ggplot(df,
                         ggplot2::aes_string(x=colname)) +
        ggplot2::geom_histogram(fill=fill, binwidth = 3)
}


#' Plot categorical phenotypes. This is not a public function, but is called from \code{\link{phenotype_plot}}
#'
#' @param df phenotype data
#' @param colname name of column to plot
#' @param fill fill color
#'
#' @return ggplot2 object
plot__category <- function(df, colname, fill) {
    df <- df %>%
        dplyr::mutate_at(2, factor) %>%
        dplyr::group_by_at(2) %>%
        dplyr::summarise(count=dplyr::n())
    p <- ggplot2::ggplot(data=df,
                         ggplot2::aes_string(x=colname, y="count")) +
        ggplot2::geom_bar(stat="identity", fill=fill)
}


#' Plot set phenotypes. This is not a public function, but is called from \code{\link{phenotype_plot}}
#'
#' @param df phenotype data
#' @param colname name of column to plot
#' @param fill fill color
#'
#' @return ggplot2 object
plot__set <- function(df, colname, fill) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x=colname)) +
        ggplot2::geom_bar(fill=fill, width = 0.5) +
        ggplot2::geom_text(stat='count', ggplot2::aes(label=..count..), vjust=-1)
}
