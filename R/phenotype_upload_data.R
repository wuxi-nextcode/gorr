#' Upload phenotype data
#'
#' The data is expected to be a data.frame, tibble or list of lists. If data.frame or tibble is provided it should contain 2 columns.
#' and list of lists should be of the form `list(list(pn1, value1),list(pn2,value2))` or `list(list(pn1),list(pn2))` For phenotypes of result_type SET.
#' The `result_type` of the phenotype dictates if each sublist should contain one or two items.
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param data a list of lists to be uploaded
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
}