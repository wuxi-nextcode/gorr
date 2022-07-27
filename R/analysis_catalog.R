#
# Analysis catalog
# ------------------
#
# Analysis catalog is a collection of phenotypes and parameters that are needed to run
# the workflow analysis
#

# AnalysisCatalog class constructor
# A local object representing an analysis catalog run response from the phenotype
# catalog service.
#
# Please refer to the API Documentation for the phenotype catalog service.
#
# In addition to the ones documented here, this object has at least these attributes:
#
#  * name - Analysis Catalog Run name
# * state - the state of the Analysis Catalog Run
# * created_at - Timestamp when the analysis catalog run was first created
# * updated_at - Timestamp when the analysis catalog run was last updated
# * ended_at - Timestamp when the analysis catalog run ended (entered 'failed' or 'completed' state)
AnalysisCatalog <- function(data, conn){
    structure(data, class = "analysis_catalog", conn=conn)
}

#' @export
print.analysis_catalog <- function(x, ...) {
    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    item_name <- function(x) paste0("$", x, ": ")
    cli::cat_rule(left = ("Analysis Catalog"))

    bullet("$name: ", x$name)
    x <- purrr::list_modify(x, "name" = NULL) %>% purrr::compact()
    purrr::modify_at(x, "recipe_parameters", list) %>%
        purrr::walk2(., names(x), ~bullet(item_name(.y), .x))
}
