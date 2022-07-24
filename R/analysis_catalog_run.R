# Analysis catalog run
# ------------------
#
# Analysis catalog run status

# AnalysisCatalogRun class constructor
# A local object representing an analysis catalog run response from the phenotype
# catalog service.
#
# Please refer to the API Documentation for the phenotype catalog service.
#
# In addition to the ones documented here, this object has at least these attributes:
#
# * name - Analysis Catalog Run name
# * state - the state of the Analysis Catalog Run
# * created_at - Timestamp when the analysis catalog run was first created
# * updated_at - Timestamp when the analysis catalog run was last updated
# * ended_at - Timestamp when the analysis catalog run ended (entered 'failed' or 'completed' state)
AnalysisCatalogRun <- function(data, conn){
    structure(data, class = "analysis_catalog_run", conn=conn)
}

#' @export
print.analysis_catalog_run <- function(x, ...) {
    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("Phenotype"))

    bullet("$name: ", x$name)
    bullet("$description: ", x$description)
    bullet("$result_type: ", x$result_type)
    bullet("$tag_list: ", paste(x$tag_list, collapse = ", "))
    bullet("$pn_count: ", x$pn_count)
    bullet("$query: ", x$query)
}
