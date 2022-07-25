
#' PhenotypeList class constructor
#'
#'
#'
PhenotypeList <- function(data, conn) {
    structure(data, class="phenotype_list", conn=conn, .Names=purrr::map_chr(data. ~.x$name))
}

#' @export
print.phenotype_list <- function(x, ...) {

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("Phenotypes"))

    if (length(attr(x, "names")) == 0) {
        cli::cat_line("  None")
    } else {
        for (name in attr(x, "names")) {
            bullet("$", name)
        }
    }
}
