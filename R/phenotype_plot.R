#' Plot phenotype
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#' @param title optional plot title. Default: name of phenotype
#'
#' @return ggplot2 plot
#' @export
phenotype_plot <- function(phenotype, title = NULL) {
        assertthat::assert_that(class(phenotype) == "phenotype")
        data <- get_data(phenotype)
        request.fun <- switch(phenotype$result_type, QT = plot__qt, CATEGORY = plot__category, SET = plot__set)

        p <- request.fun(data, colname=phenotype$name, fill = "steelblue") + ggplot2::labs(title = if (is.null(title)) phenotype$name else title)
    }


#' Plot quantitative phenotypes. This is not a public function, but is called from \code{\link{phenotype_plot}}
#'
#' @param df phenotype data
#' @param colname colname to plot
#' @param fill fill color
#'
#' @return ggplot2 object
plot__qt <- function(df, colname, fill) {
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x=colname)) + ggplot2::geom_histogram(fill = fill, binwidth = 0.5)
}


#' Plot categorical phenotypes. This is not a public function, but is called from \code{\link{phenotype_plot}}
#'
#' @param df phenotype data
#' @param colname colname to plot
#' @param fill fill color
#'
#' @return ggplot2 object
plot__category <- function(df, colname, fill) {
    df <- df %>%
        group_by_at(2) %>%
        summarise(count = n())
    p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = colname, y = "count")) +
                ggplot2::geom_bar(stat = "identity", fill = fill)
}


#' Plot set phenotypes. This is not a public function, but is called from \code{\link{phenotype_plot}}
#'
#' @param df phenotype data
#' @param colname colname to plot
#' @param fill fill color
#'
#' @return ggplot2 object
plot__set <- function(df, colname, fill) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = colname)) +
        ggplot2::geom_bar(fill = fill, width = 0.5) +
        ggplot2::geom_text(stat = 'count', ggplot2::aes(label = ..count..), vjust = -1) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
}
