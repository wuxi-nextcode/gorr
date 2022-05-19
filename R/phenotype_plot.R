# Genuity science's colors
GENUSCI_COL1 <- "#93C90E"
GENUSCI_COL2 <- "#008A97"


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
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x=colname)) +
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
    p <- ggplot2::ggplot(data=df, ggplot2::aes_string(x=colname, y="count")) +
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
