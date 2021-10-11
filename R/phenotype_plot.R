#' Plot phenotype
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#'
#' @return ggplot2 plot
#' @export
phenotype_plot <- function(phenotype) {
        assertthat::assert_that(class(phenotype) == "phenotype")
        data <- get_data(phenotype)
        request.fun <- switch(phenotype$result_type, QT = plot__qt, CATEGORY = plot__category, SET = plot__set)

        p <- request.fun(data) + ggplot2::labs(title = "Phenotype Overview") + ggplot2::geom_col(fill="steelblue")

        p
    }


plot__qt <- function(df) {
    colname <- colnames(df)[2]
    p <- ggplot2::ggplot(df, ggplot2::aes(x=colname)) + ggplot2::geom_histogram()
    return(p)
}

plot__category <- function(df) {
    colname = colnames(df)[2]
    df <- df %>%
        group_by_at(2) %>%
        summarise(count=n())
    p <- ggplot2::ggplot(data=df,ggplot2::aes(x=colname, y=count)) +
                ggplot2::geom_bar(stat="identity")
}


plot__set <- function(df) {
    p1 <- ggplot2::ggplot(data.frame(x = 1, y=nrow(df)),  aes(x="", y=y) ) +  geom_text(aes(label = paste0("Count: \n", y)),  size=13, vjust=-1)  + ggplot2::coord_polar(theta = "y") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +  theme_void()
}
