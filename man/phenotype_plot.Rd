% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype.R
\name{phenotype_plot}
\alias{phenotype_plot}
\title{Plot phenotype}
\usage{
phenotype_plot(phenotype, title = NULL, y = NULL, x = NULL)
}
\arguments{
\item{phenotype}{phenotype structure, create or get it using \code{\link{get_phenotype}}}

\item{title}{optional plot title. Default: name of phenotype}

\item{y}{optional y axis label: Default: Count}

\item{x}{optional x axis label. Default: None}
}
\value{
ggplot2 plot
}
\description{
Plot phenotype
}
