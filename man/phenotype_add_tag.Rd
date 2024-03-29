% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprec-phenotype_misc.R
\name{phenotype_add_tag}
\alias{phenotype_add_tag}
\title{Add a new tag/s to this phenotype.}
\usage{
phenotype_add_tag(tag, phenotype, conn = NULL)
}
\arguments{
\item{tag}{string or character vector of tag/s to be added. Tags should be seperated by comma e.g. "height,weight" or as vector "c("height", "weight")}

\item{phenotype}{phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}}

\item{conn}{Deprecated : gor connection structure, create it using \code{\link{platform_connect}}}
}
\value{
an updated phenotype object
}
\description{
This method is depricated and will be removed in future versions. use \code{\link{phenotype_add_tags}} instead.
}
