% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype_services.R
\name{phenotype_set_tags}
\alias{phenotype_set_tags}
\title{Set the tag list for this phenotype, overriding all previous tags}
\usage{
phenotype_set_tags(tags, phenotype, conn)
}
\arguments{
\item{tags}{string of tags to be added seperated by comma eg. "height,weight"  or character vector "c("height", "weight")}

\item{phenotype}{phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}}

\item{conn}{gor connection structure, create it using \code{\link{platform_connect}}}
}
\value{
an updated list with the phenotype object
}
\description{
Set the tag list for this phenotype, overriding all previous tags
}
