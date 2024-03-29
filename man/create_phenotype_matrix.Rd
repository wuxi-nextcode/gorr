% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype-catalog_services.R
\name{create_phenotype_matrix}
\alias{create_phenotype_matrix}
\title{Get a phenotype matrix object.}
\usage{
create_phenotype_matrix(conn, base = NULL, ...)
}
\arguments{
\item{conn}{platform connection structure, create it using \code{\link{platform_connect}}}

\item{base}{Optional name of base set}

\item{...}{named arguments passed to \code{get_phenotypes} for populating matrix.}
}
\value{
a phenotype matrix object
}
\description{
Get a phenotype matrix object.
}
\examples{
\dontrun{
phenotype_mat <- get_phenotype_matrix()
}
}
