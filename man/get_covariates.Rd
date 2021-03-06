% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/covariate_services.R
\name{get_covariates}
\alias{get_covariates}
\title{Get all covariates in current project.}
\usage{
get_covariates(conn)
}
\arguments{
\item{conn}{platform connection structure, create it using \code{\link{platform_connect}}}
}
\value{
a list of covariates
}
\description{
Get all covariates in current project.
}
\examples{
\dontrun{
api_key <- Sys.getenv("GOR_API_KEY")
project <- Sys.getenv("GOR_PROJECT")
conn <- platform_connect(api_key, project)
covariates <- get_covariates(conn)
}
}
