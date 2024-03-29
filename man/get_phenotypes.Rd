% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype-catalog_services.R
\name{get_phenotypes}
\alias{get_phenotypes}
\title{A list of all/subset of phenotypes in the current project.}
\usage{
get_phenotypes(
  conn,
  tags = list(),
  limit = 100,
  pheno_names = list(),
  all_tags = list(),
  any_tags = list(),
  pn_count = NULL,
  categories = list(),
  states = list(),
  search = NULL,
  playlist = NULL,
  updated_at = NULL,
  result_types = list()
)
}
\arguments{
\item{conn}{gor connection structure, create it using \code{\link{platform_connect}}}

\item{tags}{Deprecated : Optional character, character vector or list of tags to filter for.}

\item{limit}{Maximum number of results fetched (default: 100)}

\item{pheno_names}{Only fetch phenotypes that are in a given list of phenotype names}

\item{all_tags}{Only fetch phenotypes that have all tags in the given list of tags}

\item{any_tags}{Fetch phenotypes that have any of the tags in the given list of tags}

\item{pn_count}{Only list phenotypes that match the given pn counts. (include greater or less symbol in string eg ">10")}

\item{categories}{Only fetch phenotypes in the given list of categories}

\item{states}{Only fetch phenotypes in the given list of states}

\item{search}{String of keywords to search for in phenotypes, such as name, categories and tags}

\item{playlist}{Fetch a specific playlist of phenotypes by the playlist id}

\item{updated_at}{Only fetch phenotypes that match the given dates. Example: >=2017-04-01 ┃ <=2012-07-04 ┃ 2016-04-30..2016-07-04}

\item{result_types}{Only fetch phenotypes in the given list of result types}
}
\value{
List of phenotypes
}
\description{
A list of all/subset of phenotypes in the current project.
}
\examples{
\dontrun{
api_key <- Sys.getenv("GOR_API_KEY")
project <- Sys.getenv("GOR_PROJECT")
conn <- platform_connect(api_key, project)
phenotypes <- get_phenotypes(conn, limit=5)
}
}
