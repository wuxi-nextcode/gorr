% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenotype_playlist.R
\name{playlist_refresh}
\alias{playlist_refresh}
\title{Refresh playlist}
\usage{
playlist_refresh(playlist, conn = NULL)
}
\arguments{
\item{playlist}{structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}}

\item{conn}{gor connection structure, create it using  \code{\link{platform_connect}}}
}
\value{
a list with the playlist object
}
\description{
Refresh playlist
}
\examples{
\dontrun{
api_key <- Sys.getenv("GOR_API_KEY")
project <- Sys.getenv("GOR_PROJECT")
conn <- platform_connect(api_key, project)
name <- "testPL"
pl <- create_playlist(name, conn)
pl <- playlist_refresh(pl, conn)
}
}
