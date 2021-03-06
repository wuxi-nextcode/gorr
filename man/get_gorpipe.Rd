% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gor_pipe.R
\name{get_gorpipe}
\alias{get_gorpipe}
\title{get_gorpipe}
\usage{
get_gorpipe(path)
}
\arguments{
\item{path}{full path to gorpipe, e.g. ~/gor-scripts/bin/gorpipe}
}
\value{
function(code) -> data.frame
}
\description{
Generator function for gorpipe. Given a full path to the gorpipe executable, return a function that wraps gorpipe calls,
returning a data.frame of the output data
}
\examples{
\dontrun{
gorpipe <- get_gorpipe("~/gor-scripts/bin/gorpipe")
gorpipe("norrows 21 | calc even mod(rownum, 2) | calc even_txt if(even==1,'yes','no')")
}
}
