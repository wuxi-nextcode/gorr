% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprec-gor_connect.R
\name{get_jwt_token_payload}
\alias{get_jwt_token_payload}
\title{Decode a JWT token and extract payload}
\usage{
get_jwt_token_payload(jwt_token)
}
\arguments{
\item{jwt_token}{json web token string}
}
\value{
payload json structure as R structure, created with \code{\link[jsonlite]{fromJSON}}
}
\description{
Decode a JWT token and extract payload
}
