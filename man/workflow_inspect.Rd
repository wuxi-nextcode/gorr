% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow_job.R
\name{workflow_inspect}
\alias{workflow_inspect}
\title{Inspect a failed job for debugging.}
\usage{
workflow_inspect(job)
}
\arguments{
\item{job}{WorkflowJob object, see \code{\link{post_job}} and \code{\link{get_job}}}
}
\value{
List containing low-level debugging information
}
\description{
Returns unfiltered pod and node information from the kubernetes system.
Raises error if the server is not configured for inspection capabilities
}
