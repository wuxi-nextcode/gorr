% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow_job.R
\name{workflow__is_running}
\alias{workflow__is_running}
\title{Is the job currently running. This is not a public function}
\usage{
workflow__is_running(job, refresh = FALSE)
}
\arguments{
\item{job}{WorkflowJob object, see \code{\link{post_job}} and \code{\link{get_job}}}

\item{refresh}{Logical, if object should be refresh if job not in running state}
}
\value{
TRUE / FALSE
}
\description{
Is the job currently running. This is not a public function
}
