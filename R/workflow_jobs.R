#' WorkflowJob class constructor
#'
#' Proxy object for a server side workflow job.
#'
#' This object can be queried for the current status of a workflow job and will
#' automatically refresh from server until the job is finished (or failed)
WorkflowJob <- function(response, conn) {
    structure(response, class = "WorkflowJob", conn = conn)
}

#' @export
print.WorkflowJob<- function(x, ...) {

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("WorkflowJob"))

    bullet("$name: ", x$name)
    bullet("$description: ", x$description)
    bullet("$run_id: ", x$run_id)
    bullet("$pipeline_name: ", x$pipeline_name)
    bullet("$status: ", x$status)
    bullet("$user_name: ", x$user_name)
    bullet("Parameters: ",x$details$launch_config$run_args)
}


INIT_STATUSES <- c("PENDING")
RUNNING_STATUSES <- c("PENDING", "STARTED")
FAILED_STATUSES <-  c("ERROR", "KILLED")
FINISHED_STATUSES <- c(c("CANCELLED", "COMPLETED"), FAILED_STATUSES)



#' Refresh the local cache of the serverside job object
workflow_refresh <- function(job) {
    resp <- gorr__api_request("GET",
                              url = job$links$self,
                              conn)
    WorkflowJob(resp, conn = conn)

}

#' Wait for a running job to complete.
#' This is similar to the wait method in the Query Service and will wait by default
#' indefinitely for a workflow job to complete and poll the server regularly to update
#' the local status. When the job is completed (or failed) the method will return the
#' workflow job object.
#'
#' :param max_seconds: raise an exception if the job runs longer than this
#' :param poll_period: Number of seconds to wait between polling (max 10 seconds)
#' :returns: WorkflowJob
#' :raises: :exc:`JobError`
#'
#' @param  max_seconds: raise an exception if the job runs longer than this
#' @param poll_period: Number of seconds to wait between polling (max 10 seconds)
#' @param conn connection object, see \code{\link{platform_connect}}
#'
#' @return WorkflowJob object
#' @export
workflow_wait <- function(job, max_seconds = NULL, poll_period = .5) {
    start_time <- lubridate::now()

    spinner <- if (interactive()) gorr__spinner else invisible

    tryCatch({
        repeat {
            elapsed <- lubridate::now() - start_time
            Sys.sleep(poll_period)
            status_response <- workflow_refresh(job)


            # cancel the wait if the executor pod is in trouble after 30 seconds of waiting to start.
            # it most likely means that the nextflow script has a syntax error or something.
            if (status_response$status == "PENDING" && ( !is.null(max_seconds) && elapsed > max_seconds || elapsed > 30.0 )) {
                gorr__info(sprintf("Job has been PENDING for %.0fsec. Inspecting it...", elapsed))

                curr_status <- workflow_inspect(job)
                executor_pod <- NULL

                for (pod in curr_status$pods) {
                    if (pod$metadata$labels$app-name == "nextflow-executor") {
                        executor_pod <- pod
                        break
                    }
                }

                if (is.null(executor_pod))
                    gorr__failure(sprintf("Job is still pending after %.0fs and executor pod is missing. View logs or inspect job for failures.",
                                          elapsed))


                if (is.null(executor_pod$status$container_statuses[[1]]$state$running))
                    gorr__failure(sprintf("Job is still pending after %.0fs and executor pod is not in running state. View logs or inspect job for failures.",
                                          elapsed))
            }

            if (status_response$status %in% RUNNING_STATUSES && !is.null(max_seconds) && elapsed > max_seconds)
                gorr__failure(sprintf("Job %s has exceeded wait time %.0fs and we will not wait any longer. It is currently %s.",
                                      job_id, max_seconds, status_response$status))

            poll_period = min(poll_period + 0.5, 10.0)

        }

        if (status_response$status == "DONE") {
            gorr__info(sprintf(
                "Job %s completed in %.2f sec and returned %s rows",
                status_response$job_id,
                elapsed,
                status_response$line_count
            ))
        } else {
            gorr__info(sprintf(
                "Job %s has status %s after %.2f sec",
                status_response$job_id,
                status_response$status,
                elapsed
            ))
        }

    }, interrupt = function(err) {gorr__warning("Code interrupted")}
    )

    WorkflowJob(status_response, conn=conn)
}


workflow_check_status <- function(job) {
    workflow_refresh(job)$status
}


#' Inspect a failed job for debugging.
#'
#' Returns unfiltered pod and node information from the kubernetes system.
#' Raises error if the server is not configured for inspection capabilities
#'
#' @return List containing low-level debugging information
#' @export
workflow_inspect <- function(job) {
    tryCatch({
        gorr__api_request("GET",
                          url = job$links$inspect,
                          conn)
    },
    error = function(err)  gorr__failure("Server does not support inspect functionality")
    )
}
