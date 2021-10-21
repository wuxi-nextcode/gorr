#' WorkflowJob class constructor
#'
#' Proxy object for a server side workflow job.
#'
#' This object can be queried for the current status of a workflow job and will
#' automatically refresh from server until the job is finished (or failed)
#'
#' @param response workflow-api job response
#' @param conn connection object, see \code{\link{platform_connect}}
#'
WorkflowJob <- function(response, conn) {
    structure(response, class = "WorkflowJob", conn = conn)
}

#' @export
print.WorkflowJob<- function(x, ...) {

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("WorkflowJob"))

    params_exclude <- c("csa_api_endpoint", "csa_api_password","csa_api_user", "csa_base", "csa_env", "project_path", "user_id", "project_name", "job_id")
    nextflow_params <- x$details$launch_config$nextflow_params
    input_params <- subset(nextflow_params, !(names(nextflow_params) %in% params_exclude))

    bullet("$name: ", x$name)
    bullet("$description: ", x$description)
    bullet("$job_id: ", x$job_id)
    bullet("$pipeline_name: ", x$pipeline_name)
    bullet("$status: ", x$status)
    bullet("$user_name: ", x$user_name)
    bullet("Input Parameters: ", paste0("\n\t", paste(names(input_params), input_params, sep = ": ", collapse = "\n\t")))
}


INIT_STATUSES <- c("PENDING")
RUNNING_STATUSES <- c("PENDING", "STARTED")
FAILED_STATUSES <-  c("ERROR", "KILLED")
FINISHED_STATUSES <- c(c("CANCELLED", "COMPLETED"), FAILED_STATUSES)



#' Refresh the local object of the serverside job object
#'
#' @param job WorkflowJob object
#'
#' @returns WorkflowJob object
#' @export
workflow_refresh <- function(job) {
    resp <- gorr__api_request("GET",
                              url = job$links$self,
                              conn = attr(job, which = "conn"))
    WorkflowJob(resp, conn = attr(job, which = "conn"))

}

#' Wait for a running job to complete.
#'
#' This is similar to the wait method in the Query Service and will wait by default
#' indefinitely for a workflow job to complete and poll the server regularly to update
#' the local status. When the job is completed (or failed) the method will return the
#' workflow job object.
#'
#' @param job WorkflowJob object, see \code{\link{post_job}} and \code{\link{get_job}}
#' @param max_seconds raise an exception if the job runs longer than this
#' @param poll_period Number of seconds to wait between polling (max 10 seconds)
#'
#' @return WorkflowJob object
#' @export
workflow_wait <- function(job, max_seconds = NULL, poll_period = .5) {
    start_time <- lubridate::now()

    spinner <- if (interactive()) gorr__spinner else invisible

    job <- workflow_refresh(job)
    is_running <- workflow__is_running(job)

    if (!is_running) {
        gorr__info(sprintf("Job %s not running - job status: %s", job$job_id, job$status))
        return(job)
    }

    tryCatch({
        while (is_running) {
            elapsed <- lubridate::now() - start_time
            spinner(gorr__elapsed_time(elapsed, status = "RUNNING", info = sprintf("Current job status: %s", job$status))) # Print progress to cli
            Sys.sleep(poll_period)

            job <- workflow_refresh(job)
            is_running <- workflow__is_running(job)

            # cancel the wait if the executor pod is in trouble after 30 seconds of waiting to start.
            # it most likely means that the nextflow script has a syntax error or something.
            if (job$status == "PENDING" && ( !is.null(max_seconds) && elapsed > max_seconds || elapsed > 30.0 )) {
                gorr__info(sprintf("Job has been PENDING for %.0fsec. Inspecting it...", elapsed))

                curr_status <- workflow_inspect(job)
                executor_pod <- NULL

                for (pod in curr_status$pods) {
                    if (pod$metadata$labels$`app-name` == "nextflow-executor") {
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

            if (job$status %in% RUNNING_STATUSES && !is.null(max_seconds) && elapsed > max_seconds)
                gorr__failure(sprintf("Job %s has exceeded wait time %.0fs and we will not wait any longer. It is currently %s.",
                                      job$job_id, max_seconds, job$status))

            poll_period = min(poll_period + 0.5, 10.0)

        }

        if (job$status == "DONE") {
            gorr__info(sprintf(
                "Job %s completed in %.2f sec and returned %s rows",
                job$job_id,
                elapsed,
                job$line_count
            ))
        } else {
            gorr__info(sprintf(
                "Job %s has status %s after %.2f sec",
                job$job_id,
                job$status,
                elapsed
            ))
        }

    }, interrupt = function(err) {gorr__warning("Code interrupted")}
    )

    WorkflowJob(job, conn = attr(job, which = "conn"))
}

#' Get current status of job
#'
#' @param job WorkflowJob object, see \code{\link{post_job}} and \code{\link{get_job}}
#'
#' @return Job status
#' @export
workflow_check_status <- function(job) {
    workflow_refresh(job)$status
}


#' Inspect a failed job for debugging.
#'
#' Returns unfiltered pod and node information from the kubernetes system.
#' Raises error if the server is not configured for inspection capabilities
#'
#' @param job WorkflowJob object, see \code{\link{post_job}} and \code{\link{get_job}}
#'
#' @return List containing low-level debugging information
#' @export
workflow_inspect <- function(job) {
    tryCatch({
        gorr__api_request("GET",
                          url = job$links$inspect,
                          conn = attr(job, which = "conn"))
    },
    error = function(err)  gorr__failure("Server does not support inspect functionality")
    )
}


#' Is the job currently running. This is not a public function
#'
#' @param job WorkflowJob object, see \code{\link{post_job}} and \code{\link{get_job}}
#' @param refresh Logical, if object should be refresh if job not in running state
#'
#' @returns TRUE / FALSE
#' @export
workflow__is_running <- function(job, refresh=FALSE) {
    if (job$status %in% RUNNING_STATUSES)
        job <- workflow_refresh(job)
    job$status %in% RUNNING_STATUSES
}

