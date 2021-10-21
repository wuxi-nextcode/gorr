# Workflow Service
# ------------------
# Service object for interfacing with the Workflow service API


#' Get a workflow job
#'
#' This is a description
#'
#' @param job_id job-id as integer or l
#' @param conn connection object, see \code{\link{platform_connect}}
#'
#' @return WorkflowJob object
#' @export
get_job <- function(job_id, conn){
    url <- gorr__get_endpoint(conn, "workflow-service", "jobs")

    body <- list(limit = 1)

    if (job_id == "latest") {
        body <- append(body, list(job_id = job_id))
    } else {
        tryCatch({
            body <- if (job_id%%1==0 ) append(body, list(job_id = job_id)) # Test if job_id is an integer
        }, error = function(err) {gorr__failure(sprintf("job_id must be an integer or 'latest', not '%s'", job_id))}
        )
    }

    resp <- gorr__api_request("GET",
                      url = url,
                      query = body,
                      conn)

    if (is.null(resp$jobs))
        gorr__failure("Job not found")


    WorkflowJob(resp$jobs[[1]], conn = conn)
}



#' Get a list of jobs satisfying the supplied criteria
#'
#' @param user_name The user who created the job
#' @param status Current status of jobs
#' @param project Filter by project
#' @param pipeline Filter by pipeline name
#' @param state Filter by state, each state encapsulates several statuses (running, finished)
#' @param context Filter by context string
#' @param limit Maximum number of results. Default 20
#'
#' @returns data.frame of jobs
#' @export
get_jobs <- function(conn,
                     user_name = NULL,
                     status = NULL,
                     project = NULL,
                     pipeline=NULL,
                     state = NULL,
                     context = NULL,
                     limit = 20) {
    start_time <- lubridate::now()

    body <- list(user_name = user_name,
                 status = status,
                 project_name = project,
                 pipeline_name = pipeline,
                 sate = state,
                 context = context,
                 limit = limit)


    resp <- gorr__api_request("GET",
                              url = gorr__get_endpoint(conn, "workflow-service", "jobs"),
                              query = body,
                              conn)

    jobs <- resp$jobs

    #gorr__info(sprintf("Retrieved %s jobs in %.2f sec", length(jobs), lubridate::now() - start_time))

    cols <- c("job_id", "pipeline_name", "user_name", "project_name", "submit_date", "cost_amount", "status")
    jobs %>%
        do.call(rbind, .)  %>%
        as.data.frame() %>%
        dplyr::select(tidyselect::all_of(cols))
}


#' Run a workflow job
#'
#' This is a low-level implementation on top of the workflow service's POST /jobs endpoint.
#'
#' @param pipeline_name Name of the pipeline to run
#' @param params Named list of parameters to forward to the job
#' @param conn connection object, see \code{\link{platform_connect}}
#'
#' @return WorkflowJob object
#' @export
post_job <- function(pipeline_name, params, conn){
    url <- gorr__get_endpoint(conn, "workflow-service", "jobs")
    body <- list(pipeline_name = pipeline_name,
                 project_name = conn$project,
                 parameters = params,
                 trace=FALSE)
    resp <- gorr__api_request("POST",
                      url = url,
                      body = body,
                      conn)
    WorkflowJob(resp, conn)
}


#' Returns the pipelines available on the current server
#'
#' Refer to the API documentation for the Workflow service to see formatting of data.
#'
#' @param conn connection object, see \code{\link{platform_connect}}
#' @param include.description Logical, should pipeline description be included in data.frame results. Default: False
#'
#' @return data.frame oor list of pipeline info
#' @export
get_pipelines <- function(conn, include.description = FALSE) {
    resp <- gorr::gorr__api_request("GET",
                              url = gorr__get_endpoint(conn, "workflow-service", "pipelines"),
                              conn)
    pipelines = resp$pipelines

    cols <- if (!include.description) c("name", "script", "revision") else c("name", "description", "script", "revision")
    pipelines %>%
        do.call(rbind, .)  %>%
        as.data.frame() %>%
        dplyr::select(tidyselect::all_of(cols))
}

