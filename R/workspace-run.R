#' Create Workspace Run
#'
#' Runs given R code and returns its result.
#'
#' @param name The name of the experiment to create.
#' @export
accu_workspace_run <- function(client = NULL) {
  accu_set_tracking_uri()
  const_val <- accu.consts
  client <- resolve_client(client)
  
  # read workspace environment
  # project_id, workspace_id
  env_value = get_os_env()
  
  library(argparse)
  parser <- ArgumentParser()
  parser$add_argument("--workspaceRunId", help="ws run id", type="integer")
  parser$add_argument("--codePath", help="code path for running")
  parser$add_argument("--stopFlag", help="if workspace is stopped after running code")
  parser$add_argument("--stopTimeout", help="workspace stop waiting timeout", type="integer", default=60)
  
  argv <- parser$parse_args()
  
  library(subprocess)
  library(glue)
  
  handle <- spawn_process('/usr/local/bin/R', c('CMD', 'BATCH', argv$codePath, glue('/tmp/output_', argv$workspaceRunId, '.log')))
  while(process_state(handle)=='running') {
    Sys.sleep(1)
  }
  return_code <- process_return_code(handle)
  
  is_success <- TRUE
  if (return_code != 0) {
    is_success <- FALSE
  }
  
  post_data = list(
    "workspace_run_id" = argv$workspaceRunId,
    "stop_flag" = argv$stopFlag,
    "stop_timeout" = argv$stopTimeout,
    "is_success" = is_success
  )
  
  print(jsonlite::toJSON(post_data, auto_unbox = TRUE))
  response <- accu_ws_run_rest(
    mode = "afterRun",
    client = client, verb = "POST",
    data = post_data,
    env_value = env_value
  )
  
  return(post_data)
}