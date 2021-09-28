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
  # add env_path hard coding
  env_value = get_os_env()
  
  parser <- argparse::ArgumentParser()
  parser$add_argument("--workspaceRunId", help="ws run id", type="integer")
  parser$add_argument("--codePath", help="code path for running")
  parser$add_argument("--stopTimeout", help="workspace stop waiting timeout", type="integer", default=600)
  parser$add_argument("--argument", help="custom code arguments")
  
  argv <- parser$parse_args()
  
  if (!is.null(argv$argument)) {
    args <- gsub('\\[\\[:hyphen:\\]\\]', '--', gsub('\\[\\[:equal:\\]\\]', '=', gsub('\\[\\[:space:\\]\\]', ' ', argv$argument)))
  } else {
    args <- ''
  }
  
  library(subprocess)
  handle <- spawn_process('/usr/local/bin/Rscript', c(paste0('--workspaceRunId=', argv$workspaceRunId), paste('--args', args), gsub('\\[\\[:space:\\]\\]', ' ', argv$codePath), paste0('/tmp/output_', argv$workspaceRunId, '.log')))
  # handle <- spawn_process('/usr/local/bin/R', c('CMD', 'BATCH', paste0('--workspaceRunId=', argv$workspaceRunId), paste('--args', args), gsub('\\[\\[:space:\\]\\]', ' ', argv$codePath), paste0('/tmp/output_', argv$workspaceRunId, '.log')))
  while(process_state(handle)=='running') {
    Sys.sleep(1)
  }
  return_code <- process_return_code(handle)
  
  isSuccess <- "true"
  if (return_code != 0) {
    isSuccess <- "false"
  }
  
  post_data = list(
    "workspaceRunId" = argv$workspaceRunId,
    "stopTimeout" = argv$stopTimeout,
    "isSuccess" = isSuccess
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