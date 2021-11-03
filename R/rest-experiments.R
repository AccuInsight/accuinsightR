set_run_info_s3 <- function (current_run_meta, user_sso_id) {
  # set_run_info

  const_val = accu.consts
  model_path <- ""
  json_path <- ""

  if (!is.null(current_run_meta[[const_val$RUN_INFO_BEST_MODEL_H5_PATH]]))
      model_path <- current_run_meta[[const_val$RUN_INFO_BEST_MODEL_H5_PATH]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_BEST_MODEL_JOBLIB_PATH]]))
      model_path <- current_run_meta[[const_val$RUN_INFO_BEST_MODEL_JOBLIB_PATH]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_BEST_MODEL_JSON_PATH]]))
      json_path = current_run_meta[[const_val$RUN_INFO_BEST_MODEL_JSON_PATH]]

  run_info <- new_accu_run_info(
    name=current_run_meta[[const_val$RUN_INFO_NAME]],
    userId=user_sso_id,
    creaDt=current_run_meta[[const_val$RUN_INFO_START_TIME]],
    endDt=current_run_meta[[const_val$RUN_INFO_END_TIME]],
    duration=current_run_meta[[const_val$RUN_INFO_DELTA_TIME]],
    afLoc="",
    status="FINISHED",
    note="",
    #path="sample/untitled1.R",
    path=current_run_meta[[const_val$RUN_INFO_MODEL_FILE_PATH]],
    modelPath=model_path,
    jsonPath=json_path
  )
  return(run_info)

}

set_run_info <- function (current_run_meta, user_sso_id) {
  # set_run_info

  const_val = accu.consts
  creaDt <- "1602554188390"
  endDt <- "1602554188390"
  duration <- "3"
  path <- ""
  model_path <- ""
  json_path <- ""

  #if (!is.null(current_run_meta[[const_val$RUN_INFO_BEST_MODEL_H5_PATH]]))
  #    model_path <- current_run_meta[[const_val$RUN_INFO_BEST_MODEL_H5_PATH]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_BEST_MODEL_RDA_PATH]]))
    model_path <- current_run_meta[[const_val$RUN_INFO_BEST_MODEL_RDA_PATH]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_BEST_MODEL_JSON_PATH]]))
    json_path = current_run_meta[[const_val$RUN_INFO_BEST_MODEL_JSON_PATH]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_START_TIME]]))
    creaDt = current_run_meta[[const_val$RUN_INFO_START_TIME]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_END_TIME]]))
    endDt = current_run_meta[[const_val$RUN_INFO_END_TIME]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_DELTA_TIME]]))
    duration = current_run_meta[[const_val$RUN_INFO_DELTA_TIME]]

  if (!is.null(current_run_meta[[const_val$RUN_INFO_MODEL_FILE_PATH]]))
    path = current_run_meta[[const_val$RUN_INFO_MODEL_FILE_PATH]]
  else
    tryCatch({path = current_Rfile <- rstudioapi::getSourceEditorContext()$path}, error = function(e) {print("current_Rfile couldn't be found")})
  
  run_info <- list(
    name = current_run_meta[[const_val$RUN_INFO_NAME]],
    userId = user_sso_id,
    creaDt = creaDt,
    endDt = endDt,
    duration = duration,
    afLoc = "",
    status = "FINISHED",
    note = "",
    path = path,
    modelPath = model_path,
    jsonPath = json_path
  )

  return(run_info)
}

call_run_parser <- function () {
  # python _run_parser()
  run_data <- run_parser(get_parser_type())
  return(run_data)
}

#' Create Experiment
#'
#' Creates an modeler experiment and returns its id.
#'
#' @param name The name of the experiment to create.
#' @param artifact_location Location where all artifacts for this experiment are stored. If
#'   not provided, the remote server will select an appropriate default.
#' @export
accu_create_experiment <- function(artifact_location = NULL, client = NULL) {
  # TODO refer lc_create_run() function in python
  accu_set_tracking_uri()
  const_val <- accu.consts
  client <- resolve_client(client)

  # read workspace environment
  # project_id, workspace_id, experiment_id and user_id
  env_value = get_os_env()
  # get current run meta data from [runs/run_info.json]
  # run_meta = get_current_run()
  accu_set_current_run()
  run_meta = accu_get_current_run()
  run_proto <- set_run_info(current_run_meta = run_meta,
               user_sso_id = env_value[[const_val$ENV_USER_SSO_ID]])
    git_meta = sprintf('{"url":"%s", "commit": "%s"}', "", "")
  git_meta_data = jsonlite::fromJSON(git_meta)
  # to get parameter and metric
  run_data <- call_run_parser()
  
  print(run_data)
  
  user_id = env_value[[const_val$ENV_USER_SSO_ID]]
  print(run_data$user_id)
  if (!is.null(run_data$user_id)) {
    user_id = run_data$user_id 
  }
  
  post_data = list(
    "project_id" = env_value[[const_val$ENV_PROJECT_ID]],
    "workspace_id" = env_value[[const_val$ENV_WORKSPACE_ID]],
    "experiment_id" = env_value[[const_val$ENV_EXPERIMENT_ID]],
    "userId" = user_id,
    "language_id" = env_value[[const_val$ENV_LANGUAGE_ID]],
    "run" = run_proto,
    "artifact" = run_data$artifact,
    "git" = git_meta_data,
    "metrics" = run_data$metrics,
    "dependency" = run_data$dependency,
    "parameter" = run_data$params$parameter,
    "feature" = run_data$feature$feature
    #"artifact_location" = "artifact_location" # TODO 추후 재확인 필요
    #"visuals" = run_data$visual, # TODO 추후 재확인 필요
  )
  print(jsonlite::toJSON(post_data, auto_unbox = TRUE))
  response <- accu_rest(
    "experiments", "create",
    client = client, verb = "POST",
    data = post_data,
    env_value = env_value
  )
  response$experiment_id
  return(post_data)
}
