#' Read runs/run_info.json
get_current_run <- function (input_path = NULL) {
  const_val = accu.consts
  if (is.null(input_path)) {
    run_path <- get_runs_path()
    input_path = file.path(run_path, const_val$RUN_INFO_JSON_FILE)
  }
  # parse run_info.json
  jsonData <- accu_parse_json(input_path = input_path, json_format="records")
}

#' @export
get_run_info_json_path <- function(){
  const_val = accu.consts
  run_path <- get_runs_path()
  input_path = file.path(run_path, const_val$RUN_INFO_JSON_FILE)
  return(input_path)
}

get_target_path <- function (expected_path) {
  const_val <- accu.consts
  run_info_json <- accu_get_current_run()
  run_result_path <- run_info_json[[const_val$RUN_INFO_RESULT_PATH]]
  target_path <- run_result_path[[expected_path]]
  return(target_path)
}

get_model_json_full <- function () {
  const_val <- accu.consts
  target_path <- get_target_path(const_val$RUN_INFO_MODEL_JSON_PATH)
  return(target_path)
}

get_shap_json_full <- function () {
  const_val <- accu.consts
  target_path <- get_target_path(const_val$RUN_INFO_SHAP_JSON_PATH)
  return(target_path)
}

get_visual_json_full <- function () {
  const_val <- accu.consts
  target_path <- get_target_path(const_val$RUN_INFO_MODEL_VISUAL_JSON_PATH)
  return(target_path)
}

get_visual_csv_full <- function () {
  const_val <- accu.consts
  target_path <- get_target_path(const_val$RUN_INFO_MODEL_VISUAL_CSV_PATH)
  return(target_path)
}

