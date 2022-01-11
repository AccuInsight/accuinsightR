#library(stringi)

#' Parse ml classification
#'
#' @export
parse_run_result_mlclass <- function(run_info_json) {
  # parse ml classfication
  const_val <- accu.consts
  result_dict <- list(
    metrics= NA,
    params= NA,
    visual= NA,
    artifact= NA,
    dependency = NA,
    feature = NA
  )

  # get model_json_full from run_info.json [.result_path.model_json_full]
  data_path <- get_model_json_full(run_info_json)
  feature_data_path <- get_shap_json_full(run_info_json)

  # open file(from run_info.json [.result_path.model_json_full]) and parse json
  json_data <- accu_parse_json(input_path = data_path, json_format="records")
  params_data <- parse_run_selected_parameters(jsonData = json_data, constVal = const_val)
  metrics_data <- parse_run_selected_metrics(jsonData = json_data, constVal = const_val)
  visual_data <- parse_run_visual(jsonData = json_data, run_info_json = run_info_json) # read result_path.visual_json_full from run_info.json
  artifact_data <- parse_run_artifact(jsonData = json_data)
  dependency <- parse_used_library(jsonData = json_data)
  install_package <- get_install_package(isUsage = TRUE) # TRUE : Only usage list, FALSE : All package list
  feature_json_data <- accu_parse_json(input_path = feature_data_path, json_format="records")
  feature_data <- parse_run_selected_feature(jsonData = feature_json_data, constVal = const_val)

  result_dict$metrics <- metrics_data
  result_dict$params <- params_data
  result_dict$visual <- visual_data
  result_dict$artifact <- artifact_data$artifact
  result_dict$metrics <- c(metrics_data, visual_data) # Classification 의 경우는 visual data 를 붙임
  result_dict$dependency <- install_package$dependency # install 된 모든 패키지를 전달
  result_dict$feature <- feature_data
  return(result_dict)
}
