#' Parse ml regression
#'
#' @export
parse_run_result_mlreg <- function() {
  # parse ml regression
  const_val <- accu.consts

  result_dict <- list(
    metrics = NA,
    params = NA,
    artifact = NA,
    dependency = NA,
    feature = NA
  )

  # get model_json_full from run_info.json [.result_path.model_json_full]
  data_path <- get_model_json_full()
  feature_data_path <- get_shap_json_full()

  # open file(from run_info.json [.result_path.model_json_full]) and parse json
  json_data <- accu_parse_json(input_path = data_path, json_format="records")
  params_data <- parse_run_selected_parameters(jsonData = json_data, constVal = const_val)
  metrics_data <- parse_run_selected_metrics(jsonData = json_data, constVal = const_val)
  true_data <- parse_run_selected_true(jsonData = json_data, constVal = const_val)
  predicted_data <- parse_run_selected_preticted(jsonData = json_data, constVal = const_val)
  artifact_data <- parse_run_artifact(jsonData = json_data)
  dependency <- parse_used_library(jsonData = json_data)
  install_package <- get_install_package(isUsage = TRUE)
  feature_json_data <- accu_parse_json(input_path = feature_data_path, json_format="records")
  feature_data <- parse_run_selected_feature(jsonData = feature_json_data, constVal = const_val)

  result_dict$metrics <- metrics_data
  result_dict$params <- params_data
  result_dict$artifact <- artifact_data$artifact
  result_dict$metrics <- c(metrics_data, true_data, predicted_data) # Regression 의 경우 visual 대신에 true_y, predicted_y data 가 추가됨
  result_dict$dependency <- install_package$dependency
  result_dict$feature <- feature_data
  
  print(true_data)
  print(predicted_data)
  print(result_dict$metrics)

  return(result_dict)
}
