# writing a run_info.json

#' @export
run_info = function(model_name, run_id, current_path){
  run_info_lst = list()

  run_name_origin = paste0(modelMethod(model_name), modelType(model_name), '-', run_id)

  # key = root
  run_info_lst$run_object = NULL
  run_info_lst$run_name = paste0(modelMethod(model_name), modelType(model_name), '-', gsub('-', '', run_id))
  run_info_lst$base_path = current_path$home_path

  # key = result_path
  run_info_lst$result_path$base_path = current_path$home_path
  run_info_lst$result_path$prefix_path = current_path$prefix_path
  run_info_lst$result_path$model_json_full = current_path$model_json_full
  run_info_lst$result_path$shap_json_full = current_path$shap_json_full
  if(modelType(model_name) == 'Classification'){
    run_info_lst$result_path$visual_json_full = current_path$visual_json_full
  }
  run_info_lst$result_path$save_best_model_rda_path = paste0(current_path$save_model_rda, run_name_origin, '.rda')

  # key = model_json_path
  run_info_lst$model_json_path = current_path$model_json
  # key = visual_json_path
  run_info_lst$visual_json_path = current_path$visual_json
  # key = shap_json_path
  run_info_lst$shap_json_path = current_path$shap_json
  # key = save_best_model_rda_path
  run_info_lst$save_best_model_rda_path = strsplit(run_info_lst$result_path$save_best_model_rda_path, 'rstudio/')[[1]][2]

  # key = prefix_path
  run_info_lst$prefix_path = current_path$prefix_path

  return(run_info_lst)

}
