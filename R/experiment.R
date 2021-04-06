#-------------- add_experiment --------------------------------------------------
#' add_experiment Function
#'
#' Add you experiment to list of model experiments when using accuinsight modeler service.
#' @param model_name: your model name
#' @param testdata: test data for computing roc, pr
#' @export
#' @examples
add_experiment = function(model_name, traindata=NULL, testdata){

  model_info = list()
  Sys.setenv(TZ= "Asia/Seoul")

  run_id = uuid::UUIDgenerate()
  method = paste0(modelMethod(model_name), modelType(model_name))

  model_info$logging_time = Sys.time()                                     # logging_time
  model_info$logging_run_id = run_id                                       # logging_run_id
  model_info$used_library = modelLibrary(model_name)                       # used library
  model_info$fitted_model = method                                         # method - "svmRadialClassification"
  model_info$selected_params = tunedParams(model_name)
  model_info$selected_metrics$tmp = metricValue(model_name)
  names(model_info$selected_metrics) = metricType(model_name)

  dir_list = get_file_path(model_name)

  # classification - save a visual_info.json
  if(modelType(model_name) == 'Classification'){
    # model_info_json
    model_info_json = jsonlite::toJSON(model_info, pretty = T)
    model_json_full_path = dir_list$model_json_full
    #set_model_json_path = dir_list$model_json

    write(model_info_json, file = model_json_full_path)

    # visual_info_json
    visual_info_json = jsonlite::toJSON(visual_classification(model_name, testdata), pretty=T)
    visual_json_full_path = dir_list$visual_json_full
    #set_visual_json_path = dir_list$visual_json

    write(visual_info_json, file = visual_json_full_path)

    # shap_value_json
    #shap_value_json =
    #shap_json_full_path = dir_list$shap_json_full
    #set_shap_json_path = dir_list$shap_json

    #write(shap_value_json, file = shap_json_full_path)
    #message('Saving a shap_value_json.json is completed')
  }
  if(modelType(model_name) == 'Regression'){
    model_info$true_y = yCol(model_name)
    model_info$predicted_y = predict(model_name)

    # model_info_json
    model_info_json = jsonlite::toJSON(model_info, pretty = T)
    model_json_full_path = dir_list$model_json_full
    #set_model_json_path = dir_list$model_json

    write(model_info_json, file = model_json_full_path)

    # shap_value_json
    #shap_value_json =
    #shap_json_full_path = dir_list$shap_json_full
    #set_shap_json_path = dir_list$shap_json

    #write(shap_value_json, file = shap_json_full_path)
    #print('writing a shap_value_json.json is completed')
  }
  # model save
  path_for_setting_model_rda = paste0(dir_list$save_model_dir, '/',
                                      model_info$fitted_model, '-',
                                      model_info$logging_run_id, '.rda')
  #set_best_model_rda_path(path_for_setting_model_rda)

  # write a runs/run_info.json file
  current_dir_list = get_current_file_path(model_name)
  run_info_list = run_info(model_name, run_id, current_dir_list)
  run_info_json = jsonlite::toJSON(run_info_list, pretty = TRUE)


  write(run_info_json, file = get_run_info_json_path())

  saveModel(model_name, model_info$logging_run_id)
  accu_create_experiment()
}




#add_experiment_caret(SVM_model, test_data)

#true = trueDummy(SVM_model, test_data)
#pred = predProb(SVM_model, test_data)
#visual_data = finalDF(SVM_model, test_data)
#visual_data
#roc_pr(visual_data)
