#library(caret)
#library(uuid)
#library(jsonlite)
#setwd('/Users/park/Desktop/R-modeler')
#source('get.R')
#source('get_visual_info.R')
#source('path.R')
#source('utils.R')

write_json = function(json_object, json_path) {
  write(json_object, file = json_path)
  Sys.chmod(json_path, mode = "0666", use_umask = FALSE)
}

#-------------- add_experiment_for_caret --------------------------------------------------
#' add_experiment Function
#'
#' Add you experiment to list of model experiments when using accuinsight modeler service.
#' @param model_name: your model name
#' @param test_data: test data for computing roc, pr
#' @export
#' @examples
add_experiment = function(model_name, test_data, user_id = NULL){
  model_info = list()
  Sys.setenv(TZ= "Asia/Seoul")

  run_id = uuid::UUIDgenerate()
  # method = paste0(modelMethod(model_name), modelType(model_name))

  model_info$user_id = user_id
  model_info$logging_time = Sys.time()                                     # logging_time
  model_info$logging_run_id = run_id                                       # logging_run_id
  model_info$used_library = modelLibrary(model_name)                 # used library
  model_info$fitted_model = modelMethod(model_name)                        # method - "svmRadialClassification"
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

    write_json(model_info_json, model_json_full_path)
    message('Saving a model_info_json.json is completed')     #######

    # visual_info_json
    visual_info_json = jsonlite::toJSON(visual_classification(model_name, test_data), pretty=T)
    visual_json_full_path = dir_list$visual_json_full
    #set_visual_json_path = dir_list$visual_json

    write_json(visual_info_json, visual_json_full_path)
    message('Saving a visual_info_json.json is completed')    #######

    # shap_value_json
    #shap_value_json =
    #shap_json_full_path = dir_list$shap_json_full
    #set_shap_json_path = dir_list$shap_json

    #write(shap_value_json, file = shap_json_full_path)
    #message('Saving a shap_value_json.json is completed')
  }
  if(modelType(model_name) == 'Regression'){
    model_info$True_y = yCol(model_name)
    model_info$Predicted_y = predict(model_name)

    # model_info_json
    model_info_json = jsonlite::toJSON(model_info, pretty = T)
    model_json_full_path = dir_list$model_json_full
    #set_model_json_path = dir_list$model_json

    write_json(model_info_json, model_json_full_path)
    message('Saving a model_info_json.json is completed')    #######

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
  run_name = paste0(model_info$fitted_model, '-', model_info$logging_run_id)
  run_info_list = run_info(model_name, run_name, current_dir_list)
  run_info_json = jsonlite::toJSON(run_info_list, pretty = TRUE)

  write_json(run_info_json, get_run_info_json_path())

  saveModel(model_name, model_info$logging_run_id)
  accu_create_experiment()
}


#add_experiment_caret(SVM_model, test_data)

#true = trueDummy(SVM_model, test_data)
#pred = predProb(SVM_model, test_data)
#visual_data = finalDF(SVM_model, test_data)
#visual_data
#roc_pr(visual_data)
