#source('get.R')
#source('path.R')

# save a model

#' @export
saveModel = function(model_name, run_id){
  dir_list = get_file_path()
  save_model_path = paste0(dir_list[grepl('save_model_rda', names(dir_list))]$save_model_rda,
                           modelMethod(model_name), modelType(model_name), '-',
                           run_id, '.rda')
  saveRDS(model_name, file = save_model_path)
  Sys.chmod(save_model_path, mode = "0666", use_umask = FALSE)
}


# load a saved model
#' @export
loadModel = function(model_tag){
  dir_list = get_file_path()
  load_model_path = paste0(dir_list[grepl('save_model_rda', names(dir_list))]$save_model_rda,
                           model_tag)
  readRDS(file = load_model_path)
}


#saveModel(SVM_model,'1111')
#loadModel('svmRadialClassification-1111.rda')
