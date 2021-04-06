# get model & visual info.
#library(dummies)
#library(multiROC)

#--- caret -------------------------------------------------------------------

# used_library
#' @export
modelLibrary = function(model_name){
  model_info = model_name[grepl('modelInfo', names(model_name))]$modelInfo
  used_library = model_info[grepl('library', names(model_info))]$library
  return(used_library)
}

# fitted_model
#' @export
modelMethod = function(model_name){
  method = model_name[grepl('method', names(model_name))]$method
  return(method)
}

# model_type: regression / classification
#' @export
modelType = function(model_name){
  type = model_name[grepl('modelType', names(model_name))]$modelType
  return(type)
}

# selected_parameters
#' @export
tunedParams = function(model_name){
  param_name = names(model_name[grepl('bestTune', names(model_name))]$bestTune)
  tuned_params = list()
  for(i in 1:length(param_name)){
    tuned_params[[i]] = as.numeric(model_name[grepl('bestTune', names(model_name))]$bestTune)[i]
    names(tuned_params)[[i]] = param_name[i]
  }
  return(tuned_params)
}


# selected_metric
#' @export
metricType = function(model_name){
  metric = model_name[grepl('metric', names(model_name))]$metric
  return(metric)
}

# metric_value
#' @export
colMax <- function(data) sapply(data, max, na.rm = TRUE)

metricValue = function(model_name){
  res = model_name[grepl('results', names(model_name))]$results
  maxVal = colMax(res[metricType(model_name)])
  return(as.numeric(maxVal))
}

# feature name -- for shap
#' @export
featureName = function(model_name){
  feature_name = model_name[grepl('coefnames', names(model_name))]$coefnames
  return(feature_name)
}

# x_columns(independent variables)
#' @export
xCol = function(model_name){
  data = model_name[grepl('trainingData', names(model_name))]$trainingData
  x_col = subset(data, select=-c(.outcome))
  return(x_col)
}

# y_column(dependent variable)
#' @export
yCol = function(model_name){
  data = model_name[grepl('trainingData', names(model_name))]$trainingData  #in case of caret package
  y_col = data[,'.outcome']
  return(y_col)
}

# predicted_value
#' @export
predProb = function(model_name, test_data){
  pred = data.frame(predict(model_name, newdata = test_data, type = 'prob'))
  colnames(pred) = paste0(names(pred), '_pred_pr')
  return(pred)
}

#' @export
trueDummy = function(model_name, test_data){
  y_label = as.character(model_name[grepl('call', names(model_name))]$call$form)[2]
  true = dummies::dummy(test_data[, grepl(y_label, names(test_data))], sep = '_')
  colnames(true) = gsub(".*?\\_", "", colnames(true))
  colnames(true) = paste0(colnames(true), "_true")
  return(true)
}

# final_df_for_roc
#' @export
finalDF = function(model_name, test_data){
  true = predProb(model_name, test_data)
  pred = trueDummy(model_name, test_data)
  df = cbind(true, pred)
  return(df)
}

# roc, pr curve
#' @export
roc_pr = function(visual_data){
  roc = multiROC::multi_roc(visual_data, force_diag = T)
  pr = multiROC::multi_pr(visual_data, force_diag = T)
  visual_label = names(roc$Specificity$pr)
  return(list(roc=roc, pr=pr, visual_label=visual_label))
  }

# trial_num: for file path
#' @export
trial_number = function(path){   # path: '~/result-model/'
  file_list = list.files(path)
  json_list = list.files(paste0(path, file_list))

  if(length(json_list)==0){
    current_num = 1
  }else{
    exist_nums = as.numeric(stringr::str_extract(json_list, '[0-9]+'))
    max_num = max(exist_nums)
    current_num = max_num+1
  }
  return(current_num)
}

# current_trial_num: for file path
#' @export
current_trial_number = function(path){   # path: '~/result-model/'
  file_list = list.files(path)
  json_list = list.files(paste0(path, file_list))

  exist_nums = as.numeric(stringr::str_extract(json_list, '[0-9]+'))
  max_num = max(exist_nums)
  current_num = max_num

  return(current_num)
}


#--- mlr -------------------------------------------------------------------
#' @export
modelLibrary_mlr = function(model_train_name){
  model_learner = train_name[grepl('learner', names(train_name))]$learner
  used_library = model_learner[grepl('package', names(model_learner))]$package
  return(used_library)
}
