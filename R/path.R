# path.r
# function: get_file_path
# 모델 학습시 생성된 결과물들을 용도에 따라 다른 폴더를 구성하여 파일을 저장하기 위해 경로를 생성하는 함수

#source('get.R')
#source('os_getenv.R')

#os_getenv.get_runs_path()
#get_runs_path()

create_path = function(path) {
  dir.create(file.path(path))
  Sys.chmod(save_model_path, "0777", use_umask = FALSE)
}

#' @export
get_file_path = function(model_name = NULL, root_path = RUN_ROOT_PATH){
  if(is.null(model_name) == FALSE){
    # 기존 path 저장 - path 변경으로 인한 에러 방지
    origial_path = getwd()
    # data 저장 상위 폴더
    home = get_runs_path()
    #home = '/Users/park/Desktop/R-modeler'
    setwd(home)

    prefix_path = paste0('results', '-', modelMethod(model_name), '/')

    if (file.exists(prefix_path) == FALSE){
      create_path(prefix_path)}
    setwd(prefix_path)

    # make a dir, 'best-model'
    save_best_model = 'best-model'   # 학습한 모델 저장
    save_model_path = paste0(home,'/',save_best_model,'/')

    if (file.exists(save_model_path) == FALSE){
      create_path(save_model_path)}

    # 모델별 path 구성 및 디렉토리 생성
    json_path_model = 'model-info-json'    # 모델 관련 정보 저장- json
    json_path_visual = 'for-visual-json'   # classification의 경우, josn path 하나 더 생성됨
    json_path_shap = 'shap-value-json'     # shap_value(feature_contribution) 저장 - json



    if(modelType(model_name)=='Classification'){
      path_list=c(json_path_model, json_path_visual, json_path_shap)}
    if(modelType(model_name)=='Regression'){
      path_list=c(json_path_model, json_path_shap)}

    for(i in 1:length(path_list))if(file.exists(path_list[i])==FALSE){create_path(path_list[i])}

    # 모델별 trial_num 생성
    trial_num = trial_number(paste0(getwd(), '/'))

    # 최종 path
    json_file_name_model = paste0(json_path_model, '/', modelType(model_name), '-', trial_num, '.json')
    json_file_name_shap = paste0(json_path_shap, '/', modelType(model_name), '-shap-', trial_num, '.json')
    if(modelType(model_name)=='Classification'){
      json_file_name_visual = paste0(json_path_visual, '/', modelType(model_name), '-visual-', trial_num, '.json')

      dir_list = list()
      dir_list$home_path = home
      dir_list$prefix_path = prefix_path
      dir_list$save_model_dir = save_best_model
      dir_list$save_model_rda = save_model_path
      dir_list$model_json = json_file_name_model
      dir_list$visual_json = json_file_name_visual
      dir_list$shap_json = json_file_name_shap
      dir_list$model_json_full = paste0(home, '/', prefix_path, json_file_name_model)
      dir_list$visual_json_full = paste0(home, '/', prefix_path, json_file_name_visual)
      dir_list$shap_json_full = paste0(home, '/', prefix_path, json_file_name_shap)}
    if(modelType(model_name)=='Regression'){
      dir_list = list()
      dir_list$home_path = home
      dir_list$prefix_path = prefix_path
      dir_list$save_model_dir = save_best_model
      dir_list$save_model_rda = save_model_path
      dir_list$model_json = json_file_name_model
      dir_list$shap_json = json_file_name_shap
      dir_list$model_json_full = paste0(home, '/', prefix_path, json_file_name_model)
      dir_list$shap_json_full = paste0(home, '/', prefix_path, json_file_name_shap)}

    setwd(origial_path)

  }else{
    # data 저장 상위 폴더
    home = get_runs_path()
    #home = '/Users/park/Desktop/R-modeler'
    # make a dir, 'best-model'
    save_best_model = 'best-model'   # 학습한 모델 저장
    save_model_path = paste0(home,'/',save_best_model,'/')

    if (file.exists(save_model_path) == FALSE){create_path(save_model_path)}
    dir_list = list()
    dir_list$home = home
    dir_list$save_model_dir = save_best_model
    dir_list$save_model_rda = save_model_path
  }
  return(dir_list)
}


#get_file_path(SVM_model)


#' @export
get_current_file_path = function(model_name){
  # 기존 path 저장 - path 변경으로 인한 에러 방지
  origial_path = getwd()

  # data 저장 상위 폴더
  home = get_runs_path()
  #home = '/Users/park/Desktop/R-modeler'
  setwd(home)

  prefix_path = paste0('results', '-', modelMethod(model_name), '/')

  if (file.exists(prefix_path) == FALSE){
    create_path(prefix_path)}

  setwd(prefix_path)

  # make a dir, 'best-model'
  save_best_model = 'best-model'   # 학습한 모델 저장
  save_model_path = paste0(home,'/',save_best_model,'/')

  if (file.exists(save_model_path) == FALSE){
    create_path(save_model_path)}

  # 모델별 path 구성 및 디렉토리 생성
  json_path_model = 'model-info-json'    # 모델 관련 정보 저장- json
  json_path_visual = 'for-visual-json'   # classification의 경우, josn path 하나 더 생성됨
  json_path_shap = 'shap-value-json'     # shap_value(feature_contribution) 저장 - json



  if(modelType(model_name)=='Classification'){
    path_list=c(json_path_model, json_path_visual, json_path_shap)}
  if(modelType(model_name)=='Regression'){
    path_list=c(json_path_model, json_path_shap)}

  for(i in 1:length(path_list))if(file.exists(path_list[i])==FALSE){create_path(path_list[i])}

  # 모델별 trial_num 생성
  trial_num = current_trial_number(paste0(getwd(), '/'))
  print(getwd())

  # 최종 path
  json_file_name_model = paste0(json_path_model, '/', modelType(model_name), '-', trial_num, '.json')
  json_file_name_shap = paste0(json_path_shap, '/', modelType(model_name), '-shap-', trial_num, '.json')
  if(modelType(model_name)=='Classification'){
    json_file_name_visual = paste0(json_path_visual, '/', modelType(model_name), '-visual-', trial_num, '.json')

    dir_list = list()
    dir_list$home_path = home
    dir_list$prefix_path = prefix_path
    dir_list$save_model_dir = save_best_model
    dir_list$save_model_rda = save_model_path
    dir_list$model_json = json_file_name_model
    dir_list$visual_json = json_file_name_visual
    dir_list$shap_json = json_file_name_shap
    dir_list$model_json_full = paste0(home, '/', prefix_path, json_file_name_model)
    dir_list$visual_json_full = paste0(home, '/', prefix_path, json_file_name_visual)
    dir_list$shap_json_full = paste0(home, '/', prefix_path, json_file_name_shap)}
  if(modelType(model_name)=='Regression'){
    dir_list = list()
    dir_list$home_path = home
    dir_list$prefix_path = prefix_path
    dir_list$save_model_dir = save_best_model
    dir_list$save_model_rda = save_model_path
    dir_list$model_json = json_file_name_model
    dir_list$shap_json = json_file_name_shap
    dir_list$model_json_full = paste0(home, '/', prefix_path, json_file_name_model)
    dir_list$shap_json_full = paste0(home, '/', prefix_path, json_file_name_shap)}

  setwd(origial_path)
  return(dir_list)
}

