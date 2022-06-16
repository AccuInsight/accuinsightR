#' Define constangs values
#'
#' @export
accu.consts <- list(
  RUNS_ROOT="runs",
  EXPERIMENT_INFO_FILE=".env",
  EXPERIMENT_INFO_FILE_DIR="/home/rstudio",
  ENV_PROJECT_ID="LC_PROJECT_ID",
  ENV_WORKSPACE_ID="LC_WORKSPACE_ID",
  ENV_EXPERIMENT_ID="LC_EXPERIMENT_ID",
  ENV_USER_SSO_ID="LC_USER_SSO_ID",
  ENV_LANGUAGE_ID="LC_LANGUAGE_ID",
  ENV_BACK_END_API_URL="ENV_BACK_END_API_URL",
  ENV_BACK_END_API_PORT="ENV_BACK_END_API_PORT",

  # from run_info.json
  RUN_INFO_JSON_FILE = "run_info.json",
  RUN_INFO_START_TIME = "start_time",
  RUN_INFO_END_TIME = "end_time",
  RUN_INFO_DELTA_TIME = "delta_time",
  RUN_INFO_MODEL_FILE_PATH = "path",
  RUN_INFO_NAME = "run_name",
  RUN_INFO_RESULT_PATH = "result_path",
  RUN_INFO_MODEL_JSON_PATH = "model_json_full",
  RUN_INFO_MODEL_VISUAL_JSON_PATH = "visual_json_full",
  RUN_INFO_SHAP_JSON_PATH = "shap_json_full",
  RUN_INFO_MODEL_VISUAL_CSV_PATH = "visual_csv_full",
  RUN_INFO_BEST_MODEL_RDA_PATH = 'save_best_model_rda_path',
  RUN_INFO_BEST_MODEL_JSON_PATH = 'save_best_model_json_path',
  TRUE_Y = "True_y",
  PREDICTED_Y = "Predicted_y",

  # from model-info-json/xxx.json
  ALL_MODEL_PARAMS = "all_model_params",
  SELECTED_PARAMS = "selected_params",
  RUN_OBJ_DATA_VERSION = "data_version",
  SELECTED_METRICS = "selected_metrics",
  USED_LIBRARY = "used_library",

  BACK_END_API_URL = 'http://modeler-api.accu:8080/',
  BACK_END_API_URI = ''
)
