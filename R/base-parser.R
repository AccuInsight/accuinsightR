ModelType <- function(enum = c(
  "DL_REGRESSION",
  "DL_CLASSIFICATION",
  "ML_REGRESSION",
  "ML_CLASSIFICATION"
)) {
    enumArg <-
      switch(
        match.arg(enum), "DL_REGRESSION" = 1L, "DL_CLASSIFICATION" = 2L, "ML_REGRESSION" = 3L, "ML_CLASSIFICATION" = 4L
      )
  }


get_parser_type <- function(run_info_json) {
  parser_type = NA
  const_val = accu.consts
  # run_info_json = get_current_run()
  run_result_path = run_info_json[[const_val$RUN_INFO_RESULT_PATH]]
  visual_json_path = NA
  visual_csv_path = NA

  if (exists(const_val$RUN_INFO_MODEL_VISUAL_JSON_PATH, run_result_path)) {
    visual_json_path = run_result_path[[const_val$RUN_INFO_MODEL_VISUAL_JSON_PATH]]
  }

  if (exists(const_val$RUN_INFO_MODEL_VISUAL_CSV_PATH, run_result_path)) {
    visual_csv_path = run_result_path[[const_val$RUN_INFO_MODEL_VISUAL_CSV_PATH]]
  }

  # determine parser type
  if (is.na(visual_json_path) && !is.na(visual_csv_path)) {
    parser_type = ModelType("DL_REGRESSION")
  }

  if (!is.na(visual_json_path) && !is.na(visual_csv_path)) {
    parser_type = ModelType("DL_CLASSIFICATION")

    #input_path = normalizePath(visual_json_path)
    input_path = file.path(visual_json_path)
    visual_json_data = accu_parse_json(input_path = input_path, json_format="records")

    if (exists(const_val$TRUE_Y, visual_json_data)) {
      parser_type = ModelType("DL_REGRESSION")
    }
  }

  if (is.na(visual_json_path) && is.na(visual_csv_path)) {
    parser_type = ModelType("ML_REGRESSION")
  }

  if (!is.na(visual_json_path) && is.na(visual_csv_path)) {
    parser_type = ModelType("ML_CLASSIFICATION")
  }

  return(parser_type)
}

run_parser <- function(parser_type, run_info_json) {
  # parser ML/DL results
  if (parser_type == ModelType("DL_REGRESSION")) {
    return(parse_run_result_dlreg(run_info_json))
  }

  if (parser_type == ModelType("DL_CLASSIFICATION")) {
    return(parse_run_result_dl_class(run_info_json))
  }

  if (parser_type == ModelType("ML_REGRESSION")) {
    return(parse_run_result_mlreg(run_info_json))
  }

  if (parser_type == ModelType("ML_CLASSIFICATION")) {
      return(parse_run_result_mlclass(run_info_json))
  }
}

