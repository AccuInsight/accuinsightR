make_metrics <- function(
  key = "",
  values = list(),
  timestamp = list(),
  steps = list()) {
    list(
      "key" = key,
      "values" = values,
      "timestamp" = timestamp,
      "steps" = steps
      )
}

init_metric_data <- function () {
  init_list <- list("stpes" = list(), "values_list" = list(), "timestamp" = list())
}

set_metric_data <- function(jsonData) {
  result_dict <- list()
  metric_keys = names(jsonData)
  for (key in metric_keys) {
    data <- jsonData[[key]]
    result_dict <- c(result_dict,list(set_metric_values(data, key)))
  }
  return(result_dict)
}

set_metric_values <- function(jsonData, key) {
  result_dict <- list("key" = list("values" = list(), "timestamp" = list(), "steps" = list()))
  #metric_keys = names(jsonData)
  count <- 0
  values <- list()
  timestamp <- list()
  steps <- list()
  for (item in jsonData) {
    values <- c(values, toString(item))
    timestamp <- c(timestamp, toString(count))
    steps <- c(steps, toString(count))
    count <- count + 1
  }
  result_dict[["key"]] <- key
  result_dict[["values"]] <- values
  result_dict[["timestamp"]] <- timestamp
  result_dict[["steps"]] <- steps

  return(result_dict)
}

gen_metrics_object <- function (metrics, constVal) {
  grpc_metrics <- list()
  for (item in metrics) {
    grpc_metrics = c(grpc_metrics,
                     make_metrics(key = constVal$SELECTED_METRICS,
                                  values = item[["values"]],
                                  timestamp = item[["timestamp"]],
                                  steps = item[["steps"]])
                    )
  }

  return(grpc_metrics)
}

parse_run_selected_metrics <- function (jsonData, constVal) {
  # TODO check return format (parse_run_metrics)
  # read model-info-json/*.json
  # get "selected_metrics" field
  # return value
  # "metrics": [
  #   {
  #     "key": "string",
  #     "values": [],
  #     "timestamp": []
  #     "steps": []
  #   },
  #   ...
  #   {
  #     "key": "string",
  #     "values": [],
  #     "timestamp": []
  #     "steps": []
  #   },
  # ]

  # get selected_metrics
  selected_metrics <- jsonData[[constVal$SELECTED_METRICS]]
  result_dict <- set_metric_data(jsonData = selected_metrics)
  return(result_dict)
}

parse_run_selected_true <- function (jsonData, constVal) {
  true_y_data <- jsonData[[constVal$TRUE_Y]]
  result_data <- set_metric_values(jsonData = true_y_data, key = tolower(constVal$TRUE_Y))
  return(list(result_data))
}

parse_run_selected_predicted <- function (jsonData, constVal) {
  predicted_y_data <- jsonData[[constVal$PREDICTED_Y]]
  result_data <- set_metric_values(jsonData = predicted_y_data, key = tolower(constVal$PREDICTED_Y))
  return(list(result_data))
}

parse_run_selected_parameters <- function (jsonData, constVal) {
  param_list <- list("parameter" = list("parameter" = list()))
  # return format (parse_run_parameters)
  # return format
  # {
  #   "parameter": {
  #     "parameter": {
  #       "max_depth": "6",
  #       "learning_rate": "0.300000012"
  #     }
  #   }
  # }
  selected_params <- jsonData[[constVal$SELECTED_PARAMS]]
  keys = get_object_keys(selected_params)
  if (is.null(keys)) {
    all_model_params <- jsonData[[constVal$ALL_MODEL_PARAMS]]
    for (x in selected_params) {
      json_str <- sprintf('{"%s": "%s"}', x, toString((all_model_params[[x]])))

      param_list$parameter$parameter <- c(param_list$parameter$parameter, jsonlite::fromJSON(json_str))
    }
  } else {
    for (x in keys) {
      a = selected_params[x]
      json_str <- sprintf('{"%s": "%s"}', x, a[1] )
      param_list$parameter$parameter <- c(param_list$parameter$parameter, jsonlite::fromJSON(json_str))
    }
  }
  return(param_list)
}

get_data_version <- function (jsonData) {
  # return format
  # {
  #   "artifact": {
  #     "name": "data_version",
  #     "version": "v1.1"
  #   }
  # }
  const_val <- accu.consts

  json_str <- sprintf('{"artifact": {"name": "%s", "version": "%s"}}', const_val$RUN_OBJ_DATA_VERSION, "")
  if (exists(const_val$RUN_OBJ_DATA_VERSION, jsonData)) {
    json_str <- sprintf('{"artifact": {"name": "%s", "version": "%s"}}', const_val$RUN_OBJ_DATA_VERSION, jsonData[[const_val$RUN_OBJ_DATA_VERSION]])
  }

  return(jsonlite::fromJSON(json_str))
}

parse_run_artifact <- function (jsonData) {
  # get data version
  version_data <- get_data_version(jsonData = jsonData)
  return(version_data)
}


#set_used_libray_data <- function(array_data) {
#  #keys = list()
#  #ak = names(array_data)
#  #for (key in ak) {
#  #  keys[[key]] <- array_data[[key]]
#  #}
#  #result_dict[["dependency"]][["data"]] = keys
#  return(result_dict)
#}

parse_used_library <- function (jsonData) {
  #"used_library": ["kernlab"],
  #"dependency": {
  #  "data": [
  #    "numpy==1.19.0",
  #    "scikit-learn==0.23.1"
  #  ]
  #}
  const_val <- accu.consts
  result_dict <- list("dependency" = list("data" = list()))

  if (exists(const_val$USED_LIBRARY, jsonData)) {
    result_dict[["dependency"]][["data"]] = jsonData$used_library
  }

  return (result_dict)
}

parse_run_selected_feature <- function (jsonData, constVal) {
  feature_list <- list("feature" = list("importance" = list()))
  # return format
  # "feature": {
  #   "importance": {
  #     "speed": "0.7",
  #     "temp": "0.21"
  #   }
  # }
  keys = get_object_keys(jsonData)
  if (is.null(keys)) {
    feature_list <- list("feature" = list("importance" = list("data" = list())))
    feature_list$feature$importance$data <- NULL
  } else {
    for (x in keys) {
      a = jsonData[x]
      json_str <- sprintf('{"%s": "%s"}', x, a[1] )
      feature_list$feature$importance <- c(feature_list$feature$importance, jsonlite::fromJSON(json_str))
    }
  }
  return(feature_list)
}

get_install_package <- function (isUsage) {
  #return format
  #"dependency": {
  #  "data": [
  #    "numpy==1.19.0",
  #    "scikit-learn==0.23.1"
  #  ]
  #}
  result_dict <- list("dependency" = list("data" = list()))
  if(isUsage) {
    file_path = ''
    tryCatch({file_path = current_Rfile <- rstudioapi::getSourceEditorContext()$path}, error = function(e) {print("current_Rfile couldn't be found")})
    libraryList <- scan(file_path, what="", sep="\n")
    x <- libraryList[grep("library", libraryList)]
    if (is.null(x)) {
      result_dict <- list("dependency" = list("data" = list("data" = list())))
      result_dict$dependency$data$data <- NULL
    } else {
      packageNameList <- stringr::str_sub(x, 9, -2)
      for (i in packageNameList) {
        str <- sprintf('%s==%s', i, packageDescription(i)$Version)
        result_dict$dependency$data <- c(result_dict$dependency$data, str)
      }
    }
  } else {
    package_name <- as.character(installed.packages(priority='NA')[,c(1)])
    package_version <- as.character(installed.packages(priority='NA')[,c(3)])
    pkglist <- paste(package_name, package_version, sep = "==")
    result_dict[["dependency"]][["data"]] <- pkglist
  }

  print(jsonlite::toJSON(result_dict, auto_unbox = TRUE))
  return (result_dict)
}
