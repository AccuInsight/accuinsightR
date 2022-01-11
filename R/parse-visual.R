extract_metric_data_from_array <- function(idx, key, result_dict, visual_data) {
  inner_keys <- names(visual_data[[key]])
  timestamp = c(1:length(visual_data[[key]]))
  steps = inner_keys
  values_list = list()
  for (item in inner_keys) {
    data <- visual_data[[key]][[item]]
    # values_list = append(values_list, list(data), idx)

    #values_list = c(values_list, list(list(toString(data)) ))
    values_list = c(values_list, list(data))
    idx = idx + 1
  }
  result_dict[["metrics"]][["timestamp"]] <- timestamp
  result_dict[["metrics"]][["steps"]] <- steps
  result_dict[["metrics"]][["values"]] <- values_list

  return(result_dict)
}

extract_metric_data_from_mapped <- function (visual_data, key) {
  inner_keys <- names(visual_data[[key]])

  init_data <- init_metric_data()
  steps = init_data$steps
  values_list = init_data$values_list
  # timestamp = init_data$timestamp

  total_data_count = 0
  for (ik in inner_keys) {
    result_dict <- list("metrics" = list("values" = list(), "timestamp" = list(), "steps" = list() ) )

    temp_values = list()
    data <- visual_data[[key]][[ik]]
    total_data_count = total_data_count + length(names(data))

    for (item in names(data)) {
      steps = c(steps, paste(ik, item, sep = "_"))
      temp_values = c(temp_values, list(toString(data[[item]])))
    }

    values_list = c(values_list, list(temp_values))

    timestamp = c(1:length(steps))
    result_dict[["metrics"]][["timestamp"]] <- timestamp
    result_dict[["metrics"]][["steps"]] <- steps
    result_dict[["metrics"]][["values"]] <- values_list
  }

  return(result_dict)
}

set_extracted_visual <- function(grpc_metrics, key, result_dict) {
  metrics <- result_dict['metrics']
  for (item in metrics) {
    grpc_metrics = c(grpc_metrics,
                     list(make_metrics(key = key,
                                       values = item[["values"]],
                                       timestamp = item[["timestamp"]],
                                       steps = item[["steps"]]))
    )
  }
  return(grpc_metrics)
}

parse_mapped_data <- function (visual_data, mapped_keys) {
  # parse the below data
  # "legend": {
  # 	"roc": {
  # 		"0": 0.986111111111111,
  # 		"1": 0.9340277777777771,
  # 		"2": 0.9644097222222223,
  # 		"macro": 0.9681483318636093,
  # 		"micro": 0.9771999999999995
  # 	},
  # 	"precision-recall": {
  # 		"0": 1.0,
  # 		"1": 0.9445039714169319,
  # 		"2": 0.9463180496881478,
  # 		"macro": 0.9482649997968625,
  # 		"micro": 0.9570831563917747
  # 	}
  # }

  # return data
  #   {
  #     "key": "legend",
  #     "values": [[0.1, 0.2, 0.3, 0.4, 0.5], [0.1, 0.2, 0.3, 0.4, 0.5]],
  #     "timestamp": [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  #     "steps": ["roc_0", "roc_1", "roc_2", "roc_macro", "roc_micro",
  #               "precision-recall_0", "precision-recall_1", "precision-recall_2", "precision-recall_macro", "precision-recall_micro",
  #     ]
  #   },

  # result_dict <- list("metrics" = list("values" = list(), "timestamp" = list(), "steps" = list() ) )

  # get keys of visual_data
  # dict_keys(['fpr', 'tpr', 'roc_auc', 'recall', 'precision', 'average_precision', 'confusion_matrix'])
  keys_visual <- names(visual_data)


  # get only mapped_keys data from visual_data
  matched_index = match(mapped_keys, keys_visual)
  visual_data = visual_data[matched_index]
  keys_visual = keys_visual[matched_index]

  grpc_metrics <- list()
  for (key in keys_visual) {
    result_dict <- extract_metric_data_from_mapped(visual_data = visual_data, key = key)
    grpc_metrics <- set_extracted_visual(grpc_metrics, key, result_dict)
  }

  return(grpc_metrics)
}

find_mapped_data <- function (visual_data) {
  mapped_dict <- list()

  # get keys of visual_data
  # dict_keys(['fpr', 'tpr', 'roc_auc', 'recall', 'precision', 'average_precision', 'confusion_matrix'])
  keys_visual <- names(visual_data)

  for (key in keys_visual) {
    inner_keys <- names(visual_data[[key]])

    for (ik in inner_keys) {
      if (typeof(visual_data[[key]][[ik]]) == "list") {
        # Mapping type
        mapped_dict = c(mapped_dict, key)
      }
    }
  }

  return(unique(mapped_dict))
}

parse_array_data <- function (visual_data, mapped_keys) {
  # parse the below data
  # "fpr": {
  # 	"0": [
  # 		0.027777777777777776,
  # 		0.05555555555555555,
  # 		0.08333333333333333
  # 	],
  # 	"1": [
  # 		0.03125,
  # 		0.0625,
  # 		0.0625
  # 	],
  # 	"2": [
  # 		0.03125,
  # 		0.03125,
  # 		0.03125
  # 	],
  # 	"macro": [
  # 		1.0,
  # 		0.9722222222222222,
  # 		0.96875
  # 	],
  # 	"micro": [
  # 		0.8,
  # 		0.81,
  # 		0.82
  # 	]
  # }

  result_dict <- list("metrics" = list("values" = list(), "timestamp" = list(), "steps" = list() ) )

  # remove mapped data
  for (mk in mapped_keys) {
    visual_data[[mk]] <- NULL
  }

  # get keys of visual_data
  # dict_keys(['fpr', 'tpr', 'roc_auc', 'recall', 'precision', 'average_precision', 'confusion_matrix'])
  keys_visual <- names(visual_data)

  idx = 1;
  grpc_metrics <- list()
  for (key in keys_visual) {
    result_dict <- extract_metric_data_from_array(idx, key, result_dict, visual_data)
    grpc_metrics <- set_extracted_visual(grpc_metrics, key, result_dict)
  }

  return(grpc_metrics)
}

parse_mapped_data <- function (visual_data, mapped_keys) {
  # parse the below data
  # "legend": {
  # 	"roc": {
  # 		"0": 0.986111111111111,
  # 		"1": 0.9340277777777771,
  # 		"2": 0.9644097222222223,
  # 		"macro": 0.9681483318636093,
  # 		"micro": 0.9771999999999995
  # 	},
  # 	"precision-recall": {
  # 		"0": 1.0,
  # 		"1": 0.9445039714169319,
  # 		"2": 0.9463180496881478,
  # 		"macro": 0.9482649997968625,
  # 		"micro": 0.9570831563917747
  # 	}
  # }

  # return data
  #   {
  #     "key": "legend",
  #     "values": [[0.1, 0.2, 0.3, 0.4, 0.5], [0.1, 0.2, 0.3, 0.4, 0.5]],
  #     "timestamp": [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  #     "steps": ["roc_0", "roc_1", "roc_2", "roc_macro", "roc_micro",
  #               "precision-recall_0", "precision-recall_1", "precision-recall_2", "precision-recall_macro", "precision-recall_micro",
  #     ]
  #   },

  # get keys of visual_data
  # dict_keys(['fpr', 'tpr', 'roc_auc', 'recall', 'precision', 'average_precision', 'confusion_matrix'])
  keys_visual <- names(visual_data)

  # get only mapped_keys data from visual_data
  matched_index = match(mapped_keys, keys_visual)
  visual_data = visual_data[matched_index]
  keys_visual = keys_visual[matched_index]

  grpc_metrics <- list()
  for (key in keys_visual) {
    result_dict <- extract_metric_data_from_mapped(visual_data = visual_data, key = key)
    grpc_metrics <- set_extracted_visual(grpc_metrics, key, result_dict)
  }

  return(grpc_metrics)
}

parse_visual_data <- function (visual_data) {
  mapped_keys <- find_mapped_data(visual_data = visual_data)
  grpc_metrics <- parse_array_data(visual_data = visual_data, mapped_keys = mapped_keys)
  mapped_metrics <- parse_mapped_data(visual_data = visual_data, mapped_keys = mapped_keys)

  return(c(grpc_metrics,  mapped_metrics))
}

parse_run_visual <- function (jsonData, run_info_json) {
  # parse 'for-visual-json/xxx.json'

  # get all "keys" field
  # return value
  # [
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

  # get visual file path from run_info.json [result_path.visual_json_full]
  # ex. ~\runs\results-XGBClassifier\for-visual-json\XGBClassifier-visual-1.json
  visual_result <- list()
  data_path <- get_visual_json_full(run_info_json)

  if (!is.null(data_path)) {
    # read visual data
    visual_data <- accu_parse_json(input_path = data_path, json_format="records")

    # get keys of visual_data
    visual_result <- parse_visual_data(visual_data)
  }
  return(visual_result)
}
