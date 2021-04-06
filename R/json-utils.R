accu_parse_json <- function(input_path, json_format="split") {
  if (file.exists(input_path)) {
    switch(json_format,
      split = {
        json <- jsonlite::fromJSON(input_path, simplifyVector = TRUE)
        elms <- names(json)
        if (length(setdiff(elms, c("columns", "index", "data"))) != 0
        || length(setdiff(c("columns", "data"), elms) != 0)) {
          stop(paste("Invalid input. Make sure the input json data is in 'split' format.", elms))
        }
        df <- data.frame(json$data, row.names = json$index)
        names(df) <- json$columns
        df
      },
      records = jsonlite::fromJSON(input_path, simplifyVector = TRUE),
      stop(paste("Unsupported JSON format", json_format,
                 ". Supported formats are 'split' or 'records'"))
    )
  } else {
    print("accu_parse_json: input path file not exist")
  }
}

json_has_key <- function (key, jsonData) {
  return(exists(key, jsonData))
}
