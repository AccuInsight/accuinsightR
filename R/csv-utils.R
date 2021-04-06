accu_read_csv <- function (input_path, sep = ";") {
  # read csv file and convert to json object
  csv_data <- utils::read.csv(input_path, sep = sep)
  csv_to_json <- jsonlite::toJSON(csv_data)
  json_obj <- jsonlite::fromJSON(csv_to_json)
  return(json_obj)
}