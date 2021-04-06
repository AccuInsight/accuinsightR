get_lib_test <- function () {
  feature_list <- list("feature" = list("importance" = list()))
  feature_list$feature$importance <- ""
  print(jsonlite::toJSON(feature_list, auto_unbox = TRUE))


  feature_list <- list("feature" = list("importance" = list("data" = list())))
  feature_list$feature$importance$data <- NULL
  print(jsonlite::toJSON(feature_list, auto_unbox = TRUE))
}

