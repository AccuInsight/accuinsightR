#' Set Remote Tracking URI
#'
#' Specifies the URI to the remote server that will be used
#' to create experiments.
#'
#' @param uri The URI to the remote server.
#'
#' @export
accu_set_tracking_uri <- function() {
  constVal <- accu.consts

  env_value = get_os_env('ENV')
  tracking_uri <- paste0('http://', env_value[[constVal$ENV_BACK_END_API_URL]], ':', env_value[[constVal$ENV_BACK_END_API_PORT]], '/')
  print(tracking_uri)

  .globals$tracking_uri <- tracking_uri
  # .globals$tracking_uri <- constVal$BACK_END_API_URL
  #invisible(uri)
}

#' Get Remote Tracking URI
#'
#' Gets the remote tracking URI.
#'
#' @export
accu_get_tracking_uri <- function() {
  .globals$tracking_uri
}

accu_set_current_run <- function (run_meta) {
  .globals$run_info_json = run_meta
}

accu_get_current_run <- function () {
  .globals$run_info_json
}

accu_get_current_run_id <- function () {
  .globals$run_info_json$run_name
}
