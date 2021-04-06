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
  # print(constVal$BACK_END_API_URL)
  .globals$tracking_uri <- constVal$BACK_END_API_URL
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

accu_set_current_run <- function () {
  .globals$run_info_json = get_current_run()
}

accu_get_current_run <- function () {
  .globals$run_info_json
}

