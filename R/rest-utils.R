get_object_keys <- function (obj) {
  keys <- names(obj)
}

resolve_client <- function(client) {
  if (is.null(client)) {
    env_value = get_os_env('ENV')
    restUrl <- paste0('http://', env_value[[accu.consts$ENV_BACK_END_API_URL]], ':', env_value[[accu.consts$ENV_BACK_END_API_PORT]], '/')
    # restUrl <- accu.consts$BACK_END_API_URL
    accu_client(tracking_uri = restUrl)
  } else {
    if (!inherits(client, "accu_client")) stop("`client` must be an `accu_client` object.", call. = FALSE)
    client
  }
}
