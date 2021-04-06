get_object_keys <- function (obj) {
  keys <- names(obj)
}

resolve_client <- function(client) {
  if (is.null(client)) {
    restUrl <- accu.consts$BACK_END_API_URL
    accu_client(tracking_uri = restUrl)
  } else {
    if (!inherits(client, "accu_client")) stop("`client` must be an `accu_client` object.", call. = FALSE)
    client
  }
}
