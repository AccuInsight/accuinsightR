#library(rlang)

new_accu_client <- function(tracking_uri) {
  UseMethod("new_accu_client")
}

new_accu_client.default <- function(tracking_uri) {
  stop(paste("Unsupported scheme: '", tracking_uri$scheme, "'", sep = ""))
}

get_env_var <- function(x) {
  new_name <- paste("ACCU_TRACKING_", x, sep = "")
  res <- Sys.getenv(new_name, NA)
  if (is.na(res)) {
    old_name <- paste("ACCU_", x, sep = "")
    res <- Sys.getenv(old_name, NA)
    if (!is.na(res)) {
      warning(paste("'", old_name, "' is deprecated. Please use '", new_name, "' instead."),
                    sepc = "" )
    }
  }
  res
}

new_accu_client_impl <- function(get_host_creds, get_cli_env = list, class = character()) {
  structure(
    list(get_host_creds = get_host_creds,
         get_cli_env = get_cli_env
    ),
    class = c(class, "accu_client")
  )
}

new_accu_host_creds <- function( host = NA, username = NA, password = NA, token = NA,
                                   insecure = "False") {
  insecure_arg <- if (is.null(insecure) || is.na(insecure)) {
    "False"
  } else {
    list(true = "True", false = "False")[[tolower(insecure)]]
  }
  structure(
    list(host = host, username = username, password = password, token = token,
         insecure = insecure_arg),
    class = "accu_host_creds"
  )
}

basic_http_client <- function(tracking_uri) {
  host <- paste(tracking_uri$scheme, tracking_uri$path, sep = "://")
  get_host_creds <- function () {
    new_accu_host_creds(
      host = host,
      username = get_env_var("USERNAME"),
      password = get_env_var("PASSWORD"),
      token = get_env_var("TOKEN"),
      insecure = get_env_var("INSECURE")
    )
  }
  cli_env <- function() {
    creds <- get_host_creds()
    res <- list(
      MLFLOW_TRACKING_USERNAME = creds$username,
      MLFLOW_TRACKING_PASSWORD = creds$password,
      MLFLOW_TRACKING_TOKEN = creds$token,
      MLFLOW_TRACKING_INSECURE = creds$insecure
    )
    res[!is.na(res)]
  }
  new_accu_client_impl(get_host_creds, cli_env, class = "accu_http_client")
}

new_accu_client.accu_http <- function(tracking_uri) {
  basic_http_client(tracking_uri)
}

new_accu_client.accu_https <- function(tracking_uri) {
  basic_http_client(tracking_uri)
}

# s3 object, r generic function
new_accu_uri <- function(raw_uri) {
  parts <- strsplit(raw_uri, "://")[[1]]
  structure(
    list(scheme = parts[1], path = parts[2]),
    class = c(paste("accu_", parts[1], sep = ""), "accu_uri")
  )
}

#' Initialize an Accu modeler Client
#'
#' Initializes and returns an Accu modeler client.
#'
#' @param tracking_uri The tracking URI. If not provided, defaults to the service
#'  set by `accu_set_tracking_uri()`.
#' @export
accu_client <- function(tracking_uri = NULL) {
  #tracking_uri <- new_accu_uri(tracking_uri %||% accu_get_tracking_uri())
  tracking_uri <- new_accu_uri(accu_get_tracking_uri())
  client <- new_accu_client(tracking_uri)
  # if (inherits(client, "accu_file_client")) mlflow_validate_server(client)
  return(client)
}
