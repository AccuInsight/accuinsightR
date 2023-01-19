accu_rest_path <- function(version, env_value) {
  const_val <- accu.consts

  projectID <- env_value[[const_val$ENV_PROJECT_ID]]
  workspaceID <- env_value[[const_val$ENV_WORKSPACE_ID]]
  experimentId <- env_value[[const_val$ENV_EXPERIMENT_ID]]
  #"1.0" = "/project/{project}/workspace/{workspaceId}/experiment/{experimentId}/run"
  endpoint <- sprintf("project/%s/workspace/%s/experiment/%s/run",projectID, workspaceID, experimentId)
  switch(
    version,
    "1.0" = endpoint
  )
}

accu_ws_run_rest_path <- function(version, env_value, mode) {
  const_val <- accu.consts
  
  projectID <- env_value[[const_val$ENV_PROJECT_ID]]
  workspaceID <- env_value[[const_val$ENV_WORKSPACE_ID]]
  endpoint <- sprintf("project/%s/workspace/%s/%s", projectID, workspaceID, mode)
  switch(
    version,
    "1.0" = endpoint
  )
}

#' @importFrom httr timeout
accu_rest_timeout <- function() {
  httr::timeout(180)
}

try_parse_response_as_text <- function(response) {
  raw_content <- httr::content(response, type = "raw")
  tryCatch({
    rawToChar(raw_content)
  }, error = function(e) {
    do.call(paste, as.list(raw_content))
  })
}

#' @importFrom base64enc base64encode
get_rest_config <- function(host_creds) {
  headers <- list()
  auth_header <- if (!is.na(host_creds$username) && !is.na(host_creds$password)) {
    basic_auth_str <- paste(host_creds$username, host_creds$password, sep = ":")
    paste("Basic", base64encode(charToRaw(basic_auth_str)), sep = " ")
  } else if (!is.na(host_creds$token)) {
    paste("Bearer", host_creds$token, sep = " ")
  } else {
    NA
  }
  if (!is.na(auth_header)) {
    headers$Authorization <- auth_header
  }
  headers$`User-Agent` <- paste("accu-r-client", utils::packageVersion("accuinsight"), sep = "/")
  is_insecure <- as.logical(host_creds$insecure)
  list(
    headers = headers,
    config = if (is_insecure) {
      httr::config(ssl_verifypeer = 0, ssl_verifyhost = 0)
    } else {
      list()
    }
  )
}

#' @importFrom httr GET POST add_headers config content
accu_rest <- function( ..., client, query = NULL, data = NULL, env_value = NULL, verb = "GET", version = "1.0",
                         max_rate_limit_interval=60) {
  host_creds <- client$get_host_creds()
  rest_config <- get_rest_config(host_creds)
  args <- list(...)
  api_url <- paste0(host_creds$host, accu_rest_path(version, env_value))
  #api_url <- file.path(host_creds$host, accu_rest_path(version, env_value), paste(args, collapse = "/"))
  req_headers <- do.call(httr::add_headers, rest_config$headers)

  get_response <- switch(
    verb,
    GET = function() {
      httr::GET( api_url, query = query, accu_rest_timeout(), config = rest_config$config,
           req_headers)
    },
    POST = function(){
      httr::POST( api_url,
            body = if (is.null(data)) NULL else rapply(data, as.character, how = "replace"),
            encode = "json",
            accu_rest_timeout(),
            config = rest_config$config,
            req_headers
      )
    },
    stop("Verb '", verb, "' is unsupported.", call. = FALSE)
  )
  sleep_for <- 1
  time_left <- max_rate_limit_interval
  response <- get_response()
  while (response$status_code == 429 && time_left > 0) {
    time_left <- time_left - sleep_for
    warning(paste("Request returned with status code 429 (Rate limit exceeded). Retrying after ",
                  sleep_for, " seconds. Will continue to retry 429s for up to ", time_left,
                  " second.", sep = ""))
    Sys.sleep(sleep_for)
    sleep_for <- min(time_left, sleep_for * 2)
    response <- get_response()
  }
  if (response$status_code != 200) {
    message_body <- tryCatch(
      paste(httr::content(response, "parsed", type = "application/json"), collapse = "; "),
      error = function(e) {
        try_parse_response_as_text(response)
      }
    )
    msg <- paste("API request to endpoint '",
                 paste(args, collapse = "/"),
                 "' failed with error code ",
                 response$status_code,
                 ". Reponse body: '",
                 message_body,
                 "'",
                 sep = "")
    stop(msg, call. = FALSE)
  }
  text <- httr::content(response, "text", encoding = "UTF-8")
  print("-- response text --")
  print(text)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' @importFrom httr GET POST add_headers config content
accu_ws_run_rest <- function( ..., mode, client, query = NULL, data = NULL, env_value = NULL, verb = "GET", version = "1.0",
                       max_rate_limit_interval=60) {

  print("------ API TEST ---------")

  host_creds <- client$get_host_creds()
  rest_config <- get_rest_config(host_creds)
  args <- list(...)

  sprintf('rest args %s', args);

  api_url <- paste0(host_creds$host, accu_ws_run_rest_path(version, env_value, mode))
  req_headers <- do.call(httr::add_headers, rest_config$headers)



  get_response <- switch(
    verb,
    GET = function() {
      httr::GET( api_url, query = query, accu_rest_timeout(), config = rest_config$config,
                 req_headers)
    },
    POST = function(){
      httr::POST( api_url,
                  body = if (is.null(data)) NULL else rapply(data, as.character, how = "replace"),
                  encode = "json",
                  accu_rest_timeout(),
                  config = rest_config$config,
                  req_headers
      )
    },
    stop("Verb '", verb, "' is unsupported.", call. = FALSE)
  )
  sleep_for <- 1
  time_left <- max_rate_limit_interval
  response <- get_response()
  while (response$status_code == 429 && time_left > 0) {
    time_left <- time_left - sleep_for
    warning(paste("Request returned with status code 429 (Rate limit exceeded). Retrying after ",
                  sleep_for, " seconds. Will continue to retry 429s for up to ", time_left,
                  " second.", sep = ""))
    Sys.sleep(sleep_for)
    sleep_for <- min(time_left, sleep_for * 2)
    response <- get_response()
  }
  if (response$status_code != 200) {
    message_body <- tryCatch(
      paste(httr::content(response, "parsed", type = "application/json"), collapse = "; "),
      error = function(e) {
        try_parse_response_as_text(response)
      }
    )
    msg <- paste("API request to endpoint '",
                 paste(args, collapse = "/"),
                 "' failed with error code ",
                 response$status_code,
                 ". Reponse body: '",
                 message_body,
                 "'",
                 sep = "")
    stop(msg, call. = FALSE)
  }
  text <- httr::content(response, "text", encoding = "UTF-8")
  print("-- response text --")
  print(text)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}