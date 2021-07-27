new_accu_run_info <- function(
  name = NA,
  userId = NA,
  creaDt = NA,
  endDt = NA,
  duration = NA,
  afLoc = NA,
  status = NA,
  note = NA,
  path = NA,
  modelPath = NA,
  jsonPath = NA) {
  structure(
    list(
      name = name,
      userId = userId,
      creaDt = creaDt,
      endDt = endDt,
      duration = duration,
      afLoc = afLoc,
      status = status,
      note = note,
      path = path,
      modelPath = modelPath,
      jsonPath = jsonPath
    ),
    class = "accu_run_info"
  )
}

new_accu_ws_run_info <- function(
  workspaceRunId = NA,
  isSuccess = NA,
  stopFlag = NA,
  stopTimeout = NA) {
  structure(
    list(
      workspaceRunId = workspaceRunId,
      isSuccess = isSuccess,
      stopFlag = stopFlag,
      stopTimeout = stopTimeout
    ),
    class = "accu_ws_run_info"
  )
}