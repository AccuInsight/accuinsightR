#' Print object for accu
#'
#'
#' @param object that will be printed by its type.
#' @export
accu_print <- function (obj) {
  UseMethod("accu_print")
}

#' @export
accu_print.default <- function(obj) {
  cat("This is for printing accu data")
}

#' @export
accu_print.accu_run_info <- function (obj) {
  accu_run_info <- obj

  # name
  # userId
  # creaDt
  # endDt
  # duration
  # afLoc
  # status
  # note
  # path
  # modelPath
  # jsonPath
  args <- list(
    name = if (is.na(accu_run_info$name)) {
      ""
    } else {
      paste ("name = ", accu_run_info$name, sep = "")
    },
    userId = if (is.na(accu_run_info$userId)) {
      ""
    } else {
      paste ("userId = ", accu_run_info$userId, sep = "")
    },
    creaDt = if (is.na(accu_run_info$creaDt)) {
      ""
    } else {
      paste ("creaDt = ", accu_run_info$creaDt, sep = "")
    },
    endDt = if (is.na(accu_run_info$endDt)) {
      ""
    } else {
      paste ("endDt = ", accu_run_info$endDt, sep = "")
    },
    duration = if (is.na(accu_run_info$duration)) {
      ""
    } else {
      paste ("duration = ", accu_run_info$duration, sep = "")
    },
    afLoc = if (is.na(accu_run_info$afLoc)) {
      ""
    } else {
      paste ("afLoc = ", accu_run_info$afLoc, sep = "")
    },
    status = if (is.na(accu_run_info$status)) {
      ""
    } else {
      paste ("status = ", accu_run_info$status, sep = "")
    },
    note = if (is.na(accu_run_info$note)) {
      ""
    } else {
      paste ("note = ", accu_run_info$note, sep = "")
    },
    path = if (is.na(accu_run_info$path)) {
      ""
    } else {
      paste ("path = ", accu_run_info$path, sep = "")
    },
    modelPath = if (is.na(accu_run_info$modelPath)) {
      ""
    } else {
      paste ("modelPath = ", accu_run_info$modelPath, sep = "")
    },
    jsonPath = if (is.na(accu_run_info$jsonPath)) {
      ""
    } else {
      paste ("jsonPath = ", accu_run_info$jsonPath, sep = "")
    },
    sep = ", "
  )
  cat("accu_run_info( ")
  do.call(cat, args[args != ""])
  cat(")\n")
}


#' @export
print.accu_run_info <- function(x, ...) {
  accu_run_info <- x
  # name
  # userId
  # creaDt
  # endDt
  # duration
  # afLoc
  # status
  # note
  # path
  # modelPath
  # jsonPath

  args <- list(
    name = if (is.na(accu_run_info$name)) {
      ""
    } else {
      paste ("name = ", accu_run_info$name, sep = "")
    },
    userId = if (is.na(accu_run_info$userId)) {
      ""
    } else {
      paste ("userId = ", accu_run_info$userId, sep = "")
    },
    creaDt = if (is.na(accu_run_info$creaDt)) {
      ""
    } else {
      paste ("creaDt = ", accu_run_info$creaDt, sep = "")
    },
    endDt = if (is.na(accu_run_info$endDt)) {
      ""
    } else {
      paste ("endDt = ", accu_run_info$endDt, sep = "")
    },
    duration = if (is.na(accu_run_info$duration)) {
      ""
    } else {
      paste ("duration = ", accu_run_info$duration, sep = "")
    },
    afLoc = if (is.na(accu_run_info$afLoc)) {
      paste ("afLoc = ", "")
    } else {
      paste ("afLoc = ", accu_run_info$afLoc, sep = "")
    },
    status = if (is.na(accu_run_info$status)) {
      ""
    } else {
      paste ("status = ", accu_run_info$status, sep = "")
    },
    note = if (is.na(accu_run_info$note)) {
      ""
    } else {
      paste ("note = ", accu_run_info$note, sep = "")
    },
    path = if (is.na(accu_run_info$path)) {
      ""
    } else {
      paste ("path = ", accu_run_info$path, sep = "")
    },
    modelPath = if (is.na(accu_run_info$modelPath)) {
      ""
    } else {
      paste ("modelPath = ", accu_run_info$modelPath, sep = "")
    },
    jsonPath = if (is.na(accu_run_info$jsonPath)) {
      ""
    } else {
      paste ("jsonPath = ", accu_run_info$jsonPath, sep = "")
    },
    sep = ", "
  )
  cat("accu_run_info( ")
  do.call(cat, args[args != ""])
  cat(")\n")
}
