accu_is_verbose <- function() {
  nchar(Sys.getenv("ACCU_VERBOSE")) > 0 || getOption("accu.verbose", FALSE)
}

accu_verbose_message <- function(...) {
  if (accu_is_verbose()) {
    message(...)
  }
}
