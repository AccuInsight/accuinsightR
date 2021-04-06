# setting backend API

#' getIP Function
#'
#' function description
#'
#' @export
#' @examples
getIP = function(const_path){
  constants_path = const_path
  original = readLines(constants_path)
  line_URL = code[grepl('BACK_END_API_URL', code)]
  line_PORT = code[grepl('BACK_END_API_PORT', code)]
  splits_URL = strsplit(line_URL, "'", fixed = TRUE)[[1]]
  splits_PORT = strsplit(line_PORT, "'", fixed = TRUE)[[1]]

  # message(paste('BACK_END_API_URL: ', splits_URL[2]))
  # message(paste('BACK_END_API_PORT: ', splits_PORT[2]))
}

#' setIP Function
#'
#' function description
#'
#' @export
#' @examples
setIP = function(IP=NULL, const_path){
  constants_path = const_path
  original_code = readLines(constants_path)
  line = original[grepl('BACK_END_API_URL', code)]
  splits = strsplit(line, "'", fixed = TRUE)[[1]]
  edited_line = gsub(splits[2], IP, line)
  edited_code = original_code
  edited_code[grepl('BACK_END_API_URL', code)] = edited_line

  writeLines(edited_code, constants_path)

  original = readLines(constants_path)
  line = original[grepl('BACK_END_API_URL', code)]
  splits = strsplit(line, "'", fixed = TRUE)[[1]]
  # message(paste('BACK_END_API_URL: ', splits[2]))
}

# getIP()
# setIP(IP='123.456.789.000')

