#' @title extract function for read ini file function
#' @description used by read ini file function and not intended for use as a stand-alone fuinction
#' @param regexp blah
#' @param x blah
#' @return blah
#' @export
#' @examples
#' section = extract(header, l)
extract = function(regexp, x) regmatches(x, regexec(regexp, x))[[1]][2]

#' @title read ini file
#' @description reads ini file as list of lists
#' @param ini_file_path ini file path
#' @return list of lists
#' @export
#' @examples
#' ini_file <- read_ini_file(ini_file_path)
read_ini_file = function(ini_file_path) {

  blank = "^\\s*$"
  header = "^\\[(.*)\\]$"
  key_value = "^.*=.*$"

  lines = readLines(ini_file_path)

  ini = list()
  for (l in lines) {
    if (grepl(blank, l)) next
    if (grepl(header, l)) {
      section = extract(header, l)
      ini[[section]] = list()
    }
    if (grepl(key_value, l)) {
      kv = strsplit(l, "\\s*=\\s*")[[1]]
      ini[[section]][[kv[1]]] = kv[2]
    }
  }
  ini
}

#' @title read text file as list
#' @description reads text file as list with a given separator
#' @param text_file_path text file path
#' @param list_separator separator e.g. ";" for sql statements / code blocks
#' @return my_list list
#' @export
#' @examples
#' my_list <- read_text_file_as_list(text_file_path, list_separator)
read_text_file_as_list <- function(text_file_path, list_separator) {
  my_string <- suppressWarnings(paste(readLines(text_file_path), collapse="\n"))
  my_list <- as.list(strsplit(my_string, list_separator)[[1]])
  my_list
}

#' @title read first line of text file
#' @description reads first line of text file
#' @param text_file_path text file path
#' @return first_line first line
#' @export
#' @examples
#' first_line <- read_first_line_of_text_file(text_file_path)
read_first_line_of_text_file <- function(text_file_path){
  con <- file(text_file_path,"r")
  first_line <- readLines(con,n=1)
  close(con)
  return(first_line)
}

# print to log
print_to_log <- function(log_file_path, log_message) {

  # build message string
  my_date <- as.POSIXct(Sys.time(), format = "m/d/y H:M:S",tz=Sys.timezone())
  my_message <- paste(my_date, my_message, sep=": ")

  # print to screen
  message(log_message)

  # print to log file
  if(!is.null(log_file_path)) {

    # insert log file code here

  }

}

# get param from ini file (pipe separated)
fn_get_param <- function(my_file, my_section, my_param){
  # read file
  df_params <- read.table(my_file, sep="|", header = F, colClasses = c('character', 'character', 'character'))
  # filter for section and param then get value
  my_value <- df_params[df_params$V1 == my_section & df_params$V2 == my_param,3]
  return(my_value)
}
