#' remove_open_brackets
#' 
#' Function to remove opening brackets
#' 
#' @param stringin string to remove brackets from
#' 
remove_open_brackets <- function(stringin){
  if (substr(stringin,1,1) %in% c("{", "(", " ")){
    stringin <- remove_open_brackets(substr(stringin,2,nchar(stringin)))
  }
  stringin
  
}