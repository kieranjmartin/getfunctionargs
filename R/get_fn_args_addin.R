#' get_fn_args
#'
#' This function takes a string of arguments as if they were written for a function
#' it replaces commas with ; where appropriate and executes the code
#' Designed to easily grab several arguments from a selection.
#' Note that this works by looking for matching brackets and inverted commas
#' If for some reason you have hanging commas and speech marks nested within each other this could cause issues!
#' @param string_select String to be read and given to the console
#' @return The console will run the provided string split by ; where commas are
#' @export
#'
get_fn_args <- function(string_select = NULL){
  if (is.null(string_select)){
    string_select <- rstudioapi::getSourceEditorContext()$selection[[1]]$text
  }
  # find all commas
  commas <- str_iterate(string_select)
  if (length(commas) > 0){
    for (rep_loc in seq(1, length(commas))){
      substr(string_select, commas[rep_loc], commas[rep_loc]) <- ';'
    }
  }
  rstudioapi::sendToConsole(string_select)
}

#' str_iterate
#'
#' recursive function for going through a string and finding commas which indicate new arguments
#' will not locate commas if they are contained by (),{},'' or ""
#' will ignore escaped characters for this purpose
#' @param stringin string to explore. Note that this is the only argument that should normally be provided
#' @param location starting location to explore. Should normally be set to 1
#' @param bracket_count number of open brackets
#' @param curlbrackets  number of open curly brackets
#' @param singlespeech number of open single speech marks
#' @param doublespeech number of open double speech marks
#' @param comma_loc vector containing the location of all commas
#' @return comma_loc, a numeric vector with the location of commas in the inputted vector
str_iterate <- function(stringin,
                        location = 1,
                        bracket_count = 0,
                        curlbrackets = 0,
                        singlespeech = 0,
                        doublespeech = 0,
                        comma_loc = c()){

  # check to see if anything is open currently
  allchecks <- bracket_count + curlbrackets + singlespeech + doublespeech
  #grab the current character
  cur_char <- substr(stringin, location, location)

  # if we have a comma and nothing open we update comma_loc
  if (cur_char == "," & allchecks == 0){
      comma_loc <- c(comma_loc, location)
  }

  # if we are at the end of the vector, return comma locations

  if (location == nchar(stringin)){
    return(comma_loc)
  }
  # handle escape characters
  if (cur_char == "\\"){
    location <- location + 2
    return(str_iterate(stringin, location, bracket_count, curlbrackets, singlespeech, doublespeech, comma_loc))
  }

  #check to see if special characters open/close
  if (allchecks == 0 | bracket_count > 0 ){
    bracket_count <- bracket_handler(cur_char, bracket_count, "(", ")")
  }
  allchecks <- bracket_count + curlbrackets + singlespeech + doublespeech
  if (allchecks == 0 | curlbrackets  > 0 ){
    curlbrackets  <- bracket_handler(cur_char, curlbrackets, "{", "}")
  }
  allchecks <- bracket_count + curlbrackets + singlespeech + doublespeech
  if (allchecks == 0 | singlespeech  > 0 ){
    singlespeech <-  speech_handler(cur_char, singlespeech, "'")
  }
  allchecks <- bracket_count + curlbrackets + singlespeech + doublespeech
  if (allchecks == 0 | doublespeech > 0 ){
    doublespeech <-  speech_handler(cur_char, doublespeech, '"')
  }

  #iterate location and run again

  location <- location + 1

  str_iterate(stringin, location, bracket_count, curlbrackets, singlespeech, doublespeech, comma_loc)

}

#' bracket_handler
#'
#' Convenience function, increases inval for start, reduces it for stop
#'
#' @param curval the inputted character
#' @param inval the current value for the special character count
#' @param start what increases the value
#' @param stop what decreases the value
#' @return inval a numeric vector of length 1
bracket_handler <- function(curval, inval, start, stop){
  if (curval == start){
     return(inval + 1)
  }
  if (curval == stop){
    return(inval - 1)
  }
  inval
}

#' speech_handler
#'
#' Convenience function to handle speech marks
#' @param curval the current character value
#' @param inval current value
#' @param speech the type of speech mark being looked for
#' @return inval a numeric vector of length 1
speech_handler <- function(curval, inval, speech){
  if (curval == speech){
    return(abs(inval - 1))
  }
  inval
}
