library(stringr)
rep_strings <- function(this_str, n) {
  both <- lapply(this_str, function(x) rep(x, n))
  unlist(both)
  }

#' Writes a list of all parameters to a file
#'
#' @param full.list
#' @param file.name
#' @return none 

WriteListToParamFile <- function(full.list, file.name){
  for(key in names(full.list)){
    writeEntry(key, full.list[key], file.name)
  }
}

#' Reads in a parameter file constructed from a genParam file
#'
#' @param file.name the file where the parameters
#' @return a list with keys and values corresponding to the parameter file

ReadParamFile <- function(file.name){
  # returns the contents of a parameter file as a list 
  lines <- readLines(file.name, file.info(file.name)$size)
  do.call("c", lapply(lines, convertToListEntry))
}

#' Take a line from a genParam parameter file and turns it into a list element
#'
#' @param line a line from a parameter file
#' @return list selement 

convertToListEntry <- function(line){
  first.pattern <- "\\{.*\\}\\{"
  m <- regexpr(first.pattern, line)
  key <- gsub("[}{]", "", regmatches(line, m))
  
  second.pattern <- "\\}\\{.*\\}"
  m <- regexpr(second.pattern, line)
  value <- gsub("[}{]", "", regmatches(line, m))
  l = list()
  l[key] = value
  l 
}

# A function for generating a function that adds parameters to a tex file (parameter.file)
# which is imported in the base LaTeX document. It is useful for passing numbers, dates and so on
# programmatically to a write-up rather than writing them in by hand. 
#
# @param parameter.file file you want to hold the customer latex new commands
# @return A function that can be used to add parameters to the file created. 
# @export

genParamAdder <- function (parameter.file) {
  # if (file.exists(parameter.file)) {
  #  file.remove(parameter.file)
  # }
  f <- function(name, value) {
    ## Check if file exists already 
    if (file.exists(parameter.file)) {
      l <- ReadParamFile(parameter.file)
      # if this value was already in the params file
      if(name %in% names(l)){
        print(paste0("Updated: ", name, " from ", l[name], " to ", value))
      }
      l[name] = value
      file.remove(parameter.file)
      WriteListToParamFile(l, parameter.file)
    } else { 
      line <- paste0("\\newcommand{", name, "}{", value, "}")
      write(line, parameter.file, ncolumns = 1, append = TRUE)
    }
  }
  f
}

#' Takes an R list and creates commands for each of the keys.
#'
#' @param key.list list of keys and values. 
#' @param parameter.file.location is the location of the parameter file.
#' @param append is a boolean for whether we shoudl append the parameter file. 
#' @return None - run for side-effects.  
#' @export

createLaTeXparameters <- function(key.list, parameter.file.location, append = FALSE){
  createCommand <- function(command.name, value) paste("\\newcommand{\\", command.name, "}{", value, "}", sep="")
  command.list <- sapply(names(key.list), function(key) createCommand(key, key.list[[key]]))
  if(append){
    fileConn <- file(parameter.file.location, open = "a")
    writeLines(command.list, fileConn)
    close(fileConn)
  } else {
    writeLines(command.list, parameter.file.location)
  }
}

#' Writes a key value pair to a LaTeX parameter file
#'
#' Writes a key value pair to a LaTeX parameter file
#'
#' @param key the name of the LaTeX variable 
#' @param value the value of the variable
#' @param parameter.file the paramater file

writeEntry <- function(key, value, parameter.file){
  # Writes a single entry back to the parameter file. 
  line <- paste0("\\newcommand{", key, "}{", value, "}")
  write(line, parameter.file, ncolumns = 1, append = TRUE)
}

formatParamString <- function(x){
  str_trim(sprintf("%6.2f",x))
  str_trim(formatC(x, big.mark = ',', digits = 2, flag='#'))
}
