#' Query Writer
#'
#' This function allows you to write complex SQL queries in a normal coding format while also easily incorporating data from vectors and data frames into your query.
#' @param ... A character string, or a series of character strings separated by commas.  You can also use functions that return character strings, such as the other sqlWriter functions.
#' @keywords sqlWriter
#' @export
#' @examples
#' sqlQueryWrite()

sqlQueryWrite <- function(...) {
  #paste together all elements
  query <- paste(..., sep=" ")

  #replace any -- comments with /* */
  query <- sapply(query, function(x) strsplit(x, "\\\n")[[1]], USE.NAMES=FALSE)
  query <- sapply(query, function(x){if(length(grep("--", x) > 0)) {x <- paste(x,"*/", sep = "")} else{x}}, USE.NAMES = FALSE)
  query <- sapply(query, sub, pattern = "--", replacement = "/\\*")
  query <- paste(query, collapse = " ")

  #remove any remaining \t and \n characters
  query <- gsub("\\t"," ", query)
  query <- gsub("\\n"," ", query)
  query <- gsub("\\r"," ", query)
}

query <- "SELECT * FROM
--WHAT!
table
WHERE -- yeah
red = blue"
