#' Query Writer
#' 
#' This function allows you to write complex SQL queries in a normal coding format while also easily incorporating data from vectors and data frames into your query.
#' @param ... A character string, or a series of character strings separated by commas.  You can also use functions that return character strings, such as the other sqlWriter functions.
#' @keywords sqlWriter
#' @export
#' @examples 
#' sqlQueryWrite()

sqlQueryWrite <- function(...) {
  query <- paste(..., sep="")
  query <- str_replace_all(query, "\t"," ")
  str_replace_all(query, "\n"," ")
}