#' Create a List for SQL IN Clauses
#' 
#' This function allows you to take a vector and turn it into a character string containing a comma separated list, bookended by parentheses, for use in an SQL IN clause.
#' @param vect A vector you want to use in an SQL IN Clause
#' type Either "num" for numeric or "char" for character.  "char" will put single quotes around each value in the list so the query will be process the values as strings.  Defaults to "char".
#' @keywords sqlWriter
#' @export
#' @examples 
#' sqlIn(mtcars$mpg, type = "num")
#' sqlIn(iris$Species)


sqlIn <- function(vect, type="char") {
  if (type=="char") {paste(" ('", paste(unique(vect[!is.na(vect)]), collapse="','"), "') ", sep="") } else
    if (type=="num") {paste(" (", paste(unique(vect[!is.na(vect)]), collapse=","), ") ", sep="")}
}
