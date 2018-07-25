#' Create a Series of SQL LIKE Statements
#' 
#' This function allows you to take a vector and turn it into a character string containing a series of LIKE statements for use in an SQL query.
#' @param vect A vector you want to use for creating your LIKE statements
#' field The name of the field in your SQL database to use in your LIKE statements
#' wildcard Which wildcard character you would like to use in your LIKE statements. Defaults to "%"
#' loc Where you would like to locate the wildcard character for each value.  Current options are "end" for the appending the wildcard to the end of the value, "lead" for appending the wildcard to the beginning of the value, and "both" for both.  Defaults to "end".
#' @keywords sqlWriter
#' @export
#' @examples 
#' sqlLike(iris$Species, field = "iris_species")
#' sqlLike(iris$Species, field = "iris_species", wildcard = "_", loc = "both")


sqlLike <- function(vect, field, wildcard="%", loc="end") {
  
  vect <- vect[!is.na(vect)]
  
  if(loc=="end") 
  {vect <- paste("'", unique(vect), wildcard, "'", sep="")} else
    if(loc=="lead")
    {vect <- paste("'", wildcard, unique(vect), "'", sep="")} else
      if(loc=="both")
      {vect <- paste("'", wildcard, unique(vect), wildcard, "'", sep="")}
  
  paste(field, "LIKE", vect, sep=" ", collapse = ' OR ')
}