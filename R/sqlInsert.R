#' Insert Values into an SQL Table
#'
#' This function converts a Data Frame or Matrix into a character string containing an SQL statement to insert the values into a table.
#' @param sourceframe A data frame you want to insert into a table
#' table The name of the table you want to insert values into.
#' @keywords sqlWriter
#' @export
#' @examples
#' sqlInsert(mtcars, table = "car_data")

sqlInsert <- function(sourceframe, table)
{
  sourceframe %>% mutate_if(is.factor, as.character) -> sourceframe
  for (i in 1: ncol(sourceframe))
  {
    coln <- colnames(sourceframe)[i]
    colnames(sourceframe)[i] <- "tempname"
    if (is.character(sourceframe$tempname))
    {
      sourceframe %>% filter(!is.na(tempname)) %>% mutate(tempname = paste("'", tempname, "'", sep="")) -> sourceframea
      sourceframe %>% filter(is.na(tempname)) -> sourceframeb
      sourceframe <- bind_rows (sourceframea, sourceframeb)
      rm(sourceframea,sourceframeb)
    }
    colnames(sourceframe)[i] <- coln
    i = i+1
  }
  rm(coln)

  sourceframe %>% mutate_all(as.character) -> sourceframe
  sourceframe[is.na(sourceframe)] <- "NULL"
  sourceframe %>% unite(values, sep = ",") %>% mutate(values = paste("(", values, ")", sep=""))-> sourceframe
  paste("INSERT INTO ", table, " VALUES ", paste(sourceframe$values, collapse=","), "; ", sep="")
}
