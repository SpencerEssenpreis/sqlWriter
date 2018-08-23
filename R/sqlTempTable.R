#' Insert Values into a Temporary SQL Table
#'
#' This function converts a Data Frame or Matrix into a character string containing an SQL statement to insert the values into a temporary table.  While this will create all the code for inserting the values into the temporary table, you will still need to write code to create the temporary table in your query.
#' @param sourceframe A data frame you want to insert into a temporary table
#' table The name of the temporary table you want to insert values into.
#' @keywords sqlWriter
#' @export
#' @examples
#' sqlTempTable(mtcars, table = "#mtcars")

sqlTempTable <- function(sourceframe, table)
{
  sourceframe %>% dplyr::mutate_if(is.factor, as.character) -> sourceframe
  for (i in 1: ncol(sourceframe))
  {
    coln <- colnames(sourceframe)[i]
    colnames(sourceframe)[i] <- "tempname"
    if (is.character(sourceframe$tempname))
    {
      sourceframe %>% dplyr::filter(!is.na(tempname)) %>% dplyr::mutate(tempname = paste("'", tempname, "'", sep="")) -> sourceframea
      sourceframe %>% dplyr::filter(is.na(tempname)) -> sourceframeb
      sourceframe <- dplyr::bind_rows (sourceframea, sourceframeb)
      rm(sourceframea,sourceframeb)
    }
    colnames(sourceframe)[i] <- coln
    i = i+1
  }
  rm(coln)

  sourceframe %>% dplyr::mutate_all(as.character) -> sourceframe
  sourceframe[is.na(sourceframe)] <- "NULL"
  sourceframe %>% tidyr::unite(values, sep = ",") %>% dplyr::mutate(values = paste("(", values, ")", sep=""))-> sourceframe
  paste("INSERT INTO ", table, " VALUES ", paste(sourceframe$values, collapse=","), "; ", sep="")
}
