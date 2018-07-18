#' Add Leading 0s to A Number
#' 
#' This function will add leading 0s to a number to the values in a vector of numbers.  The result will be a character string or vector of character strings.
#' @param vect Either a numeric value or a vector of numeric values to which you want to add leading 0s
#' len The length you want each value to have after leading 0s are added.  For example, if every number in a vector should be 10 characters long, the function will add 1 leading 0 to 123456789, but 5 leading 0s to 12345.
#' @keywords sqlWriter
#' @export
#' @examples 
#' lead0(12345, len = 10)
#' lead0(ids, len = 20)
#' lead0(sales$product_id, len = 15)

lead0 <- function(vect, len)
{
  vect <- if(!is.character(vect)){as.character(vect)} else {vect}
  for (i in 1:length(vect))
  {
    if(!is.na(vect[i]) & nchar(vect[i]) > 0)
    {
      if(nchar(vect[i]) < len & nchar(vect[i]) > 0 & !is.na(vect[i])) 
      {
        repval <- len - nchar(vect[i])
        repval <- as.character(rep(0,repval))
        repval <- paste(repval, collapse="")
        vect[i] <- paste(repval, vect[i], sep="")
      }
    }
    i = i+1
  }
  return(vect)
}