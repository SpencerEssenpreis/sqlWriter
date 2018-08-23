#' Update sqlWriteR
#'
#' This function simplifies updating the sqlWriteR library by re-installing it from github.
#' Simply run sqlUpdate() and R will update and reload sqlWriteR for you!
#'
#' @keywords sqlWriteR
#' @export
#' @examples
#' sqlUpdate()

sqlUpdate <- function()
{
  devtools::install_github("spenceredwin/sqlWriteR")
  library(sqlWriteR)
}
