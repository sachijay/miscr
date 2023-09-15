##+++++++++++++++++++++++++++++++++++++++##
##                                       ##
## Copies the dataframe to the clipboard ##
##                                       ##
##+++++++++++++++++++++++++++++++++++++++##

#' Copy a dataframe in to the clipboard.
#'
#' @param .dat `data.frame` to copy. 
#' @param .sep Separator for the cells. To copy to excel use the default (`"\t"`). 
#' @param .na String to replace `NA` in the `data.frame`. The default is an empty string (`""`).
#'
#' @export
copy_to_clipboard <- function(
    .dat,
    .sep = "\t",
    .na = ""
){
  
  utils::write.table(
    x = .dat,
    file = "clipboard",
    sep = .sep,
    row.names = FALSE,
    na = .na
  )
  
}
