##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                             ##
## Formats a given numeric vector with the specified number of ## 
## decimals.                                                   ##
##                                                             ##
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##


#' Formats a given numeric vector with the specified number of decimals
#'
#' @param x A numeric vector of values.
#' @param .output_digits Number of output digits.
#'
#' @return A formatted character vector of the values.
format_number <- function(x, .output_digits){
  
  out <- ifelse(is.na(x) | is.nan(x),
                "-",
                format(round(x, digits = .output_digits), nsmall = .output_digits, big.mark = ",", trim = TRUE))
  
  return(out)
  
}
