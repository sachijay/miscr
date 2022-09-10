##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                           ##
## Get the n(%) text, given n and the proportion separately. ##
##                                                           ##
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##


#' Get the n(%) text, given n and the proportion separately.
#'
#' @param n A vector or a single value of counts.
#' @param prop A vector or a single value of proportions
#' @param .output_digits Number of decimal points in the output percentages. If a numeric vector with more than 2 elements provided, 
#' the first number will be taken as the number of digits for n (default is 0) and the second number as the digits for the percentage (default is 1). 
#' @param .with_percentage A logical indicating if the percentage mark (%) should be shown. The default is `TRUE`.
#'
#' @return A vector or a single string as n(%).
get_n_perc_txt <- function(n, 
                           prop,
                           .output_digits = c(0, 1),
                           .with_percentage = TRUE){
  
  
  if(length(n) != length(prop)){
    
    stop("Counts and proportions should have the same number of elements!")
    
  }
  
  
  n_digits <- 0
  perc_digits <- NULL
  
  
  if(length(.output_digits) == 1){
    
    perc_digits <- .output_digits
    
  } else if(length(.output_digits) == 2){
    
    n_digits <- .output_digits[1]
    perc_digits <- .output_digits[2]
    
  } else{
    
    warning("Only the first 2 arguments of output_digits will be used.")
    
    n_digits <- .output_digits[1]
    perc_digits <- .output_digits[2]
    
  }
  
  
  if(!is.numeric(n)){
    
    message("Will convert counts to numeric.")
    n <- as.numeric(n)
    
  }
  
  
  if(!is.numeric(prop)){
    
    message("Will convert proportions to numeric.")
    prop <- as.numeric(prop)
    
  }
  
  
  perc_symbol <- "%"
  
  if(!.with_percentage){
    
    perc_symbol <- ""
    
  }
  
  n_formatted <- format_number(n, .output_digits = n_digits)
  perc_formatted <- format_number(prop*100, .output_digits = perc_digits)
  
  
  out <- paste0(
    n_formatted,
    " (", 
    perc_formatted, 
    perc_symbol, 
    ")")
  
  
  return(out)
  
}