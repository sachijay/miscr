##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                                    ##
## Returns a string as num1 (num2) given the num1 and num2 separately ##
##                                                                    ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##



#' Returns a string as num1 (num2, num3) given the num1 and num3 separately
#'
#' @param num1 A vector or a single value of num1 (numerical) value(s).
#' @param num2 A vector or a single value of num2 (numerical) value(s).
#' @param num3 A vector or a single value of num3 (numerical) value(s).
#' @param .output_digits Number of decimal points in the output percentages. Accepts vectors with 2 elements. If a vector with more than 2 elements is provided, 
#' the first 2 items are selected with a warning. The default is set to 2.
#'
#' @return A string pasted as num1 (num2, num3). 
get_num1_num2_num3_txt <- function(num1,
                                   num2, num3,
                                   .output_digits = 2){
  
  num1_digits <- num2_digits <- num3_digits <- NULL
  
  if(length(.output_digits) == 1){
    
    num1_digits <- num2_digits <- num3_digits <- .output_digits
    
  } else if(length(.output_digits) == 2){
    
    num1_digits <- .output_digits[1]
    num2_digits <- num3_digits <- .output_digits[2]
    
  } else if(length(.output_digits) == 3){
    
    num1_digits <- .output_digits[1]
    num2_digits <- .output_digits[2]
    num3_digits <- .output_digits[3]
    
  } else{
    
    warning("Only the first 3 arguments of output_digits will be used")
    
    num1_digits <- .output_digits[1]
    num2_digits <- .output_digits[2]
    num3_digits <- .output_digits[3]
    
  }
  
  
  if(!is.numeric(num1)){
    
    message("Will convert num1 to numeric.")
    num1 <- as.numeric(num1)
    
  }
  
  
  if(!is.numeric(num2)){
    
    message("Will convert num2 to numeric.")
    num2 <- as.numeric(num2)
    
  }
  
  
  if(!is.numeric(num3)){
    
    message("Will convert num3 to numeric.")
    num3 <- as.numeric(num3)
    
  }
  
  
  if(length(num1) != length(num2) & length(num1) != length(num3)){
    
    stop("num1, num2 and num3 should have the same number of elements!")
    
  }
  
  num1_formatted <- format_number(num1, .output_digits = num1_digits)
  num2_formatted <- format_number(num2, .output_digits = num2_digits)
  num3_formatted <- format_number(num3, .output_digits = num3_digits)
  
  
  out <- paste0(num1_formatted,
                " (", 
                num2_formatted,
                ", ", 
                num3_formatted,
                ")")
  
  
  return(out)
  
}