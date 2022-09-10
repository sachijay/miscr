##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                            ##
## Changes a vector of date strings from a given date format  ##
## to another set of date formats                             ##
##                                                            ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##


#' Changes date format orders
#'
#' @param x A character or numeric vector of dates.
#' @param from_formats A character vector of formats. Should be the same length as `to_formats`.
#' @param to_formats A character vector of formats. Should be the same length as `from_formats`.
#'
#' @return A character vector with dates changed into `to_format`.
#' @export
#'
#' @examples
#' dates <- c("10/23/2022", "01/01/2018", "10/2022", "01/2018", "2019/05", "2025")
#' date_order_change(dates,
#'                   from_formats = c("%m/%d/%Y", "%Y-%m-%d", "%Y/%m", "%m/%Y", "%Y"),
#'                   to_formats = c("%d/%m/%Y", "%d/%m/%Y", "%m/%Y", "%m/%Y", "%Y"))
date_order_change <- function(x,
                              from_formats,  
                              to_formats){
  
  if(length(from_formats) != length(to_formats)){
    
    stop("From and to format lengths should be equal!")
    
  }
  
  
  n_formats <- length(from_formats)
  
  formatted_txt_mat <- sapply(1:n_formats, 
                              FUN = function(format_ind){
                                
                                from_f <- from_formats[format_ind]
                                to_f <- to_formats[format_ind]
                                
                                out <- format(
                                  lubridate::fast_strptime(x, 
                                                           format = from_f),
                                  format = to_f)
                                
                                return(out)
                                
                              })
  
  if(all(is.na(formatted_txt_mat))){
    
    formatted_txt <- rep(NA_character_, 
                         times = length(x))
    
  } else if(is.vector(formatted_txt_mat)){
    
    formatted_txt <- na.omit(formatted_txt_mat)
    
  } else{
    
    formatted_txt <- apply(formatted_txt_mat, 
                           MARGIN = 1, 
                           FUN = function(x){ 
                             return(ifelse(all(is.na(x)), 
                                           NA_character_, 
                                           na.omit(x)))
                           }, 
                           simplify = TRUE)
    
  }
  
  return(formatted_txt)
  
}






## TESTING 



