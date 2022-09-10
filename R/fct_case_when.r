##+++++++++++++++++++++++++++++++++++##
##                                   ##
## Adds factor levels to a case_when ##
##                                   ##
##+++++++++++++++++++++++++++++++++++##


#' Adds factor levels to a case_when
#'
#' @param ... `dplyr::case_when()` code.
#'
#' @return Output from `case_when()` with factor levels added. The factor order is the same as specified in the syntax. 
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' df <- mutate(starwars,
#'              height_cat = fct_case_when(
#'                height <= 150 ~ "<= 150",
#'                height > 150 ~ "> 150"
#'              ))
#' 
#' levels(df$height_cat)
#' 
#'
fct_case_when <- function(...) {
  
  args <- as.list(match.call())
  
  levels <- sapply(args[-1], function(f) f[[3]])
  
  levels <- levels[!is.na(levels)]
  
  out <- factor(dplyr::case_when(...), 
                levels = levels)
  
  return(out)
  
}


