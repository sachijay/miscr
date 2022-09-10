##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                              ##
## Outputs the `case_when` code given classes in a table format ##
##                                                              ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##

#' Outputs the `case_when` code given classes in a table format
#'
#' @param classes_dat Dataset with the conditionas and the responses.
#' @param response_col The name of the response column
#' @param output_file A file path for the output file.
#' @param dplyr_code Should the code be compatible with `dplyr` programming. The default is `TRUE`. If `FALSE` the dataset name will be appended to all variables.
#' @param dataset_name Dataset name. Required if `dplyr = FALSE`.
#'
#' @details A case will be defined for each row with the conditions joined by `&`. The column names are considered as the variable names. There can be OR (`|`) statements inside cells.
#'
#' @export
get_case_when_code <- function(classes_dat, 
                               response_col,
                               output_file,
                               dplyr_code = TRUE,
                               dataset_name = NULL){
  
  library(stringr)
  
  if(!dplyr_code && is.null(dataset_name)){
    
    stop("Dataset name required!!!!")
    
  }
  
  response_col_id <- which(names(classes_dat) == response_col)
  
  
  criteria <- classes_dat[, -response_col_id]
  response <- classes_dat[, response_col_id]
  
  criteria_names <- names(criteria)
  
  
  criteria_txt_mat <- sapply(criteria_names, function(cn){
    
    if(dplyr_code){
      
      lead_cn <- cn
      
    } else {
      
      lead_cn <- paste0(dataset_name, "$", cn)
      
    }
    
    
    out <- stringr::str_replace(paste0(lead_cn, " == ", paste0("\'", criteria[, cn], "\'")), 
                                pattern = "\\s*\\|\\s*", 
                                replacement = paste0("\' \\| ", lead_cn, " == \'"))
    
    out <- ifelse(stringr::str_detect(out, "\\s*\\|\\s*"),
                  paste0("(", out, ")"),
                  out)
    
    return(out)
    
  })
  
  criteria_txt <- apply(criteria_txt_mat, 1, paste0, collapse = " &\n")
  
  criteria_response_vec <- paste0(criteria_txt, " ~ ", response)
  
  criteria_response <- paste0(criteria_response_vec,
                              collapse = ",\n")
  
  final_txt <- paste0(response_col, " <- case_when(\n",
                      criteria_response,
                      "\n)")
  
  
  file_conn <- file(output_file)
  writeLines(final_txt, file_conn)
  close(file_conn)
  
}
