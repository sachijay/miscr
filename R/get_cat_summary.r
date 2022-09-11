##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                            ##
## Define functions necessary to get summary statistics for a ##
## categorical variable                                       ##
##                                                            ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##



#' Generates a summary table for a categorical variable
#'
#' @param .data A data frame or a data frame extension (e.g. a tibble).
#' @param .cat_var Primary (categorical) variable of interest.
#' @param ... Additional variables to count by. This can be left blank.
#' @param .output_digits Number of decimal points in the output percentages. The default is set to 1.
#' @param .with_percentage A logical indicating if the percentage mark (%) should be shown. The default is `TRUE`.
#'
#' @return A tibble with counts and percentages for `.cat_var` grouped by variables in `...`. 
#' 
#' @details The percentages are calculated for each set of `...` combinations.
#' @note  The `.cat_var` and `...` are converted to factor variables.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' starwars %>% 
#' get_cat_summary(.cat_var = gender,
#'                 species, eye_color)
get_cat_summary <- function(.data,
                            .cat_var,
                            ...,
                            .output_digits = 1,
                            .with_percentage = TRUE){
  
  tmp_dat <- dplyr::ungroup(x = .data)
  
  tmp_dat <- dplyr::select(.data = tmp_dat, 
                           {{ .cat_var }}, 
                           ...)
  
  tmp_dat <- dplyr::mutate(.data = tmp_dat,
                           {{ .cat_var }} := forcats::as_factor({{ .cat_var }}))

  
  if(rlang::dots_n(...) > 0){
    
    tmp_dat <- dplyr::mutate(.data = tmp_dat,
                             dplyr::across(.cols = c(...),
                                           .fns = forcats::as_factor))
    
    tmp_dat <- dplyr::group_by(.data = tmp_dat,
                               ...,
                               .add = TRUE)

  } 
  
  
  tmp_dat <- dplyr::group_by(.data = tmp_dat,
                             {{ .cat_var }},
                             .add = TRUE)
  
  out_tmp <- dplyr::summarise(.data = tmp_dat,
                              .n = dplyr::n(),
                              .groups = "drop_last")
  
  out_tmp <- dplyr::mutate(.data = out_tmp,
                           .prop = .n/sum(.n),
                           n_perc = get_n_perc_txt(n = .n,
                                                   prop = .prop,
                                                   .output_digits = .output_digits,
                                                   .with_percentage = .with_percentage))
  
  out_tmp <- dplyr::select(.data = out_tmp,
                           ...,
                           {{ .cat_var }}, 
                           n_perc)
  
  out <- dplyr::ungroup(x = out_tmp)

  
  return(out)
  
}
