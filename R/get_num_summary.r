##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                            ##
## Define functions necessary to get summary statistics for a ##
## numerical variable                                         ##
##                                                            ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##



#' Get the numerical summary by multiple categorical by variables
#'
#' @param .data A data frame or a data frame extension (e.g. a tibble).
#' @param .num_var Primary (numerical) variable of interest.
#' @param ... Additional variables to count by. This can be left blank.
#' @param .na.rm A logical value indicating whether `NA` values should be stripped before the computation proceeds.
#' @param .output_digits Number of decimal points in the output for the mean and standard deviation. The default is set to 2.
#'
#' @return A tibble with the by variables (if specified in `...`) and 3 columns. Number of values (`N`), Number of non-missing values (`n`), Mean-SD (`mean_sd`), Median-Q1-Q3 (`median_q1_q3`) and Min-Max (`min_max`).
#' The number of rows depends on the combinations of the by variables, if specified. Will have one row if by variables are not specified.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' starwars %>%
#' get_num_summary(.num_var = height,
#'                 species, gender,
#'                 .na.rm = TRUE,
#'                 .output_digits = 1)
get_num_summary <- function(.data,
                            .num_var,
                            ...,
                            .na.rm = FALSE,
                            .output_digits = 2,
                            .drop = dplyr::group_by_drop_default(.data)){
  
  tmp_dat <- dplyr::ungroup(x = .data)
  
  tmp_dat <- dplyr::select(.data = tmp_dat,
                           {{ .num_var }}, 
                           ...)
  
  
  if(rlang::dots_n(...) > 0){
    
    tmp_dat <- dplyr::mutate(.data = tmp_dat,
                             dplyr::across(.cols = c(...),
                                           .fns = forcats::as_factor))
    
    tmp_dat <- dplyr::group_by(.data = tmp_dat,
                               ...,
                               .add = TRUE,
                               .drop = .drop)
    
  } 
  
  
  tmp_out <- dplyr::summarise(.data = tmp_dat,
                              .N = length({{ .num_var }}),
                              .n = sum(!is.na({{ .num_var }})),
                              .mean = mean({{ .num_var }},
                                           na.rm = .na.rm),
                              .sd = stats::sd({{ .num_var }},
                                              na.rm = .na.rm),
                              .median = stats::median({{ .num_var }},
                                                      na.rm = .na.rm),
                              .q1 = stats::quantile({{ .num_var }},
                                                    0.25,
                                                    na.rm = .na.rm),
                              .q3 = stats::quantile({{ .num_var }},
                                                    0.75,
                                                    na.rm = .na.rm),
                              .min = min({{ .num_var }},
                                         na.rm = .na.rm),
                              .max = max({{ .num_var }},
                                         na.rm = .na.rm),
                              .groups = "drop")
  
  tmp_out <- dplyr::mutate(.data = tmp_out,
                           mean_sd = get_num1_num2_txt(num1 = .mean,
                                                       num2 = .sd,
                                                       .output_digits = .output_digits),
                           median_q1_q3 = get_num1_num2_num3_txt(num1 = .median,
                                                                 num2 = .q1,
                                                                 num3 = .q3,
                                                                 .output_digits = .output_digits),
                           min_max = paste0(format_number(.min, .output_digits = .output_digits),
                                            ", ",
                                            format_number(.max, .output_digits = .output_digits)))
  
  out <- dplyr::select(.data = tmp_out,
                       ..., N = .N, n = .n, mean_sd, median_q1_q3, min_max)
  
  
  return(out)
  
}
