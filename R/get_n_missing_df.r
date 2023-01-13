##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                            ##
## Define functions necessary to get number of missing values ##
##                                                            ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##



#' Get the number of missing values for each column.
#'
#' @param .dat Input tibble or dataframe.
#' @param ... Additional arguments to pass to [get_n_perc_txt()].
#'
#' @return A tibble with 2 columns. One for the column and one for the number of missing values.
#' @export
get_n_missing_df <- function(
    .dat,
    ...
){
  
  out_wide <- dplyr::summarise(
    .data = .dat,
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~ get_n_perc_txt(
        n = sum(is.na(.x)),
        prop = sum(is.na(.x))/length(.x)
      )
    )
  )
  
  out <- tidyr::pivot_longer(
    data = out_wide,
      cols = dplyr::everything(),
      names_to = "variable",
      values_to = "n_missing"
    )
  
  return(out)
  
}
