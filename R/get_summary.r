library(tidyverse)

get_n_perc_txt <- function(n, 
                           prop,
                           output_digits){
  
  out <- paste0(n, 
                " (", round(prop*100, digits = output_digits), "%)")
  
  return(out)
  
}

get_mean_sd_txt <- function(mean,
                            sd,
                            output_digits){
  
  mean_digits <- NULL
  sd_digits <- NULL
  
  if(length(output_digits) == 1){
    
    mean_digits <- output_digits
    sd_digits <- output_digits
    
  } else if(length(output_digits) == 2){
    
    mean_digits <- output_digits[1]
    sd_digits <- output_digits[2]
    
  } else{
    
    warning("Only the first 2 arguments of output_digits will be used")
    
    mean_digits <- output_digits[1]
    sd_digits <- output_digits[2]
    
  }
  
  out <- paste0(round(mean, digits = mean_digits),
                " (", round(sd, digits = sd_digits), ")")
  
  return(out)
  
}

get_median_q1_q3_txt <- function(median,
                                 q1, q3,
                                 output_digits){
  
  median_digits <- q1_digits <- q3_digits <- NULL
  
  if(length(output_digits) == 1){
    
    median_digits <- q1_digits <- q3_digits <- output_digits
    
  } else if(length(output_digits) == 2){
    
    median_digits <- output_digits[1]
    q1_digits <- q3_digits <- output_digits[2]
    
  } else if(length(output_digits) == 3){
    
    median_digits <- output_digits[1]
    q1_digits <- output_digits[2]
    q3_digits <- output_digits[3]
    
  } else{
    
    warning("Only the first 3 arguments of output_digits will be used")
    
    median_digits <- output_digits[1]
    q1_digits <- output_digits[2]
    q3_digits <- output_digits[3]
    
  }
  
  out <- paste0(round(median, digits = median_digits),
                " (", round(q1, digits = q1_digits), ", ", round(q3, digits = q3_digits), ")")
  
  return(out)
  
}


#' Generates a summary table for a categorical variable
#'
#' @param .data A data frame or a data frame extension (e.g. a tibble).
#' @param .cat_var Primary variable of interest.
#' @param ... Additional variables to count by. This can be left blank.
#' @param .output_digits Number of decimal points in the output percentages. The default is set to 1.
#'
#' @return A tibble with counts and percentages for `.cat_var` grouped by variables in `...`. 
#' 
#' @details The percentages are calculated for each set of `...` combinations.
#' @note  The `.cat_var` and `...` are converted to factor variables.
#' 
#' @export
#' starwars %>% 
#' get_cat_summary(.cat_var = gender,
#'                 species, eye_color)
#'
#' @examples
get_cat_summary <- function(.data,
                            .cat_var,
                            ...,
                            .output_digits = 1){
  
  tmp_dat <- .data %>% 
    ungroup() %>% 
    select({{ .cat_var }}, ...) %>% 
    mutate(
      
      {{ .cat_var }} := {{ .cat_var }} %>% 
        as_factor()
      
    )
  
  if(rlang::dots_n(...) > 0){
    
    tmp_dat <- tmp_dat  %>% 
      mutate(
        
        across(
          .cols = c(...),
          .fns = as_factor
        )
        
      ) %>% 
      group_by(
        
        ...,
        
        .add = TRUE
      )
    
  } 
  
  out <- tmp_dat %>%
    group_by(
      
      {{ .cat_var }},
      
      .add = TRUE
    ) %>% 
    summarise(
      
      .n = n(),
      
      .groups = "drop_last"
    ) %>% 
    mutate(
      
      .prop = .n/sum(.n),
      n_perc = get_n_perc_txt(n = .n, 
                              prop = .prop, 
                              output_digits = .output_digits)
      
    ) %>% 
    select(..., {{ .cat_var }}, n_perc) %>% 
    ungroup()
  
  return(out)
  
}

get_num_summary <- function(.data,
                            .num_var,
                            ...,
                            .na.rm = FALSE,
                            .output_digits = 2){
  
  tmp_dat <- .data %>% 
    ungroup() %>% 
    select({{ .num_var }}, ...)
  
  if(rlang::dots_n(...) > 0){
    
    tmp_dat <- tmp_dat  %>% 
      mutate(
        
        across(
          .cols = c(...),
          .fns = as_factor
        )
        
      ) %>% 
      group_by(
        
        ...,
        
        .add = TRUE
      )
    
  } 
  
  out <- tmp_dat %>%
    summarise(
      
      .mean = mean({{ .num_var }}, 
                   na.rm = .na.rm),
      .sd = sd({{ .num_var }}, 
               na.rm = .na.rm),
      .median = median({{ .num_var }}, 
                       na.rm = .na.rm),
      .q1 = quantile({{ .num_var }}, 
                     0.25,
                     na.rm = .na.rm),
      .q3 = quantile({{ .num_var }}, 
                     0.75,
                     na.rm = .na.rm),
      
      .groups = "drop"
    ) %>% 
    mutate(
      
      mean_sd = get_mean_sd_txt(mean = .mean,
                                sd = .sd,
                                output_digits = .output_digits),
      median_q1_q3 = get_median_q1_q3_txt(median = .median,
                                          q1 = .q1,
                                          q3 = .q3,
                                          output_digits = .output_digits)
      
    ) %>% 
    select(..., mean_sd, median_q1_q3)
  
  return(out)
  
}


starwars %>%
  get_cat_summary(.cat_var = gender,
                  species, eye_color)

starwars %>%
  get_num_summary(.num_var = height,
                  species, gender,
                  .na.rm = TRUE)
