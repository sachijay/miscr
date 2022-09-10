##++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                  ##
## Finds all R packages used in a file or directory ##
##                                                  ##
##++++++++++++++++++++++++++++++++++++++++++++++++++##


#' Get a vector of package names given the contents inside `library()` or `required()`.
#'
#' @param x A string with the package names as presented inside `library()` or `required()`..
#'
#' @return 
#' A character vector of package names. 
#' 
#' @noRd
get_lib_req_pkgs <- function(x){
  
  pkgs <- gsub(pattern = "\"",
               replacement = "",
               gsub(pattern = "[\\(\\)]",
                    replacement = "",
                    regmatches(x,
                               gregexpr("\\(.*?\\)",
                                        x))))
  
  return(pkgs)
}


#' Find packages inside a single file.
#'
#' @param file A string of the file name to read.
#' @param details A descriptive table of the number of packages, a list of packages invoked using `library()`, `required()`, `::` or `:::`. The default is `FALSE`.
#'
#' @return 
#' A character vector of packages in the file if `details = FASLE`. If `details = TRUE`, a list with 2 elements. A character vector of packages in `pkgs` and a details dataframe in `details_df`.
#' 
#' @noRd
find_pkgs_file <- function(file,
                           details = FALSE){
  
  file_content <- suppressWarnings(readLines(con = file))
  
  lines_wo_comments <- grep(pattern = "^\\s*#",
                            x = file_content,
                            value = TRUE,
                            invert = TRUE)
  
  lines_ws_removed <- gsub(pattern = "\\s",
                           replacement = "",
                           x = lines_wo_comments)
  
  split_lines <- unlist(
    strsplit(lines_ws_removed,
             split = ";")
  )
  
  
  library_lines <- grep(pattern = "library",
                        x = split_lines,
                        value = TRUE)
  
  require_lines <- grep(pattern = "require",
                        x = split_lines,
                        value = TRUE)
  
  direct_lines <- grep(pattern = "\\w+\\:{2}\\w+",
                       x = split_lines,
                       value = TRUE)
  
  internal_direct_lines <- grep(pattern = "\\w+\\:{3}\\w+",
                                x = split_lines,
                                value = TRUE)
  
  
  library_pkgs <- get_lib_req_pkgs(library_lines)
  
  require_pkgs <- get_lib_req_pkgs(require_lines)
  
  direct_pkgs <- gsub(pattern = "[[:punct:]\\:]",
                      replacement = "",
                      unlist(regmatches(direct_lines,
                                        gregexpr("(^\\w+?\\:\\:)|([[:punct:]]\\w+?\\:\\:)",
                                                 direct_lines))))
  
  
  
  internal_direct_pkgs <- gsub(pattern = "[[:punct:]\\:]",
                               replacement = "",
                               unlist(regmatches(internal_direct_lines,
                                                 gregexpr("(^\\w+?\\:\\:\\:)|([[:punct:]]\\w+?\\:\\:\\:)",
                                                          internal_direct_lines))))
  
  
  pkgs <- unique(
    c(library_pkgs, require_pkgs, direct_pkgs, internal_direct_pkgs)
  )
  
  
  if(details){
    
    details_df <- data.frame(invoke = c("library", 
                                        "require", 
                                        "::",
                                        ":::"),
                             n_pkgs = c(length(library_pkgs), 
                                        length(require_pkgs), 
                                        length(direct_pkgs), 
                                        length(internal_direct_pkgs)),
                             pkg_list = c(paste(library_pkgs, collapse = ", "),
                                          paste(require_pkgs, collapse = ", "),
                                          paste(direct_pkgs, collapse = ", "),
                                          paste(internal_direct_pkgs, collapse = ", ")))
    
    out <- list(pkgs = pkgs,
                details_df = details_df)
    
  } else {
    
    out <- pkgs
    
  }
  
  return(out)
  
}


#' Get packages used in a file or a directory.
#'
#' @param file A file path to search for packages.
#' @param dir A directory path to search for packages. If `file` is already specified this is ignored. See details.
#' @param details A logical value. Is a detailed description of how packages are invoked needed. The default is `FALSE`.
#' @param simplify A logical value indicating if the `dir` name should be removed from file names in details. This value is ignored if a `file` is  specified or if `details = FALSE`. 
#'
#' @details 
#' When scanning a directory only files with the extension `.r` or `.R` searched.
#'
#' @return 
#' A character vector of packages in the file if `details = FASLE`. If `details = TRUE`, a list with 2 elements. A character vector of packages in `pkgs` and a details dataframe in `details_df`.
#' 
#' @export
find_pkgs <- function(file = NULL,
                      dir = NULL,
                      details = FALSE,
                      simplify = TRUE){
  
  if(!is.null(file)){
    
    file_pkgs <- find_pkgs_file(file,
                                details = details)
    
    out <- file_pkgs
    
  } else if(!is.null(dir)){
    
    list_files <- list.files(path = dir,
                             recursive = TRUE,
                             full.names = TRUE)
    
    r_files <- grep(pattern = ".[rR]$",
                    list_files,
                    value = TRUE)
    
    all_file_pkgs <- lapply(r_files, find_pkgs_file, details = details)
    
    
    if(details){
      
      all_pkgs <- unique(
        unlist(
          lapply(all_file_pkgs, function(result){
            return(result$pkgs)
          }
          )
        )
      )
      
      all_pkg_details <- do.call("rbind", 
                                 lapply(all_file_pkgs, function(result){
                                   return(result$details_df)
                                 }))
      
      r_files_out <- r_files
      
      if(simplify){
        
        r_files_out_tmp <- gsub(pattern = dir,
                                replacement = "",
                                r_files_out)
        
        r_files_out <- gsub(pattern = "^/",
                            replacement = "",
                            r_files_out_tmp)
        
      }
      
      out <- list(pkgs = all_pkgs,
                  details_df = data.frame(file = rep(r_files_out, each = 4),
                                          all_pkg_details))
      
    } else {
      
      all_pkgs <- unique(
        unlist(all_file_pkgs)
      )
      
      out <- all_pkgs
      
    }
    
  } else {
    
    out <- NULL
    stop("No file or directory specified!")
    
  }
  
  return(out)
  
}
