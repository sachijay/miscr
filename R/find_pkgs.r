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
  
  direct_lines <- grep(pattern = ".*\\:\\:.*",
                       x = split_lines,
                       value = TRUE)
  
  
  library_pkgs <- get_lib_req_pkgs(library_lines)
  
  require_pkgs <- get_lib_req_pkgs(require_lines)
  
  direct_pkgs <- gsub(pattern = "[[:punct:]\\:]",
                      replacement = "",
                      regmatches(direct_lines,
                                 gregexpr("(^\\w+?\\:\\:)|([[:punct:]]\\w+?\\:\\:)",
                                          direct_lines)))
  
  
  pkgs <- unique(
    c(library_pkgs, require_pkgs, direct_pkgs)
  )
  
  
  if(details){
    
    details_df <- data.frame(invoke = c("library", 
                                        "require", 
                                        "::"),
                             n_pkgs = c(length(library_pkgs), 
                                        length(require_pkgs), 
                                        length(direct_pkgs)),
                             pkg_list = c(paste(library_pkgs, collapse = ", "),
                                          paste(require_pkgs, collapse = ", "),
                                          paste(direct_pkgs, collapse = ", ")))
    
    out <- list(pkgs = pkgs,
                details_df = details_df)
    
  } else {
    
    out <- pkgs
    
  }
  
  return(out)
  
}


find_pkgs <- function(file = NULL,
                      dir = NULL,
                      details = FALSE){
  
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
      
      out <- list(pkgs = all_pkgs,
                  details_df = data.frame(file = rep(r_files, each = 3),
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

find_pkgs(dir = "Z:/documents/shiny_apps/watershed/",
          details = TRUE)

find_pkgs(file = "Z:/documents/shiny_apps/watershed/Install_Packages.R",
          details = TRUE)

