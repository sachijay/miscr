##+++++++++++++++++++++++++++++++++++++++++++++##
##                                             ##
## Installs missing packages from CRAN         ##
## Template file: Change packages as necessary ##
##                                             ##
##+++++++++++++++++++++++++++++++++++++++++++++##


#' Installs specified packages from CRAN if missing.
#'
#' @param cran_pkgs A character vector with package names as elements.
#' @param repos A character vector of the base URL(s) of the repositories to use. The default is CRAN ("https://cloud.r-project.org")
#'
#' @export
#'
#' @examples
#' install_missing_pkgs(cran_pkgs = c("tidyverse"))
install_missing_pkgs <- function(cran_pkgs,
                                 repos = "https://cloud.r-project.org"){
  
  
  if(length(missing_pkgs <- setdiff(cran_pkgs, row.names(utils::installed.packages()))) > 0){
    
    message("Installing missing package(s): ",
            paste(missing_pkgs, collapse = ", "))
    
    utils::install.packages(missing_pkgs, repos = repos)
    
  } else {
    
    message("All packages already installed")
    
  }
  
}
