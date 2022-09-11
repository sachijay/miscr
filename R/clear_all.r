##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##                                                            ##
## Clears the workspace, shuts down all open graphics devices ##
## and clears the console.                                    ##
##                                                            ##
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##


#' Clears the workspace, console and shuts down all open graphics devices.
#'
#' @export
#'
clear_all <- function(){
  
  ## Remove all variables from the workspace
  rm(list = ls())
  
  
  ## Remove all figures
  grDevices::graphics.off()
  
  
  ## Clear console
  cat("\014")
  
  
}
