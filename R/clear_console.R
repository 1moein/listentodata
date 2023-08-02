#' Clear the Console
#' 
#' @description 
#' This function clears the console. 
#' 
#' In RStudio, Console is located in the lower 
#' 
#' left panel where previously-executed codes
#' 
#' and their outputs are displayed. 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' clear_console()
#' }
#' 
clear_console <- function(){

  cat("\014")
  cat("\f")

  # if (as.character(Sys.info()['sysname'])=="Windows"){
  #   cat("\014") #Sends CTRL+L to the Console
  #   cat("\f") #Sends CTRL+L to the Console
  # } else {
  #   cat("\014") #Sends CTRL+L to the Console
  # }
  #
  # # Remove all variables from memory to start fresh
  # rm(list=ls())
  # # If there are plots, delete them to start fresh
  # if (length(dev.list())!= 0) dev.off()
  # 

}

