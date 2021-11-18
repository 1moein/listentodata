#' Open a dialog to select a csv data file
#' 
#' This function opens a file selection dialog and
#'  allows user to select a file. Any file can be 
#'  selected, but only .txt files will be accepted.
#'  The working directory will also be set to the
#'  folder that contains the selected file. The working
#'  directory is the directory where the R session looks for data 
#'  files or saves the results.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' df = load_text_data()
#' }
#' 
load_text_data <- function(){

  # print("\n Please select the appropriate .txt data file!", "I'm waiting ...\n\n", quote = FALSE)
  # print(" Can't see the file selector dialog?\n Look for it behind your other open apps/windows.\n", quote = FALSE)

  filename = file.choose(new = FALSE)
  setwd(dirname(filename))
  extension = tolower(substr(filename, nchar(filename)-2, nchar(filename)))
  if (extension=="txt"){
    cat("Thank you! Your selected .txt file has been recieved!\n")
    cat("You now need to run your next line of Code...\n")
    mydata = readLines(con=filename)

  } else {
    cat("ERROR:\n")
    cat("      We can't use the file you have selected.\n")
    cat("      It is not a .txt data file. Please try again...\n")
}

  if (extension=="txt"){
    return(mydata)
  } else {
   return()
  }
}

