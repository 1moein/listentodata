#' Open a dialog to select a csv data file
#' 
#' This function opens a file selection dialog and
#'  allows user to select a file. Any file can be 
#'  selected, but only csv files will be accepted.
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
#' df = load_csv_data()
#' }
#' 
load_csv_data <- function(){

  # print("\n Please select the appropriate .csv data file!", "I'm waiting ...\n\n", quote = FALSE)
  # print(" Can't see the file selector dialog?\n Look for it behind your other open apps/windows.\n", quote = FALSE)

  filename = file.choose(new = FALSE)
  setwd(dirname(filename))
  extension = tolower(substr(filename, nchar(filename)-2, nchar(filename)))
  if (extension=="csv"){
    cat("Thank you! Your selected .csv file has been recieved!\n")
    cat("You now need to run your next line of Code...\n")
    mydata = utils::read.csv(file=filename)

  } else {
    cat("ERROR:\n")
    cat("      We can't use the file you have selected.\n")
    cat("      It is not a CSV data file. Please try again...\n")
}

  if (extension=="csv"){
    return(mydata)
  } else {
   return()
  }
}

