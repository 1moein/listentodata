#' Opens a dialog for user to select a csv data file
#'
#' @return
#' @export
#'
#' @examples
#' df = getdata()
getdata <- function(){

  # print("\n Please select the appropriate .csv data file!", "I'm waiting ...\n\n", quote = FALSE)
  # print(" Can't see the file selector dialog?\n Look for it behind your other open apps/windows.\n", quote = FALSE)

  filename = file.choose(new = FALSE)
  setwd(dirname(filename))
  extension = tolower(substr(filename, nchar(filename)-2, nchar(filename)))
  if (extension=="csv"){
    cat("Thank you! Your selected .csv file has been recieved!\n")
    cat("You now need to run your next line of Code...\n")
    mydata = read.csv(file=filename)

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

