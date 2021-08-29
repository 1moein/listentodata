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
    print("Thank you! Your selected .csv file has been recieved!", quote = FALSE)
    print("You now need to run your next line of Code...", quote = FALSE)
    mydata = read.csv(file=filename)

  } else {
    print("ERROR:", quote = FALSE)
    print("      We can't use the file you have selected.", quote = FALSE)
    print("      It is not a CSV data file.", quote = FALSE)
    print("      Run the getdata() line to try again...", quote = FALSE)
}

  if (extension=="csv"){
    return(mydata)
  } else {
   return()
  }
}

