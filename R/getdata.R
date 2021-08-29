#' Opens a dialog for user to select a csv data file
#'
#' @return
#' @export
#'
#' @examples
#' df = getdata()
getdata <- function(){

  print("\n Please select the appropriate .csv data file!", "I'm waiting ...\n\n", quote = FALSE)
  print(" Can't see the file selector dialog?\n Look for it behind your other open apps/windows.\n", quote = FALSE)

  filename = file.choose(new = FALSE)
  setwd(dirname(filename))
  extension = tolower(substr(filename, nchar(filename)-2, nchar(filename)))
  if (extension=="csv"){
    cat(blue$bold(" \n Thank you! Your selected .csv file has been recieved!\n "))
    cat(blue$bold(" You now need to run your next line of Code. \n"))
    mydata = read.csv(file=filename)

  } else {
    cat(red$bold("\n ERROR: We can't use the file you have selected,\n"))
    cat(red(" It is not a CSV data file."))
    cat(red(" \n Run the getdata() line to try again."))
}

  if (extension=="csv"){
    return(mydata)
  } else {
   return()
  }
}

