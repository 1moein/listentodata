#' Opens a dialog for user to select a csv data file
#'
#' @return
#' @export
#'
#' @examples
#' df = getdata()
getdata <- function(){
  #print("testing....")
  #library(crayon)
  cat(
    " \n" %+%
    " Please select the appropriate .csv data file!\n\n" %+%
    " If you can't see the file selection dialog,\n look for it behind your other open windows or apps. \n" %+%
    " \n " %+%
    "OK, I'm waiting ...................." %+%
    " \n "
  )
  filename = file.choose(new = FALSE)
  setwd(dirname(filename))
  extension = tolower(substr(filename, nchar(filename)-2, nchar(filename)))
  if (extension=="csv"){
    cat(blue$bold(" \n Thank you! Your selected .csv file has been recieved!\n "))
    cat(blue$bold(" You now need to run your next line of Code. "))
    mydata = read.csv(file=filename)

  } else {
    cat(
      " \n" %+%
        red$bold(" ERROR: We can't use the file you have selected,\n") %+%
        red(" because it's not a data file with the .csv extension.") %+%
        red(" \n You need to run the GetData() line again\n and select the right data file!\n ")
    )
}

  if (extension=="csv"){
    return(mydata)
  } else {
  return()
  }
}

