#' Add comma Function
#'
#' This function allows you to group every three digits in a number using a comma.
#' @param x The number you wish to add commas to.
#' @keywords comma
#' @export
#' @examples
#' addcomma(23564715565)
#'
addcomma <- function(x){
  y <- prettyNum(x,big.mark=",",scientific=FALSE)
  return(y)
}