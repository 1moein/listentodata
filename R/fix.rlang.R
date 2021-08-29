#' Fix rlang package
#'
#' This function removes and installs rlang to update it to the newest version so that the broom package can be installed
#' @keywords rlang
#' @export
#' @examples
#' fix.rlang()
#'
fix.rlang = function(){
  remove.packages("rlang")
  install.packages("rlang")
}