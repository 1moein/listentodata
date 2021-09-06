#' Select data folder or results folder and set it as working directory
#' 
#' This function opens a folder selection dialog and
#'  allows user to select a folder. Then, it sets the
#'  working directory to the selected directory. The
#'  working directory is the directory where the R 
#'  session looks for data files or saves the results.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' select_folder()
#' }
#' 
select_folder = function() {
  caption = 'Select the folder where your data file is...'
  if (exists('utils::choose.dir')) {
    thisdir = utils::choose.dir(caption = caption) 
  } else {
    thisdir = easycsv::choose_dir()
  }
  setwd(thisdir)
}

