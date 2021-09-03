
#' Select the folder to  results will be saved
#'
#' @export
#'
look.here = function() {
  caption = 'Select the folder where your data file is...'
  if (exists('utils::choose.dir')) {
    thisdir = utils::choose.dir(caption = caption) 
  } else {
    thisdir = easycsv::choose_dir()
  }
  setwd(thisdir)
}

