

#' Select the folder where results will be saved
#'
#' @export
#'
save.to = function() {
  caption = 'Select where to save the results...'
  if (exists('utils::choose.dir')) {
    thisdir = utils::choose.dir(caption = caption) 
  } else {
    thisdir = tcltk::tk_choose.dir(caption = caption)
  }
  setwd(thisdir)
}

