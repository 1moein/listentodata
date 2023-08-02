#' Create patterns on a circle using the times table
#' 
#' This function draws a circle, places 1000 equi-distanced dots on it
#' and then connects them by multiples of n. The patterns are repeated after n=200 such 
#' that for every n, n+200 will generate an identical pattern. 
#' For the math behind these patterns, watch this video on YouTube: https://youtu.be/qhbuKbxJsk8
#' 
#' @param n Can be a positive integer, or even a real number - defaults to n = 2.
#' Choosing n=0 shows a demo of 23 interesting patterns ant their n values.
#'
#' @param mycolor Any color name in R. To get color names try colors() in the Console.
#' defaults to mycolor = "deeppink"
#'
#' @param m optional argument that defaults to 0. If a positive non-zero value is provided for m, all patterns from n=1 upto n=m will be displayed. For example, setting m=10 displays all the patterns from n=1 to n=10, effectively ignoring the values for n and mycolor.")
#' 
#' @param printall optional argument that defaults to 0. If printall=1, all patterns from n=1 upto n=200 will be saved in a pdf file and the details will be shared in the Console.")
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' easyart(n=51)
#' easyart(n=0)
#' easyart(m=5)
#' }
easyart <- function(n=2,mycolor="deeppink", m=0, printall=0){
  
  drawit = function(n=2,mycolor="black", m=0, printall=0){
    
  # set plot area (parameter: pty) equal to square
  graphics::par(pty="s")
  
  # plot a circle
  z <- 0:1000/500   # create 1000 angle values between 0 and 2pi
  # find the x-y coordinates for the intersection of the line of those 
  # angles with a unit circle
  x <- cospi(z)
  y <- sinpi(z)

    # draw the circle using the x-y coordinates
  plot(x,y, type="l", ann=FALSE , xaxt='n', yaxt='n', axes = FALSE, col=mycolor)
  graphics::title(main=paste0("n = ",n))
  
  # create 200 angles between 0.01 and 2pi
  a <- seq(0.01,2, by=0.01) 
  # find the x-y coordinates for those angles
  x0 <- cospi(a)
  y0 <- sinpi(a)
  x1 <- cospi(n*a)
  y1 <- sinpi(n*a)
  # draw lines from (x0,y0) to (x1,y1)
  graphics::segments(x0, y0, x1, y1, col = mycolor)
  }
  
  
  # Now let's write a loop over possible values of n: Classroom activity
  
  # Write a loop over the values in the vecto "interesting"
  interesting <- c(2,3,4,5,21,26, 29,33, 34, 49, 41, 51, 66, 67, 68, 73, 75, 76, 79, 80, 81, 86, 91, 99, 101, 102, 134, 161,  176, 199,200)
  cat("\014")
  allcolors = c("#9E0142", "#3288BD", "#0DA907" , "#F46D43", "#3288BD", "#EC2055", "#FDAE61", "#ABDDA4", "#5E4FA2", "#66C2A5", "#D53E4F", "981285")
  # #check colors
  # scales::show_col(allcolors)
  if (n==0){
    # if (length(dev.list())!= 0) dev.off()
    for (i in 1:length(interesting)){
    cat("\014")
      cat("Now showing a few interesting ones in the Plots Panel! ")
      cat("\nif the images are not changing, your computer\n may be slow.")
      cat("\nYou can always click the red stop sign at the top right\n corner of this panel to stop code execution.")
      
    drawit(interesting[i], allcolors[sample(1:12,1)])
    Sys.sleep(0.75)
    }
    cat("\nEnd of Show!\n")
    } else if (n<0)  {
      n=2
      cat(" Nice try!\n But I can't draw with negative numbers!\n I'll give you n=2?\n")
      drawit(n, mycolor)
    } else if (m!=0){
      # if (length(dev.list())!= 0) dev.off()
      for (i in 1:m){
        cat("\014")
        cat(paste0("Now showing results in the Plots tab for n= 1 to ", m))
        cat("\nif the images are not changing, your computer\n can't handle the graphical load.")
        cat("\nYou can click the red stop sign at the top right\n corner of this pane to stop code execution.")
        # cat("Now showing you 1 - 60! ")
        # readline(prompt="Press [enter] to continue")
        drawit(i, allcolors[sample(1:12,1)])
        Sys.sleep(0.75)
      }
      cat("\nEnd of Show!\n")
      
      } else {
      drawit(n, mycolor)
    }

  
  
  
  ################# save results in a pdf file ############
if (!is.numeric(printall)) printall = 0
if (is.numeric(printall)) printall = floor(printall)
if (printall==1){
  larger = 1
  suppressWarnings(resres <- try(grDevices::pdf("! Results_easyart200.pdf", height=larger*8.5, width=larger*11), silent = TRUE))

    for (i in 1:200) drawit(i, allcolors[sample(1:12,1)])
    
  grDevices::dev.off()
  
  
  
  if (!is.null(resres)){
    cat("\n\n ERROR:\n We were not able to\n save the 200 patterns in \"! Results_easyart200.pdf\"")
    cat(" \n This is probably due to a PDF file with the same name being open.\n")
    cat(" Make sure you close that file, and then run the previous line of code again.")
    
  } else {
    cat("\n\n easyart() generates the patterns in the Plots tab (lower right pane).\n")
    cat("\n But we also saved all patterns from n=1 to n=200 in a pdf file named: \"! Results_easyart200.pdf\"\n")
    cat(" You can find this file in this folder on your computer:\n ")
    cat(as.character(getwd()))
    cat(" \n\n ")

  }
  
}
}








