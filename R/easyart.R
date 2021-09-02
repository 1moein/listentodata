# Times Table on a circle
# Author: Dr. Khanlari
# Jan. 2020

# This surprisingly fun code shows examples of the following:
# 1. Math functions
# 2. The colon operator :
# 3. The seq() function
# 4. The plot() function
# 5. Creating vectors using the c() function
# 6. For Loops

# For the math behind it all, see https://youtu.be/qhbuKbxJsk8


easyart <- function(n=2,mycolor="deeppink", m=0){
  
#' Easy Math Art: Times table on a Circle
#' 
#' This function draws a circle, places 1000 equi-distanced dots on it
#' and then connects them by a pattern determined by the value of n.
#' For the interesting math behind it all, watch: https://youtu.be/qhbuKbxJsk8
#' @param n Can be a positive integer, or even a real number - defaults to n = 2.
#' Choosing n=0 shows 23 interesting patterns.
#'
#' @param mycolor Any color name in R. To get color names try colors() in the Console.
#' defaults to mycolor = "deeppink"
#'
#' @param m optional argument that defaults to 0. If a value for it is provided, you will see all the patterns from n=1 upto n=m. For example, setting m=200, shows you all the pattern from 1 to 200")
#' 
#' @return
#' @export
#'
#' @examples
#' easyart(2)
#' easyart(60,"darkblue")
  drawit = function(n=2,mycolor="black", m=0){
    
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
  interesting <- c(2,3,4,5,21,29,33, 34, 49, 51, 66, 67, 68, 73, 75, 76, 79, 80, 81, 86, 91, 99, 200)
  cat("\014")
  allcolors = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")
  if (n==0){
    # if (length(dev.list())!= 0) dev.off()
    for (i in 1:length(interesting)){
    cat("\014")
      cat("Now showing a few interesting ones! ")
      cat("\nif the images are not changing, your computer\n can't handle the graphical load.")
      cat("\nYou can click the red stop sign at the top right\n corner of this pane to stop code execution.")
      # readline(prompt="Press [enter] to continue")
    drawit(interesting[i], allcolors[sample(1:11,1)])
    Sys.sleep(0.5)
    }
    cat("\nEnd of Show!\n")
    } else if (n<0)  {
      n=2
      cat(" Nice try!\n But I can't draw with negative numbers!\n How about n=2?\n")
      drawit(n, mycolor)
    } else if (m!=0){
      # if (length(dev.list())!= 0) dev.off()
      for (i in 1:m){
        cat("\014")
        cat(paste0("Now showing for n= 1 to ", m))
        cat("\nif the images are not changing, your computer\n can't handle the graphical load.")
        cat("\nYou can click the red stop sign at the top right\n corner of this pane to stop code execution.")
        # cat("Now showing you 1 - 60! ")
        # readline(prompt="Press [enter] to continue")
        drawit(i, allcolors[sample(1:11,1)])
        Sys.sleep(0.5)
      }
      cat("\nEnd of Show!\n")
      
      } else {
      drawit(n, mycolor)
    }

}








