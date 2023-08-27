###########################################################
#        Positioning Analysis with Perceptual Maps        #
###########################################################
#    R Codes for Marketing Analytics                      #
#    Author: Dr. Moein Khanlari                           #
#    Course: Marketing Analytics                          #
#    Version 1.0                                          #
#    Copyright© 2019, Moein Khanlari All rights reserved. #
#    This software is provided to students at the         #
#    University of New Hampshire on an "AS IS" BASIS,     #
#    WITH ABSOLUTELY NO WARRANTIES either expressed or    #
#    implied. The software may not be redistributed       #
#    in whole or part without the express written         #
#    permission of the author.                            #
###########################################################


# # Remove all variables from memory to start fresh
# rm(list=ls())
# # If there are plots, delete them to start fresh
# if (length(dev.list())!= 0) dev.off()


####################################################
#######      Setting the Working Directory   #######
# The working directory is the folder in which
# we will place our R code and data files.

# # Here, I automatically set the working directory
# # to the folder that contains THIS R Script
# if (!require(rstudioapi)) install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# If the above code fails, uncomment the setwd() line below
# and type the path of the data file in the quotation marks:
# setwd("Folder_path_goes_here")

####################################################

# This script is a template for creating perceptual maps.
# First, ask a random sample of customers to rate your brand and competitors' brands
# on several important attributes.
# Then calculate the average rating of each brand on each attribute.
# This script takes such average ratings of brands or products as input
# and creates perceptual maps for a positioning analysis.


# # Here we load the data sets
# # df will contain the perceptual data
# # d1 will contain the preferences data
# df = read.csv("cars_positioning.csv", header = TRUE)
# d1 = read.csv("cars_preferences.csv", header = TRUE)


#' Positioning Analysis
#' 
#' This function conducts a positioning analysis using two data sets 
#' of perceptual and preferences data
#'
#' @param perceptions_data Perceptual data csv file
#' @param preferences_data Preferences data csv file
#' @param resizepaper How much larger should the pdf paper size be to fit everything? Default value is 1.2
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # This is the sample code to be copied and used in a new R Script:
#' library(listentodata)
#' clear_console()
#' perceptions_data = load_csv_data()
#' preferences_data = load_csv_data()
#' resizepaper = 1.2
#' Run_Positioning_Analysis(perceptions_data,preferences_data, resizepaper)
#' }

Run_Positioning_Analysis = function(perceptions_data, preferences_data, resizepaper=1.2) {


df = perceptions_data
d1 = preferences_data
larger = resizepaper
# View the perceptual data
df

# View the top rows of the preferences data
utils::head(d1)

# Attribute names have been given to us as a variable in the data set
# we will convert them to row names for each row and delete the first column

rownames(df) = df[,1]
df = df[,-1]

# View the data set again
df

# Transpose the data set: i.e., switch rows and columns:
# The method we are going to use requires the brands to be the rows
# in the data frame rather than columns.
df = t(df)

# Visualize average preference ratings for each brand
AvP = data.frame(Brands=names(d1[,-1]), Avg=round(colMeans(d1[,-1]),2))
AvP = AvP[order(AvP$Avg, decreasing=TRUE),]

# # Plot Outputs commented out for the function
# bp = graphics::barplot(height = AvP$Avg,
#               ylim=c(0,max(ceiling(AvP$Avg))+2),
#               names.arg=AvP$Brands,
#               col=grDevices::topo.colors(length(AvP$Brands), alpha = 1),
#               main="Average Preferences for Brands",
#               ylab="Average Preference Rating",
#               axisnames = FALSE)
# graphics::text(x=bp, y = AvP$Avg+0.2 , labels=as.character(AvP$Avg))
# graphics::text(bp, graphics::par("usr")[3], labels = AvP$Brands, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)


# Do a principal component analysis on the perceptions data
pca = stats::prcomp(df, scale = TRUE)

# # Show vairances explained by each principal component
# if (!require(factoextra)) install.packages("factoextra")
# library(factoextra)
var_explained = factoextra::fviz_eig(pca, main = "PCA Percentage of Explained Variances", addlabels = TRUE, barfill = "deepskyblue")

# # Plot Outputs commented out for the function
# plot(var_explained)

# Map Preference data into the PCA coordinate system
comp1 = as.matrix(d1[,-1])%*%(pca$x[,1])
comp2 = as.matrix(d1[,-1])%*%(pca$x[,2])
comp3 = as.matrix(d1[,-1])%*%(pca$x[,3])

# create an aribitrary scale to shorten the length of preference vectors
# we call this number ss and divide all mapped preferences by it.
ss = max(comp1,comp2,comp3)
c1=comp1/ss
c2=comp2/ss
c3=comp3/ss
map1 = data.frame(c1, c2)
map2 = data.frame(c1, c3)
map3 = data.frame(c2, c3)


# Create the perceptual map(s)

# We now create the perceptual maps two dimensions at a time
# Visualize the first two dimensions of the perceptual map
biplot1 = factoextra::fviz_pca_biplot(pca, axes = c(1, 2),
                repel = TRUE,
                col.var = "darkblue",
                col.ind = "deeppink3",
                title = "Perceptual Map of Dimensions 1 and 2")
# # Plot Outputs commented out for the function
# plot(biplot1)

# If the 3rd component explains a large amount of variance
# we keep 3 components in the analysis and will have to check
# three maps instead of one

biplot2 = factoextra::fviz_pca_biplot(pca, axes = c(1, 3),
                repel = TRUE,
                col.var = "darkblue",
                col.ind = "deeppink3",
                title = "Perceptual Map of Dimensions 1 and 3")
# # Plot Outputs commented out for the function
# plot(biplot2)

biplot3 = factoextra::fviz_pca_biplot(pca, axes = c(2, 3),
                repel = TRUE,
                col.var = "darkblue",
                col.ind = "deeppink3",
                title = "Perceptual Map of Dimensions 2 and 3")
# # Plot Outputs commented out for the function
# plot(biplot3)

# # Create a theme for formatting our tables
# # This block of code is merely for beautifying our tables
# if (!require(gridExtra)) install.packages("gridExtra")
# library(gridExtra)
mytablecolors = c("#ccddee","#fff7dc")
mytheme = gridExtra::ttheme_minimal(
  core=list(bg_params = list(fill =  mytablecolors, col=NA),
            fg_params=list(fontface=3L)),
  colhead=list(fg_params=list(col="blue", fontface=4L)),
  rowhead=list(fg_params=list(col="black", fontface=3L)))

# Find the contribution of perceptual variables to each PCA Dimension
var = factoextra::get_pca_var(pca)
con = data.frame(round(var$contrib,2)[,1:3])
con2 = con
con2[nrow(con)+1,] = apply(con,2,sum)
row.names(con2)[nrow(con2)] = "Total"

# # Plot Outputs commented out for the function
# gridExtra::grid.arrange(top="Contribution of variables to Dimensions",gridExtra::tableGrob(con2, theme=mytheme))


# Contribution of variables to Dimension 1
con = con[order(con$Dim.1, decreasing = TRUE),]
slices = con[,1]
lbls = row.names(con)
pct = round(slices/sum(slices)*100)

# # Plot Outputs commented out for the function
# p1 = graphics::barplot(height = slices,
#               ylim=c(0,max(ceiling(slices))+2),
#               names.arg=lbls,
#               col=grDevices::rainbow(length(pct), alpha = 0.5),
#               main="Contribution of variables to\n Dimension 1",
#               ylab="Contributions (%)",
#               axisnames=FALSE)
# graphics::text(x=p1, y = slices+0.5 , labels=paste(pct,"%", sep=""))
# graphics::text(x=p1, graphics::par("usr")[3], labels = lbls, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)


# Contribution of variables to Dimension 2
con = con[order(con$Dim.2, decreasing = TRUE),]
slices = con[,2]
lbls = row.names(con)
pct = round(slices/sum(slices)*100)

# # Plot Outputs commented out for the function
# p2 = graphics::barplot(height = slices,
#               ylim=c(0,max(ceiling(slices))+2),
#               names.arg=lbls,
#               col=grDevices::rainbow(length(pct), alpha = 0.5),
#               main="Contribution of variables to\n Dimension 2",
#               ylab="Contributions (%)",
#               axisnames=FALSE)
# graphics::text(x=p2, y = slices+0.5 , labels=paste(pct,"%", sep=""))
# graphics::text(x=p2, graphics::par("usr")[3], labels = lbls, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

# Contribution of variables to Dimension 3
con = con[order(con$Dim.3, decreasing = TRUE),]
slices = con[,3]
lbls = row.names(con)
pct = round(slices/sum(slices)*100)

# # Plot Outputs commented out for the function
# p3 = graphics::barplot(height = slices,
#               ylim=c(0,max(ceiling(slices))+2),
#               names.arg=lbls,
#               col=grDevices::rainbow(length(pct), alpha = 0.5),
#               main="Contribution of variables to\n Dimension 3",
#               ylab="Contributions (%)",
#               axisname=FALSE)
# graphics::text(x=p3, y = slices+0.5 , labels=paste(pct,"%", sep=""))
# graphics::text(x=p3, graphics::par("usr")[3], labels = lbls, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)



# Adding the preferences data to the maps

####################################################
p1 = factoextra::fviz_pca_biplot(pca, axes = c(1, 2),
                      repel = TRUE,
                      col.var = "darkblue",
                      col.ind = "deeppink3",
                      title = "Perceptual Map of Dimensions 1 and 2 with Preferences")

p1_full = p1 + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = c1, yend = c2, color = "pink"), data = map1)

# # Plot Outputs commented out for the function
# plot(p1_full)

p2 = factoextra::fviz_pca_biplot(pca, axes = c(1, 3),
                      repel = TRUE,
                      col.var = "darkblue",
                      col.ind = "deeppink3",
                      title = "Perceptual Map of Dimensions 1 and 3 with Preferences")
p2_full = p2 + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = c1, yend = c3, colour = "blue"), data = map2)

# # Plot Outputs commented out for the function
# plot(p2_full)

p3 = factoextra::fviz_pca_biplot(pca, axes = c(2, 3),
                      repel = TRUE,
                      col.var = "darkblue",
                      col.ind = "deeppink3",
                      title = "Perceptual Map of Dimensions 2 and 3 with Preferences")
p3_full = p3 + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = c2, yend = c3, colour = "blue"), data = map3)

# # Plot Outputs commented out for the function
# plot(p3_full)



# Only plot the super-attributes
####################################################
biplot1s = factoextra::fviz_pca_biplot(pca, axes = c(1, 2),
                repel = TRUE,
                invisible = "var",
                col.var = "darkblue",
                col.ind = "deeppink3",
                title = "Perceptual Map of Dimensions 1 and 2")
# # Plot Outputs commented out for the function
# plot(biplot1s)

biplot2s = factoextra::fviz_pca_biplot(pca, axes = c(1, 3),
                repel = TRUE,
                invisible = "var",
                col.var = "darkblue",
                col.ind = "deeppink3",
                title = "Perceptual Map of Dimensions 1 and 3")

# # Plot Outputs commented out for the function
# plot(biplot2s)

biplot3s = factoextra::fviz_pca_biplot(pca, axes = c(2, 3),
                repel = TRUE,
                invisible = "var",
                col.var = "darkblue",
                col.ind = "deeppink3",
                title = "Perceptual Map of Dimensions 2 and 3")

# # Plot Outputs commented out for the function
# plot(biplot3s)

# Table of brand coordinates
dimensions_data = round(pca$x[,1:3],2)
colnames(dimensions_data) = c("Dim.1","Dim.2","Dim.3")

# # Plot Outputs commented out for the function
# gridExtra::grid.arrange(top="Brand Coordinates",gridExtra::tableGrob(dimensions_data, theme=mytheme))

# The above table  shows the coordinates of each brand on the plots.
# It is useful when you cannot visually compare the location of the brands.
# For example, FastCafe is locate at PC2 = -1.06, and PC3=1.23 on the
# Perceptual Map of Dimensions 2 and 3



# if (as.character(Sys.info()['sysname'])=="Windows"){
#   if (!require(pca3d)) install.packages("pca3d")

# Interact with the 3-D plot of the dimensions

###################################################################
# # The 3D map below works on PCs with Windows OS.
# # To get it to work on a Mac, you need to install
# # XQuartz from https://www.xquartz.org/ (Optional for Mac users)
# # See the XQuartz installation page on Canvas.
###################################################################
 # if (!require(pca3d)) install.packages("pca3d")
# 
# if (map3D==1){
#   
# suppressWarnings(res3d <- try(pca3d::pca3d(pca,
#                                            biplot = TRUE,
#                                            show.labels = TRUE,
#                                            axes.color = "black",
#                                            show.plane = TRUE,
#                                            labels.col = "blue",
#                                            bg = "white",
#                                            radius = 1.5), silent = TRUE))
# 
#     cat("\n")
#     cat("------------------------------------------\n")
#     cat("You have set your map3D=1 to generate a 3D perceptual map.\n")
#     cat("The map will be in a separate interactive window.\n")
#     cat("You can resize the 3D map window and click on the map and drag to rotate it.\n")
#     cat("If you don't see the map, the map window might be hiding behind your other open windows.\n")
#     cat("\n")
#     cat("If you are on a Mac, you need to install XQuratz for the 3D map to work.\n")
#     cat("Mac users can get and install XQuartz from:  https://www.xquartz.org/ \n")
#     cat("Alternatively, you can set map3D=0 to skip 3D map generation. \n")
#     cat("Remember to close the 3D map window when you are done with it.\n ")
#     cat("------------------------------------------")
#     cat("\n\n")
#     
# }


  # library(pca3d)
# pca3d::pca3d(pca,
#       biplot = TRUE,
#       show.labels = TRUE,
#       axes.color = "black",
#       show.plane = TRUE,
#       labels.col = "blue",
#       bg = "white",
#       radius = 1.5
# )

###################################################################


################# save results in a pdf file ############

# Decide what to call your file name; make sure to put .pdf at the end of the name
filename =  "! Results_Positioning_Analysis.pdf"


# pdf(filename, paper="USr", height=7, width=7)
suppressWarnings(res3 <- try(grDevices::pdf(filename, paper="USr", height=larger*8.5, width=larger*11), silent = TRUE))


bp = graphics::barplot(height = AvP$Avg,
              ylim=c(0,max(ceiling(AvP$Avg))+2),
              names.arg=AvP$Brands,
              col=grDevices::topo.colors(length(AvP$Brands), alpha = 1),
              main="Average Preferences for Brands",
              ylab="Average Preference Rating",
              axisnames = FALSE)
graphics::text(x=bp, y = AvP$Avg+0.2 , labels=as.character(AvP$Avg))
graphics::text(bp, graphics::par("usr")[3], labels = AvP$Brands, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

var_explained = factoextra::fviz_eig(pca, main = "PCA Percentage of Explained Variances", addlabels = TRUE, barfill = "deepskyblue")
plot(var_explained)

biplot1 = factoextra::fviz_pca_biplot(pca, axes = c(1, 2),
                          repel = TRUE,
                          col.var = "darkblue",
                          col.ind = "deeppink3",
                          title = "Perceptual Map of Dimensions 1 and 2")
plot(biplot1)

# If the 3rd component explains a large amount of variance
# we keep 3 components in the analysis and will have to check
# three maps instead of one

biplot2 = factoextra::fviz_pca_biplot(pca, axes = c(1, 3),
                          repel = TRUE,
                          col.var = "darkblue",
                          col.ind = "deeppink3",
                          title = "Perceptual Map of Dimensions 1 and 3")
plot(biplot2)

biplot3 = factoextra::fviz_pca_biplot(pca, axes = c(2, 3),
                          repel = TRUE,
                          col.var = "darkblue",
                          col.ind = "deeppink3",
                          title = "Perceptual Map of Dimensions 2 and 3")
plot(biplot3)

gridExtra::grid.arrange(top="Contribution of variables to Dimensions",gridExtra::tableGrob(con2, theme=mytheme))


# Contribution of variables to Dimension 1
con = con[order(con$Dim.1, decreasing = TRUE),]
slices = con[,1]
lbls = row.names(con)
pct = round(slices/sum(slices)*100)
p1 = graphics::barplot(height = slices,
              ylim=c(0,max(ceiling(slices))+2),
              names.arg=lbls,
              col=grDevices::rainbow(length(pct), alpha = 0.5),
              main="Contribution of variables to\n Dimension 1",
              ylab="Contributions (%)",
              axisnames=FALSE)
graphics::text(x=p1, y = slices+0.5 , labels=paste(pct,"%", sep=""))
graphics::text(x=p1, graphics::par("usr")[3], labels = lbls, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)


# Contribution of variables to Dimension 2
con = con[order(con$Dim.2, decreasing = TRUE),]
slices = con[,2]
lbls = row.names(con)
pct = round(slices/sum(slices)*100)
p2 = graphics::barplot(height = slices,
              ylim=c(0,max(ceiling(slices))+2),
              names.arg=lbls,
              col=grDevices::rainbow(length(pct), alpha = 0.5),
              main="Contribution of variables to\n Dimension 2",
              ylab="Contributions (%)",
              axisnames=FALSE)
graphics::text(x=p2, y = slices+0.5 , labels=paste(pct,"%", sep=""))
graphics::text(x=p2, graphics::par("usr")[3], labels = lbls, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)

# Contribution of variables to Dimension 3
con = con[order(con$Dim.3, decreasing = TRUE),]
slices = con[,3]
lbls = row.names(con)
pct = round(slices/sum(slices)*100)
p3 = graphics::barplot(height = slices,
              ylim=c(0,max(ceiling(slices))+2),
              names.arg=lbls,
              col=grDevices::rainbow(length(pct), alpha = 0.5),
              main="Contribution of variables to\n Dimension 3",
              ylab="Contributions (%)",
              axisname=FALSE)
graphics::text(x=p3, y = slices+0.5 , labels=paste(pct,"%", sep=""))
graphics::text(x=p3, graphics::par("usr")[3], labels = lbls, srt = 25, adj = c(1.1,1.1), xpd = TRUE, cex=.9)



# Adding the preferences data to the maps

####################################################
p1 = factoextra::fviz_pca_biplot(pca, axes = c(1, 2),
                     repel = TRUE,
                     col.var = "darkblue",
                     col.ind = "deeppink3",
                     title = "Perceptual Map of Dimensions 1 and 2 with Preferences")

p1_full = p1 + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = c1, yend = c2, color = "pink"), data = map1)
plot(p1_full)

p2 = factoextra::fviz_pca_biplot(pca, axes = c(1, 3),
                     repel = TRUE,
                     col.var = "darkblue",
                     col.ind = "deeppink3",
                     title = "Perceptual Map of Dimensions 1 and 3 with Preferences")
p2_full = p2 + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = c1, yend = c3, colour = "blue"), data = map2)
plot(p2_full)

p3 = factoextra::fviz_pca_biplot(pca, axes = c(2, 3),
                     repel = TRUE,
                     col.var = "darkblue",
                     col.ind = "deeppink3",
                     title = "Perceptual Map of Dimensions 2 and 3 with Preferences")
p3_full = p3 + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = c2, yend = c3, colour = "blue"), data = map3)
plot(p3_full)




# Only plot the super-attributes
####################################################
biplot1s = factoextra::fviz_pca_biplot(pca, axes = c(1, 2),
                           repel = TRUE,
                           invisible = "var",
                           col.var = "darkblue",
                           col.ind = "deeppink3",
                           title = "Perceptual Map of Dimensions 1 and 2")
plot(biplot1s)

biplot2s = factoextra::fviz_pca_biplot(pca, axes = c(1, 3),
                           repel = TRUE,
                           invisible = "var",
                           col.var = "darkblue",
                           col.ind = "deeppink3",
                           title = "Perceptual Map of Dimensions 1 and 3")
plot(biplot2s)

biplot3s = factoextra::fviz_pca_biplot(pca, axes = c(2, 3),
                           repel = TRUE,
                           invisible = "var",
                           col.var = "darkblue",
                           col.ind = "deeppink3",
                           title = "Perceptual Map of Dimensions 2 and 3")
plot(biplot3s)

# Table of brand coordinates
gridExtra::grid.arrange(top="Coordinates of Brands in each Dimension",gridExtra::tableGrob(dimensions_data, theme=mytheme))


grDevices::dev.off()

if (!is.null(res3)){
  cat("\n ERROR:\n The analysis was performed, but we were not able to\n save the results in \"! Results_Positioning_Analysis.pdf\"")
  cat(" \n This is probably due to a PDF file with the same name being open.\n")
  cat(" Make sure you close that file, and then run the previous line of code again.")
  
} else {
  cat("\n Positioning analysis has been performed on these data!\n")
  cat("\n Results have been saved in a file named: \"! Results_Positioning_Analysis.pdf\"\n")
  cat(" You can find this file in the same folder as your data files, which is here:\n ")
  cat(as.character(getwd()))
  cat(" \n\n ")
  cat(" If you see any warnings below, simply disregard them.\n\n ")
  
}


}