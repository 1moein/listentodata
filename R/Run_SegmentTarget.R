###########################################################
#          Segmentation and Targeting Analyses            #
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

#' This function runs a segmentation and targeting analysis and produces results
#' in a pdf file.
#'
#' @param df_seg segmentation data csv file
#' @param df_targ targeting data csv file
#' @param HowManySegments selected number of segments
#' @param paperresize How much larger should the pdf paper size be to fit everything? 
#'
#' @return a pdf file
#' @export
#' 
#' @examples
#' x=1:10 #just to get rid of the warning
Run_SegmentTarget <- function(df_seg,df_targ,HowManySegments, paperresize = 1) {
#
# # Remove all variables from memory to start fresh
# rm(list=ls())
# # If there are plots, delete them to start fresh
# if (length(dev.list())!= 0) dev.off()


####################################################
#######      Setting the Working Directory   #######
# The working directory is the folder in which
# we will place our R code and data files.

# Here, I automatically set the working directory
# to the folder that contains *THIS* R Script


# # Notice that we first make sure the rstudioapi package is installed.
# if (!require(rstudioapi)) install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# If the above code doesn't work, uncomment the setwd() line below
# and type the path of the data file in the quotation marks:
# setwd("D:/Teaching/RCodes/Topic 01 - Segmentation and Targeting Analysis")

# # Load a set of functions that the instructor has written
# # for this course if they do not already exist in memory
# if(!exists("MarketingAnalytics", mode="function"))
#   source("../z_Extras/MarketingAnalytics_functions.R")


##############  Loading data files #################
# In this example, we are using the data from a
# portable gps segmentation and targeting study.
# Load segmentation and targeting data sets
# Here we load data for a portable gps device segmentation study:
# df_seg = read.csv("gps_segmentation.csv", header = TRUE)
# df_targ = read.csv("gps_targeting.csv", header = TRUE)

####################################################

# # View the segmentation data set and its summary statistics
# utils::head(df_seg)
# str(df_seg)
# summary(df_seg)
#
# # View the targeting data set and its summary statistics
# utils::head(df_targ)
# str(df_targ)
# summary(df_targ)

# copy the segmentation data frame into df
df_orig = df_seg
# Standardize the segmentation data in the df data set
# Note that using scale() on a data frame converts it into a matrix.
# We have to convert it back to a data frame, because the code
# has been written to work with a data frame.
# If there's no need to standardize the data,
# the next two lines should be commentd out.
df = scale(df_seg)
df = data.frame(df)
larger = paperresize

# Let's see what these data look like now:
summary(df)

#----- Do a hierarchical clustering analysis-------------------------
# Create the distance matrix (a.k.a.dissimilarity matrix) to be used as input for clustering analysis
d = stats::dist(df, method = "euclidean")
# Hierarchical clustering using Ward's method
# Ward's method is most appropriate for numeric variables, but not for binary variables.
hc = stats::hclust(d, method = "ward.D" )

# Plot the dendogram
# The \n in the x and y labels below, writes what comes next, on a new line.
plot(hc, cex = 0.6, hang = -1, labels = FALSE,
     xlab= "Each vertical line is a segment and\n segments are being merged into fewer segments\n as we move up the dendogram.",
     ylab="Measure of Within-Cluster Sum of Squared Errors (SSE)"
)

# Plot the scree plot for 1- to 20-cluster solutions using the heights of the dendogram
# First, we get the dendogram heights from the hc object and reverse its order and call it x. Then we plot it.
x = rev(hc$height)
plot(x[1:20], type="b", col="navy",
     main="Scree Plot for Hierarchical Clustering",
      ylab="Measure of Within-Cluster Sum of Squared Errors (SSE)",
     xlab="Number of clusters")

#--------Set the number of clusters by setting k, and determine cluster membership for each Observation-----
# Set the value of HowManySegments equal to the number of clusters you have decided after examining the scree plot
# HowManySegments = 2

######################################################
# Plot the dendogram and show the clusters
plot(hc, cex = 0.6, hang = -1, labels = FALSE,
     xlab= "Each vertical line is a segment and\n segments are being merged into fewer segments\n as we move up the dendogram.",
     ylab="Measure of Within-Cluster Sum of Squared Errors (SSE)"
)
stats::rect.hclust(hc, k = HowManySegments, border = grDevices::rainbow(HowManySegments))

# Create a variable to determine which cluster each observation belongs to
Assigned_Segment = stats::cutree(hc, k = HowManySegments)

#-------- Add segment data to the original segmentation and targeting datasets--------
df_seg$segment = Assigned_Segment
df_targ$segment = Assigned_Segment

# # Visualize cluster plots by drawing the first two principal components
# if (!require(factoextra)) install.packages("factoextra")

segmentmap = factoextra::fviz_cluster(list(data = df, cluster = Assigned_Segment), ellipse.type = "norm")

plot(segmentmap)

#########################################################################
# ##testing something from this source
# # https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/
# res.km <- eclust(df, "kmeans", nstart = 25)
# # Enhanced hierarchical clustering
# res.hc <- eclust(df, "hclust") # compute hclust
# fviz_dend(res.hc, rect = TRUE) # dendrogam
# fviz_cluster(res.hc) # scatter plot
# set.seed(123)
# # Data preparation
# # +++++++++++++++
# data("iris")
# utils::head(iris)
# # Remove species column (5) and scale the data
# iris.scaled <- scale(iris[, -5])
# 
# # K-means clustering
# # +++++++++++++++++++++
# km.res <- kmeans(df, 3, nstart = 10)
# 
# # Visualize kmeans clustering
# # use repel = TRUE to avoid overplotting
# fviz_cluster(km.res, df, ellipse.type = "norm")
#####################################################






#---------- Find Segment Sizes---------------
x = table(Assigned_Segment)
y = 100*prop.table(table(Assigned_Segment))
segment_sizes = data.frame(rbind(x,y))

# Instead of the above three lines, I could have simply written this single line. It does the exact same job.
segment_sizes = data.frame(rbind(table(Assigned_Segment),100*prop.table(table(Assigned_Segment))))

# Make the table more informative
names(segment_sizes) = paste("Segment",1:ncol(segment_sizes), sep="")
row.names(segment_sizes) = c("No. of customers", "Percentage of customers")
segment_sizes = round(segment_sizes,2)


#---- Most important ouput and the actual results from these analyses--------------

# How large are the segments we have found?
segment_sizes


# # Create a theme for formatting our tables
# if (!require(gridExtra)) install.packages("gridExtra")
# library(gridExtra)
mytablecolors = c("#e5f5e0","#fff7dc")
mytheme = gridExtra::ttheme_minimal(
  core=list(bg_params = list(fill =  mytablecolors, col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="black", fontface=3L)))
# (Optional) In the above theme, we can use fill = hsv(0.61,seq(0.1,1,length.out = 25), 0.99,0.6) to paint the table with shades of blue

# Show the segment sizes table with the new formatting
gridExtra::grid.arrange(gridExtra::tableGrob(segment_sizes, theme=mytheme))


#----- Calculate the means of segmentation variables for each segment--------
# How are the segments different in their needs/preferences/wants/desires?
# Inside two other functions, I'm using a function from the MarketingAnalytics_Functions.R file, called
# calculate_segment_means(). I am feeding my segmentation dataset to it as input.
# This is the original segmentation data set that now contains a new variable
# called segment at the end that shows segment membership for each individual.

gridExtra::grid.arrange(top="Segmentation Analysis: Main Results", gridExtra::tableGrob(calculate_segment_means(df_seg)[[2]], theme=mytheme))


# # Visualizing the means of segmentation variable for each segment
# 
# if (!require(flexclust)) install.packages("flexclust")
# library(flexclust)
seg_Results = lattice::barchart(hc, df, k = HowManySegments,
         shade = TRUE,
         main = "Plot of Segmentation Variable Means \n Dots show population* means standardized at 0. \n Bars show segment means for each variable. \n Differences between population and segment means can help us describe each segment \n relative to each other and the population. \n *Population refers to: All respondents in our dataset",
         xlab = paste("Segment ", as.character(rep(1:HowManySegments)))
)
plot(seg_Results)

#--------- Targeting Analysis --------------
# For targeting analysis, we only calculate the mean of the targeting variables
# for each segment and determine if they are different from the population on those variables.

# How are the segments different in how we can reach/target them?

# if (length(dev.list())!= 0) dev.off()

################# save results in a pdf file ############

# Decide what to call your file name; make sure to put .pdf at the end of the name
filename  =  "! Results_Segment_Target.pdf"

# # next line to be used in later versions to check if the file is open
# fileisopen = suppressWarnings("try-error" %in% class(try(file(filename, open = "w"), silent = TRUE)))


# larger has become a function input in the listentodata package.
# If the tables are nod displayed properly in the pdf pages, change the
# value for "larger" below to 1.5, 1.7, 1.8, 2, or larger values for larger
# page sizes
# larger =  1
# library(grDevices)
suppressWarnings(res2 <- try(grDevices::pdf("! Results_Segment_Target.pdf", height=larger*8.5, width=larger*11), silent = TRUE))
# suppressWarnings(res <- try(write.csv(df, file ="RFM_Analysis_Results.csv", row.names = FALSE), silent = TRUE))

utils::head(df_seg)
summary(df_seg)

utils::head(df_targ)
summary(df_targ)

plot(hc, cex = 0.6, hang = -1, labels = FALSE,
     xlab= "Each vertical line is a segment and\n segments are being merged into fewer segments\n as we move up the dendogram.",
     ylab="Measure of Within-Cluster Sum of Squared Errors (SSE)"
)
stats::rect.hclust(hc, k = HowManySegments, border = grDevices::rainbow(HowManySegments))

x = rev(hc$height)
plot(x[1:20], type="b", col="navy",
     main="Scree Plot for Hierarchical clustering",
     ylab="Measure of Within-Cluster Sum of Squared Errors (SSE)",
     xlab="Number of clusters")

segmentmap = factoextra::fviz_cluster(list(data = df, cluster = Assigned_Segment), ellipse.type = "norm")
plot(segmentmap)

gridExtra::grid.arrange(gridExtra::tableGrob(segment_sizes, theme=mytheme))

seg_Results = lattice::barchart(hc, df, k = HowManySegments,
                       shade = TRUE,
                       main = "Bar Chart of Standardized Segmentation Variable Means \n Dots show population means \n Bars show segment means \n Difference of means between a segment and the population helps us describe each segment",
                       xlab = paste("Segment ", as.character(rep(1:HowManySegments)))
)
plot(seg_Results)

gridExtra::grid.arrange(top="Segmentation Analysis: Main Results", gridExtra::tableGrob(calculate_segment_means(df_seg)[[2]], theme=mytheme))

gridExtra::grid.arrange(top="Targeting Analysis: Main Results", gridExtra::tableGrob(calculate_segment_means(df_targ)[[2]], theme=mytheme))


suppressWarnings(grDevices::dev.off())


if (!is.null(res2)){
  cat("\n ERROR:\n The analysis was performed, but we were not able to\n save the results in \"! Results_Segment_Target.pdf\"")
  cat(" \n This is probably due to a PDF file with the same name being open.\n")
  cat(" Make sure you close that file, and then run the previous line of code again.")
  
} else {
  cat("\n Segmentation and Targeting analysis has been performed on these data!\n")
  cat("\n Results have been saved in a file named: \"! Results_Segment_Target.pdf\"\n")
  cat(" You can find this file in the same folder as your data files, which is here:\n ")
  cat(as.character(getwd()))
  cat(" \n\n ")
  cat(" If you see any warnings below, simply disregard them.\n\n ")
  
}


}
