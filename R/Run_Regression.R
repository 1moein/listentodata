###########################################################
#      Regression and Logistic Regression Analysis        #
###########################################################
#    R Codes for Marketing Analytics                      #
#    Author: Moein Khanlari, PhD                          #
#    Version 1.0                                          #
#    Copyright© 2019, Moein Khanlari All rights reserved. #
#    This software is provided to students at the         #
#    University of New Hampshire on an "AS IS" BASIS,     #
#    WITH ABSOLUTELY NO WARRANTIES either expressed or    #
#    implied. The software may not be redistributed       #
#    in whole or part without the express written         #
#    permission of the author.                            #
###########################################################

# 
# ##############   Attention    ##############
# # In this code, you will find the regression
# # tables in the Console only, but the plots
# # will be be generated in a PDF file and
# # in the Plots pane.
# ############################################
# 
# firstrun = function ()
# {
#   remove.packages("rlang")
#   install.packages("rlang")
# }
# 
# ####### First Run #############
# # If this is the first time
# # that you run this code
# # on THIS computer/laptop,
# # type firstrun()
# # in the Console below
# # after the > sign
# # and press Enter/Return
# # Select YES If asked:
# # Do you want to Restart R...?
# ###############################


# # Have you read the "First Run" Section above?
# 
# if (!require(broom)) install.packages("broom")
# library(broom)

# # Remove all variables from memory to start fresh
# rm(list=ls())
# # If there are plots, delete them to start fresh
# if (length(dev.list())!= 0) dev.off()



# ####################################################
# #######      Setting the Working Directory   #######
# # The working directory is the folder in which
# # we will place our R code and data files.
# 
# # Here, I automatically set the working directory
# # to the folder that contains THIS R Script
# if (!require(rstudioapi)) install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 
# # If the above code fails, uncomment the setwd() line below
# # and type the path of the data file in the quotation marks:
# # setwd("Folder_path_goes_here")
# 
# ####################################################


# Additional Preparation

# # Install and/or load some needed libraries
# if (!require(jtools)) install.packages("jtools") # Ignore the RTools required message
# if (!require(ggstance)) install.packages("ggstance")
# if (!require(corrplot)) install.packages("corrplot")
# if (!require(kableExtra)) install.packages("kableExtra") #to format the table in Rmd
# if (!require(broom.mixed)) install.packages("broom.mixed")
# if (!require(sjPlot)) install.packages("sjPlot")
# library(corrplot)
# library(jtools)
# library(ggstance)
# library(kableExtra)
# library(sjPlot)

#' Title
#'
#' @param mydata csv data file
#' @param myformula formula for the lm model
#'
#' @examples
#' x=1:10 #just to get rid of the warning
Run_Regression = function(mydata,myformula) {

# mydata = getdata()  
d1 = mydata

datasummary = summary(d1)

options(scipen=999) # to prevent the use of scientific notation in results
  
# Regression Analysis

# This is a data set of car seat sales in different stores.
# d1 = read.csv(file="carseats.csv", header = TRUE)
# head(d1)
# A scatterplot matrix of numeric variables
nums <- unlist(lapply(d1, is.numeric))
numcols <- d1[ , nums]


graphics::pairs(numcols, lower.panel = NULL, pch=16,cex=0.3)

#Visualize the correlations among numeric variables as numbers or pie charts
correlations = stats::cor(numcols)
corrplot::corrplot(correlations, method="pie")
corrplot::corrplot(correlations, method="number")

# Estimate regression models
# # Start with simple regression in model m1 and move on to
# # multiple regression analyses in models m2 to m5
# 
# m1 = lm(Sales ~ CompPrice, data=d1)
# m2 = lm(Sales ~ CompPrice + Income, data=d1)
# m3 = lm(Sales ~ CompPrice + Income + Advertising, data=d1)
# m4 = lm(Sales ~ CompPrice + Income + Advertising + Population, data=d1)
# m5 = lm(Sales ~ CompPrice + Income + Advertising + Population + Price, data=d1)
  m6 = stats::lm(myformula, data=d1)

# Display the results

# summary of results from the full model
jtools::summ(m6)

# # Comparative table of all models
# jtools::export_summs(m1, m2, m3, m4, m5, m6)
# ## If it is difficult to see the above table
# ## uncoment the next line of code and run it
# ## to create this table in the Plots area,
# ## and then click the window-arrow icon
# ## located on the right side of the broom
# ## in the the Plots tab
# # tab_model(m1, m2, m3, m4, m5, m6)

# Visual summary
suppressWarnings(jtools::plot_summs(m6, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95))


# Find the best model using stepwise regression.
# What variables should remain and what variables should be deleted
# from the equation?\

# selected_model = step(m6)


# # Display the best model
# jtools::summ(selected_model)

# # Display the best model visually
# jtools::plot_summs(selected_model, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)
# 
# # Model Diagnostics for the Best model
# par(mfrow=c(2,2))
# plot(selected_model, main  = "Diagnostics for the Best Model")
# par(mfrow=c(1,1))

# # Relative impact of variables
# # Standardize all numeric variables except the Dependent Variable
# d2 = d1
# d2[, nums] = scale(d2[, nums])
# 
# m6_s = lm(myformula, data=d2)
# selected_model_s = step(m6_s)
# 
# jtools::summ(selected_model_s)


# # Individual effect plots
# jtools::effect_plot(m6, pred = CompPrice, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Income, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Advertising, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Population, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Price, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Age, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Education, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = Urban, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = US, interval = TRUE, plot.points = TRUE)
# jtools::effect_plot(m6, pred = ShelfLoc, interval = TRUE, plot.points = TRUE)


# # Prediction with multiple regression
# newdata = read.csv(file="carseats_salesprediction.csv", header = TRUE)
# predictedSales = predict(selected_model, newdata)
# predictions = cbind(newdata, predictedSales)
# newdata
# predictions


################# save results in a pdf file ############

# Set up some parameters
# Decide what to call your file name; make sure to put .pdf at the end of the name
filename =  "! Results_Regression_Analysis.pdf"
larger =  1
grDevices::pdf(filename, height=larger*8.5, width=larger*11)

# A scatterplot matrix of numeric variables
datasummary
graphics::pairs(numcols, lower.panel = NULL, pch=16,cex=0.3)

correlations = stats::cor(numcols)
corrplot::corrplot(correlations, method="pie")
corrplot::corrplot(correlations, method="number")

jtools::summ(m6)
# jtools::export_summs(m1, m2, m3, m4, m5, m6)
jtools::plot_summs(m6, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)

# Display the best model
jtools::summ(selected_model)

# Display the best model visually
jtools::plot_summs(selected_model, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)

# Model Diagnostics for the Best model
selected_model = stats::step(m6)

graphics::par(mfrow=c(2,2))
graphics::plot(selected_model, main  = "Diagnostics for the Best Model")
graphics::par(mfrow=c(1,1))

# # Relative impact of variables
# jtools::summ(selected_model_s)

# # Individual effect plots
# effect_plot(m6, pred = CompPrice, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Income, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Advertising, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Population, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Price, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Age, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Education, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = Urban, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = US, interval = TRUE, plot.points = TRUE)
# effect_plot(m6, pred = ShelfLoc, interval = TRUE, plot.points = TRUE)

grDevices::dev.off()


}
