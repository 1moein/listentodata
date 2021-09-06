###########################################################
#                     Conjoint Analysis                   #
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


# # Load a set of functions that the instructor has written
# # for this course if they do not already exist in memory
# if(!exists("MarketingAnalytics", mode="function"))
#   source("../z_Extras/MarketingAnalytics_functions.R")

####################################################


################# Analyze ratings data and calculate partial utility utilities ##############
# This script assumes that you have
# designed a conjoint study based on a number of product attributes
# and levels, and that you have collected ratings data
# from a represenative sample of customers in your target market
# on the hypothetical product profiles from that design.
#
# For data collection, you will need to ask
# respondents to rate the product
# profiles from your conjoint study design on a scale
# from 0 to 100, with 100 representing a
# product that they like a lot.
# you will need to put all those ratings in
# an Excel .CSV file with the following structure:
#
# product1 Product2 ....  ProductN
# rating1  rating2  ....  ratingN
# rating1  rating2  ....  ratingN
# rating1  rating2  ....  ratingN
# rating1  rating2  ....  ratingN
# .
# .
# .
# Each row above would contain all the ratings of one respondent
# and the template shown above assumes N products are being rated by each respondent
# where N is the number of product profiles in your conjoint study design.
#
# As an example, for 16 products and 50 respondents, this scheme
# would lead to a .CSV file with 51 rows and 16 columns
# Note, there would be a header row of product names, hence 50+1 rows.


################# Estimate partial utilities ##############

# A technical note:
# This code requires all attribute levels to be unique, such that no two
# attribute levels from different attributes are identical.
# If some attributes have similar levels, you will need to add extra characters
# to some attribute levels
# to make sure all attribute levels are unique: For example, if you have
# two Attributes both of which have levels "Yes" and "No", add the attribute name
# to the "Yes" and "No" values, so that no two attribute levels are  identical
# For example a Bluetooth with Yes and No as its levels would be renamed to
# "bluetoothYes", "bluetoothNo".

#' Conjoint Analysis
#' 
#' This function conducts a conjoint analysis. It requires five csv data files that are listed below.
#' 
#' @param design the design table csv file
#' @param products the list of tested products as a csv file
#' @param ratings ratings of the products in the conjoint survey in a csv file
#' @param alternatives list of proposed product alternatives as a csv file
#' @param competitors list of competitors' products in the market as a csv file
#' @param resizepaper how much larger should the report paper be to fit everything?
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # This is the sample code to be copied and used in a new R Script:
#' library(listentodata)
#' clear_console()
#' select_folder()  #Select the folder where your data files are
#' design = read.csv("Tablet_levels_attributes.csv", na.strings=c("","NA")) #design table
#' products = read.csv("Tablet_survey_products.csv")    # READ PRODUCT PROFILES DATA
#' ratings =  read.csv("Tablet_survey_ratings.csv")    # READ RATINGS DATA
#' alternatives = read.csv("Tablet_New_Product_Alternatives.csv") # Load New product alternative
#' competitors = read.csv("Tablet_competitors.csv")  # Load competitors products
#' resizepaper = 1
#' Run_Conjoint_Analysis(design, products, ratings, alternatives, competitors, resizepaper)
#' }
Run_Conjoint_Analysis = function(design, products, ratings, alternatives, competitors, resizepaper=1) {
  
cat(".\n")
cat("..\n")
cat("...\n")
  
cat("1. Pre-processing data... ")
  
  
  DesignTable = design
  df_products = products
  df_ratings = ratings
  alternatives = alternatives
  competitors = competitors
  larger = resizepaper
  

# # Let's take a look at the design matrix
# (DesignTable = read.csv(file="Tablet_levels_attributes.csv",na.strings=c("","NA")))

# # Read the data from a .CSV Excel file and place it in a new variable:
# (df_products = read.csv(file="Tablet_survey_products.csv"))             # READ PRODUCT PROFILES DATA

# df_ratings =  read.csv(file="Tablet_survey_ratings.csv")             # READ RATINGS DATA

myratings = data.frame(t(df_ratings))
utils::head(myratings)

# # Load New product alternative
# (alternatives = read.csv(file="Tablet_New_Product_Alternatives.csv"))

# Create a copy of alternative with attribute names
alts_with_names = alternatives
row.names(alts_with_names) = names(DesignTable)[2:ncol(DesignTable)]
alts_with_names

# # Load competitors products
# (competitors = read.csv(file="Tablet_competitors.csv"))

# Create a copy of competitors with attribute names
comps_with_names = competitors
row.names(comps_with_names) = names(DesignTable)[2:ncol(DesignTable)]
comps_with_names

#A minor renaming to make product profile names consistent across different projects or data sets.
names(df_products) = paste0(rep("prod",ncol(df_products)),1:ncol(df_products))
myproducts = data.frame(t(df_products))

cat("Done!\n")

cat("2. Calculating Partial utilities...")

# Estimate Partial Utilities
pws = Estimate_Partworth_Utilities(myratings,myproducts)
# View the top rows of pws
utils::head(pws)
cat("Done!\n")

################# Conduct Market share Analysis using partial utilities ##############

# In this section, you can analyze market shares for
# existing and/or new products based on the knowledge
# obtained about customer preferences from their ratings data
# This knowledge is now summarized in respondents' partial utilities.
#
# To convert preferences into choices, we need to make an assumption
# about how people convert their preferences to choices.
# We can use one of three choice rules:

# Choice Rule 1: First preference or utility maximization rule
# Choice Rule 2: Share of Preference rule
# Choice Rule 3: Logit Choice rule

cat("3. Estimating market shares(MS) for alternatives (no comp.)...")

# Now we can estimate market shares for alternative products competing against one another
ourMarketShares0 = Estimate_Market_Shares(alternatives,pws)
row.names(ourMarketShares0) = names(alternatives)

# print and plot the results
ourMarketShares0

#Plot_MS_rule1() calculates market shares using First Choice Rule
#Plot_MS_rule2() calculates market shares using Preference Share Rule
#Plot_MS_rule3() calculates market shares using Logit Choice Rule
Plot_MS_rule1(ourMarketShares0, "Only for our alternatives in the absence of competition")
Plot_MS_rule2(ourMarketShares0, "Only for our alternatives in the absence of competition")
Plot_MS_rule3(ourMarketShares0, "Only for our alternatives in the absence of competition")

cat("Done!\n")

cat("4. Estimating MS for competitors (no alt.)...")

# Let's also estimate market shares for each alternative product in the face of existing competition

# Load competitor product profiles


# First estimate current market shares of competitors:

competitionset = competitors
# Now we can estimate market shares
competitorShares = Estimate_Market_Shares(competitionset,pws)
row.names(competitorShares) = names(competitionset)
# print and plot the results
competitorShares

Plot_MS_rule3(competitorShares, "Only for our competitors before we enter the market")
cat("Done!\n")
cat("5. Estimating MS for alternatives among competitors...")
# Now Add the new product alternatives to the mix of competitors one by one and estimate their market share
# in the presence of competitors.
#
# Specify which alternative product from alternatives you'd like to test against the competitors
# using the for loop. To simplify the Tutorial, I have commented out the parts that show
# the market shares using the first two choice rules and only display the results from the
# Logit choice rule.

for (j in 1:ncol(alternatives)){
  competitionset = cbind(competitors,alternatives[,j])
  names(competitionset)[dim(competitionset)[2]] = names(alternatives)[j]
  ourMarketShares = Estimate_Market_Shares(competitionset,pws)
  row.names(ourMarketShares) = names(competitionset)
  ourMarketShares
  # Plot_MS_rule1(ourMarketShares)
  # Plot_MS_rule2(ourMarketShares)
  Plot_MS_rule3(ourMarketShares, paste("when we launch Alternative", j ," \n against the current competitors"))
  # readline(prompt="Press [enter] to see the next Market Share Chart")
}
cat("Done!\n")

cat("6. Estimating MS for all potential products...")


################# Find the optimal product design from all possible designs and partial utility estimates ####
# First create a list of all attributes.

Attributes = list()
for (i in 1:(ncol(DesignTable)-1)){
  Attributes[[i]] = (stats::na.omit(DesignTable[,i+1]))
}

All_combinations = expand.grid(Attributes)
All_products = data.frame(t(All_combinations))
names(All_products) = paste0(rep("product",ncol(All_products)),1:ncol(All_products))

All_products[,1:5]

# Now let's plot the top 5 products with highest market shares
# the second argument in the function below is the rule type

MS = Estimate_Market_Shares(All_products,pws)
cat("Done!\n")

cat("7. Finding optimal product(OP) in the market of all products...")

utils::head(MS)
dim(MS)

# Sort market shares for each choice rule from highest to lowest
MS1 = MS[order(MS$MS_FirstChoice, decreasing = TRUE),]
MS2 = MS[order(MS$MS_PreferenceShare, decreasing = TRUE),]
MS3 = MS[order(MS$MS_Logit, decreasing = TRUE),]

# select the 100 products with the highest market shares for the logit rule
MS3_50 = MS3[1:50,]

# select the products with the top 5 market shares for each of the three choice rules
MS1 = MS1[1:5,]
MS2 = MS2[1:5,]
MS3 = MS3[1:5,]

# Plot market shares for best products and identify them

Plot_MS_rule3(MS3_50, paste("For the top 50 products in the design space \n when all",dim(All_products)[2],"products are launched."))

Plot_MS_rule1(MS1, paste("For the top 5 products in the design space \n when all",dim(All_products)[2],"products are launched."))
All_products[,which(colnames(All_products) %in% row.names(MS1))]

Plot_MS_rule2(MS2, paste("For the top 5 products in the design space \n when all",dim(All_products)[2],"products are launched."))
All_products[,which(colnames(All_products) %in% row.names(MS2))]

Plot_MS_rule3(MS3, paste("For the top 5 products in the design space \n when all",dim(All_products)[2],"products are launched."))
All_products[,which(colnames(All_products) %in% row.names(MS3))]


# Which product maximizes market share when using the First Choice rule?
which(MS$MS_FirstChoice==max(MS$MS_FirstChoice))
# What are its attribute levels or features?
data.frame(All_products[,which(MS$MS_FirstChoice==max(MS$MS_FirstChoice))])
# Which product maximizes market share when using the Preference Share rule?
which(MS$MS_PreferenceShare==max(MS$MS_PreferenceShare))
# What are its attribute levels or features?
All_products[,which(MS$MS_PreferenceShare==max(MS$MS_PreferenceShare))]
# Which product maximizes market share when using the Logit Choice rule?
which(MS$MS_Logit==max(MS$MS_Logit))
# What are its attribute levels or features?
All_products[,which(MS$MS_Logit==max(MS$MS_Logit))]

cat("Done!\n")

cat("8. Finding OP in the market of competitors...Be patient :) ...")

# Find the optimal product design based on existing competitors.

# It would also be a good idea to test each potential product
# against the existing competitors and see which
# one gains the highest market share.

# The result should be similar to previous findings but this is
# a direct check: We are comparing the market share of each of all our 2592
# products against the existing competitors to see which single
# potential would be the strongest against the current competitors.

# We will only use the Logit choice rule here:
old_Logit_Marketshare = 0
best_Logit_Marketshare = 0
keepthis = 0

# identify the best product and save its index number in keepthis
for (j in 1:dim(All_products)[2]){
  competitionset = cbind(competitors,All_products[,j])
  names(competitionset)[dim(competitionset)[2]] = names(All_products)[j]
  ourMarketShares = Estimate_Market_Shares(competitionset,pws)
  row.names(ourMarketShares) = names(competitionset)
  best_Logit_Marketshare = ourMarketShares[dim(competitionset)[2],2]
  if (best_Logit_Marketshare>old_Logit_Marketshare){
    keepthis = j
    old_Logit_Marketshare = best_Logit_Marketshare
  }
}
keepthis

# Locate the best product and compare its market share agains existing competitors
j = keepthis
All_products[,j]
competitionset = cbind(competitors,All_products[,j])
names(competitionset)[dim(competitionset)[2]] = names(All_products)[j]
bestproductMS = Estimate_Market_Shares(competitionset,pws)
row.names(bestproductMS) = names(competitionset)

cat("Done!\n")

cat("9. Saving results in a pdf file...")

# Plot the results: Which product is the single strongest potential product
# against the current competitors?

Plot_MS_rule1(bestproductMS, paste("For the STRONGEST potential product out of all",dim(All_products)[2],"products \n against the current competitors"))

Plot_MS_rule2(bestproductMS, paste("For the STRONGEST potential product out of all",dim(All_products)[2],"products \n against the current competitors"))

Plot_MS_rule3(bestproductMS, paste("For the STRONGEST potential product out of all",dim(All_products)[2],"products \n against the current competitors"))

################# save results in a pdf file ############

# Decide what to call your file name; make sure to put .pdf at the end of the name
filename =  "! Results_Conjoint_Analysis.pdf"

# If the tables are nod displayed properly in the pdf pages, change the
# value for "larger" below to 1.5, 1.7, 1.8, 2, or larger values for larger
# page sizes
# larger =  1

# pdf(filename, height=larger*8.5, width=larger*11)
suppressWarnings(res7 <- try(grDevices::pdf(filename, height=larger*8.5, width=larger*11), silent = TRUE))


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

# Tables
gridExtra::grid.arrange(gridExtra::tableGrob(alts_with_names, theme=mytheme))
gridExtra::grid.arrange(gridExtra::tableGrob(comps_with_names, theme=mytheme))

# Plots
Plot_MS_rule1(ourMarketShares0, "Only for our alternatives in the absence of competition")
Plot_MS_rule2(ourMarketShares0, "Only for our alternatives in the absence of competition")
Plot_MS_rule3(ourMarketShares0, "Only for our alternatives in the absence of competition")
Plot_MS_rule3(competitorShares, "Only for our competitors before we enter the market")


for (j in 1:ncol(alternatives)){
  competitionset = cbind(competitors,alternatives[,j])
  names(competitionset)[dim(competitionset)[2]] = names(alternatives)[j]
  ourMarketShares = Estimate_Market_Shares(competitionset,pws)
  row.names(ourMarketShares) = names(competitionset)
  ourMarketShares
  # Plot_MS_rule1(ourMarketShares)
  # Plot_MS_rule2(ourMarketShares)
  Plot_MS_rule3(ourMarketShares, paste("when we launch Alternative", j ," \n against the current competitors"))
  # readline(prompt="Press [enter] to see the next Market Share Chart")
}

# Plot top products
Plot_MS_rule3(MS3_50, paste("For the top 50 products in the design space \n when all",dim(All_products)[2],"products are launched."))

Plot_MS_rule1(MS1, paste("For the top 5 products in the design space \n when all",dim(All_products)[2],"products are launched."))

Plot_MS_rule2(MS2, paste("For the top 5 products in the design space \n when all",dim(All_products)[2],"products are launched."))

Plot_MS_rule3(MS3, paste("For the top 5 products in the design space \n when all",dim(All_products)[2],"products are launched."))





# Plot the results: Which product is the single strongest potential product
# against the current competitors?

Plot_MS_rule1(bestproductMS, paste("For the STRONGEST potential product out of all",dim(All_products)[2],"products \n against the current competitors"))

Plot_MS_rule2(bestproductMS, paste("For the STRONGEST potential product out of all",dim(All_products)[2],"products \n against the current competitors"))

Plot_MS_rule3(bestproductMS, paste("For the STRONGEST potential product out of all",dim(All_products)[2],"products \n against the current competitors"))


grDevices::dev.off()
if (!is.null(res7)) cat(" ***Failed!*** :\"-( \n")
if (is.null(res7)) cat("Done! :D \n")

cat(" ...............................................................\n\n")




if (!is.null(res7)){
  cat("\n ERROR:\n The analysis was performed, but we were not able to\n save the results in \"! Results_Conjoint_Analysis.pdf\"")
  cat(" \n This is probably due to a PDF file with the same name being open.\n")
  cat(" Make sure you close that file, and then run the previous line of code again.")
  
} else {
  cat("\n Conjoint Analysis was performed on these data!\n")
  cat("\n Results have been saved in a file named: \"! Results_Conjoint_Analysis.pdf\"\n")
  cat(" You can find the result file in this folder:\n ")
  cat(as.character(getwd()))
  cat(" \n\n ")
  cat(" If you see any warnings below, simply disregard them.\n\n ")
}





}