###########################################################
#          Design Conjoint Product Profiles in R          #
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
# This will be the folder in which we will
# place our R code and data files.

# # Here, I automatically set the working directory
# # to the folder that contains THIS R Script
# if (!require(rstudioapi)) install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# If the above code fails, uncomment the setwd() line below
# and type your data file path within the quotation marks:
# setwd("Folder_path_goes_here")

####################################################


########### Conjoint study product profiles design ############
# Instructions:
# In this section, you can design a traditional conjoint study by
# creating the product profiles that survey respondents will rate later.
# First, you will need to define product attributes and their levels.
# An attribute is a product feature, and levels are the different values you can have for each feature.
# You can change the values in the c() command below to specify the levels of each product attribute
# You can change the names of Attributes in mylist from NameOfAttribute1, NameOfAttribute2 to names of your choosing.
# When changing Attribute names only use alphanumeric characters for new names.
# You can also add more attributes to the list or delete some attributes.
# When adding or deleting attributes, note that there must be a comma at the end of code lines for
# each attribute EXCEPT for the last one.
# Some examples of attributes:
# Example 1: For a computer mouse, Price is a product attribute and it can have levels like $20, $50, $100
# Example 2: Connection_Type can be another attribute for a mouse with 2 levels: Wired, Wireless.

########## Attribute Definitions ##################
# Use the following template to define your attributes.
# Each line from NameOfAttribute1 to NameOfAttribute4 defines an attribute and its levels.
# you can add more attributes by adding a similar line:
# Mistakes to avoid: Each attribute definition line ends with a "comma", except for the last one.


# mylist = list(
#   NameOfAttribute1 = c("$100","$200","$300"),
#   NameOfAttribute2 = c("A","B","C","D"),
#   NameOfAttribute3 = c("Low","Medium","High"),
#   NameOfAttribute4 = c("Yes","No")
# )
########## End of Attribute Definitions ###########

#' Title
#'
#' @param designparameters list of attributes and values
#' 
#' @examples
#' x=1:10 #just to get rid of the warning
Run_Conjoint_Study_Design = function(designparameters) {
  
mylist = designparameters

# # Creating all candidate products that can be designed by combining the above attribute levels:
# # First, if the package AlgDesign is not installed, install it. You wood need access to the Internet to install this package.
# if (!require("AlgDesign")) install.packages("AlgDesign")
# library(AlgDesign)

# Create all potential products with the above attributes.
(All_product_candidates = expand.grid(mylist))


# It's not a good idea to ask people to rate all potential products you found above.
# Let's use an algorithm to select an optimal group of product candidates and have respondents rate them
any_number = 12345
set.seed(any_number)

Select_candidates = AlgDesign::optFederov( ~ ., data = All_product_candidates, criterion = "D")
profiles = Select_candidates$design
m = nrow(profiles)
row.names(profiles) = paste(rep("product",m),1:m)
# 
# # Run the next line to see the list of final profiles that will be used for conjoint data collection
# # You can save these profiles and use them in your questionnnaire design.
# profiles


# Great! We are done here; we have created the set of product profiles we need
# to use in our conjoint survey design. But what is next?

# NEXT STEPS:
# For data collection, you will need to ask
# at least 50 respondents to rate the product profiles
# created above on a scale of 0 to 100, with 100 representing a product that they like the most.
# You can show each product profile to your survey respondents and ask:
# "Please rate the following product from 0 (Not like at all) to 100 (Like a lot)."

# Set up some parameters
# Decide what to call your file name; make sure to put .pdf at the end of the name
filename =  "! Results_Conjoint_Study.csv"
# larger =  1.2
# pdf(filename, height=larger*8.5, width=larger*11)


suppressWarnings(res6 <- try(utils::write.csv(profiles, filename, row.names = FALSE), silent = TRUE))


if (!is.null(res6)){
  cat("\n ERROR:\n The analysis was performed, but we were not able to\n save the results in \"! Results_Conjoint_Study.csv\"")
  cat(" \n This is probably due to a CSV file with the same name being open.\n")
  cat(" Make sure you close that file, and then run the previous line of code again.")
  
} else {
  cat("\n Conjoint study was designed based on your design parameters!\n")
  cat("\n Results are displayed below and also saved in a file named: \"! Results_Conjoint_Study.csv\"\n")
  cat(" You can find the result file in this folder:\n ")
  cat(as.character(getwd()))
  cat(" \n\n ")
  
  cat("           Conjoint Study Design for a Conjoint Survey \n")
  
  print(profiles)
  cat(" \n ")
  
}


}




