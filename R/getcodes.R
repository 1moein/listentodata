





#' Get Analysis Hub Codes
#'
#' @export
#'
#' @examples
#' getcodes()
getcodes = function(){
  
clear_console()
  
  # Copy Everything below this line and above the
  # Other guide at the bottom.
  
  
  # ##########################################################
  # #                     Analysis Hub                       #
  # ##########################################################
  # #    R Codes for Marketing Analytics                     #
  # #    Author: Moein Khanlari, PhD                         #
  # #    Version 1.1                                         #
  # #    Copyright© 2021, Moein Khanlari All rights reserved.#
  # #    The listentodata R package and this accompanying    #
  # #    R Script are provided to the students at the        #
  # #    University of New Hampshire on an "AS IS" BASIS,    #
  # #    WITH ABSOLUTELY NO WARRANTIES either expressed or   #
  # #    implied. The package and this script may not be     #
  # #    redistributed in whole or part without the express  #
  # #    written permission of the author.                   #
  # ##########################################################
  # ###################   Attention    #######################
  # # This R script allows you to run all of the analyses    #
  # # taught in mktg763. The data required for each analysis #
  # # must be prepared as taught in this course for these    #
  # # scripts to function probably.                          # 
  # ##########################################################
  # # ---------------    Setup & Update ----------------------
  # # Run the next three lines each time you open this Script
  # if (!require(devtools)) install.packages("devtools", dependencies = TRUE)
  # devtools::install_github("1moein/listentodata", force=TRUE, dependencies=TRUE)
  # library(listentodata)
  # # ---------------- Course Setup Complete!---------------
  # 
  # 
  # #---------------------------------------------------
  # #--       Help on each analysis function          --
  # #--------------------------------------------------- 
  # # Run each line to get function details in the Help panel
  # ?easyart
  # ?Run_SegmentTarget
  # ?Run_RFM_Analysis
  # ?Run_Positioning_Analysis
  # ?Run_CLV_Analysis
  # ?Run_Optimal_Pricing
  # ?Run_Regression
  # ?Run_Regression_Prediction
  # ?Run_Logistic
  # ?Run_Logistic_Prediction
  # ?Run_Conjoint_Study_Design
  # ?Run_Conjoint_Analysis
  # 
  # # To learn more about using functions
  # # Run the next line and check out the Console
  # tips()
  # # To get the original copy of this script run this
  # getcodes()
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--          0. Practice with easyart             --
  # #--------------------------------------------------- 
  # 
  # # We will practice using this function in the classroom.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console() 
  # 
  # select_folder()
  # mycolor ="darkblue"
  # n = 10
  # easyart(n, mycolor)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--      1. Segmentation and Targeting            --
  # #--------------------------------------------------- 
  # 
  # # You need to have one CSV data file for your segmentation
  # # data and a second CSV file for your targeting data.
  # # Make sure you select the right file by paying attention
  # # to the variable name the load_csv_data() is being 
  # # assigned to.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # segmentation_data = load_csv_data()
  # targeting_data = load_csv_data()
  # segments = 3
  # Run_SegmentTarget(segmentation_data,targeting_data,segments)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--                2. RFM Analysis                --
  # #--------------------------------------------------- 
  # 
  # # You need to have a CSV data file that includes 
  # # three columns called Recency, Frequency, and Monetary
  # # respectively containing numeric values for R, F, and M.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # df = load_csv_data()
  # Run_RFM_Analysis(df)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--            3. Positioning Analysis            --
  # #--------------------------------------------------- 
  # 
  # # You need to have one CSV data file for your perceptions
  # # data and a second CSV file for your preferences data.
  # # Make sure you select the right file by paying attention
  # # to the variable name the load_csv_data() is being
  # # assigned to.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # perceptions_data = load_csv_data()
  # preferences_data = load_csv_data()
  # Run_Positioning_Analysis(perceptions_data,preferences_data)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--              4. CLV Analysis                  --
  # #--------------------------------------------------- 
  # 
  # # You need to have one CSV data file for your profit
  # # groups data and a second CSV file for your transition
  # # matrix data.
  # # Make sure you select the right file by paying attention
  # # to the variable name the load_csv_data() is being 
  # # assigned to.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # profitgroups_data = load_csv_data()
  # transitionmatrix_data = load_csv_data()
  # discount_rate = 0.15
  # new_customers = 0
  # more_customers = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  # papersize = 1.2
  # Run_CLV_Analysis(profitgroups_data,
  #                  transitionmatrix_data,
  #                  discount_rate,
  #                  new_customers,
  #                  more_customers,
  #                  resizepaper)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--            5. Optimal Pricing                 --
  # #--------------------------------------------------- 
  # 
  # # You need to have one CSV data file for your pricing
  # # survey data. You also need to specify the tested prices
  # # in the prices vector. Unless you need to, do not 
  # # change the values of the three remaining parameters.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # survey_data = load_csv_data()
  # prices = c(1.99,3.99,7.99,10.99,15.99,21.99,26.99)
  # probabilities = c(0, 0, 0, 0.2, 0.5)
  # marketsize = 10000
  # resizepaper = 1.2
  # Run_Optimal_Pricing(survey_data,
  #                     prices,
  #                     probabilities,
  #                     marketsize,
  #                     resizepaper)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--         6. Regression Analysis                --
  # #--------------------------------------------------- 
  # 
  # # You need to have one CSV data file for your regression
  # # data. You also need to specify the formula in myformula.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # mydata = load_csv_data()
  # head(mydata)
  # str(mydata)
  # myformula = "Sales ~ ."
  # Run_Regression(mydata, myformula)
  # newdata = load_csv_data()
  # Run_Regression_Prediction(mydata, newdata, myformula)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--       7. Logistic Regression Analysis         --
  # #--------------------------------------------------- 
  # 
  # # You need to have one CSV data file for your logistic
  # # regression data.
  # # You also need to specify the formula in myformula.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # mydata = load_csv_data()
  # head(mydata)
  # str(mydata)
  # myformula = "visited ~ ."
  # Run_Logistic(mydata, myformula)
  # newdata = load_csv_data()
  # Run_Logistic_Prediction(mydata, newdata, myformula)
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--         8. Conjoint Study Design              --
  # #--------------------------------------------------- 
  # 
  # # You only need to specify the design parameters and 
  # # their attributes here. The profiles design will be
  # # displayed in the Console and also saved in a csv 
  # # file in the folder you will select.
  # # The next few lines prepare and run the analysis. 
  # library(listentodata)
  # clear_console()
  # select_folder()
  # designparameters = list(
  #   NameOfAttribute1 = c("$100","$200","$300","500"),
  #   NameOfAttribute2 = c("A","B","C","D","E"),
  #   NameOfAttribute3 = c("Low","Medium","High"),
  #   NameOfAttribute4 = c("Yes","No"),
  #   NameOfAttribute5 = c("option1", "option2", "option3", "option4"),
  #   NameOfAttribute6 = c("option11", "option22", "option33", "option44")
  #   
  # )
  # Run_Conjoint_Study_Design(designparameters)  
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #---------------------------------------------------
  # #--            9.  Conjoint Analysis              --
  # #--------------------------------------------------- 
  # 
  # # This analysis requires multiple data files. 
  # # You also need to specify the formula in myformula.
  # # The next few lines allow you to analyze the provided
  # # tablet design data.
  # 
  # library(listentodata)
  # clear_console()
  # select_folder()  #Select the folder where your data files are
  # design = read.csv("Tablet_levels_attributes.csv", na.strings=c("","NA")) #design table
  # products = read.csv("Tablet_survey_products.csv")    # READ PRODUCT PROFILES DATA
  # ratings =  read.csv("Tablet_survey_ratings.csv")    # READ RATINGS DATA
  # alternatives = read.csv("Tablet_New_Product_Alternatives.csv") # Load New product alternative
  # competitors = read.csv("Tablet_competitors.csv")  # Load competitors products
  # resizepaper = 1
  # Run_Conjoint_Analysis(design,
  #                       products,
  #                       ratings,
  #                       alternatives,
  #                       competitors,
  #                       resizepaper)
  # 
  # # To analyze data sets from other projects,
  # # use these lines that allow you to load your data sets.
  # 
  # library(listentodata)
  # clear_console()
  # design = load_csv_data()
  # products = load_csv_data()
  # ratings =  load_csv_data()
  # alternatives = load_csv_data()
  # competitors = load_csv_data()
  # resizepaper = 1
  # Run_Conjoint_Analysis(design,
  #                       products,
  #                       ratings,
  #                       alternatives,
  #                       competitors,
  #                       resizepaper)
  # 

  
  # The guide at the bottom! Copy everything above this line
  

  cat("To get the Analysis Hub R Script, first type getcodes in the console\n")
  cat("with no parentheses at the end, and then press Enter/Return. \n")
  cat("Then copy all the commented text and paste them into a new R Script.\n")
  cat("Then selcet all the commented codes in that script and uncomment them\n")
  cat("by simultaneously pressing CTRL+SHIFT+C on your keyboard\n")
  


}