###########################################################
#          Targeting using RFM Analysis                   #
###########################################################
#    R Codes for Marketing Analytics                      #
#    Author: Dr. Moein Khanlari                           #
#    Course: Marketing Analytics                          #
#    Version 1.0                                          #
#    Copyright 2019, Moein Khanlari, All rights reserved. #
#    This software is provided to students at the         #
#    University of New Hampshire on an "AS IS" BASIS,     #
#    WITH ABSOLUTELY NO WARRANTIES either expressed or    #
#    implied. The software may not be redistributed       #
#    in whole or part without the express written         #
#    permission of the author.                            #
###########################################################


## Remove all variables from memory to start fresh
#rm(list=ls())
# If there are plots, delete them to start fresh
#if (length(dev.list())!= 0) dev.off()


# Introduction

# This is a code template for RFM analysis.
# Please notice that this code will not generate a pdf file.
# The final results will be available in the R Console as a data table.
# They will also be saved in CSV file that can be opened with Excel or a similar spreadsheet program.

# RFM stands for Recency, Frequency, and Monetary value. These are the most readily available
# customer data for most businesses that can be put together from purchase transactions data in a
# particular period, for example, a quarter, six months, or a year.
#
#   Recency        (R): How recently (i.e., how many days ago) has a customer made a purchase?
#   Frequency      (F): How frequently (i.e., how many times) has a customer made a purchase?
#   Monetary value (M): How much has a customer purchased in total?
#
#   Taken together, these three data points about a customer can allow a firm to assign
# an approximate value to each customer.
#
#
#   For this data analysis template, we will simulate data. For an actual data set,
# the data has to be loaded first:


#----------------- Simulate data for this template-------
#
# For this template, I am simulating a small data set. This block of code
# should be excluded if you have a real data set to work on.

# x = 31 # I'm keeping the dataset small for simplicity
# set.seed(1000)
# customerID = paste("customer",1:x, sep='')
# R = sample(1:100, x, replace = T)
# Fr = sample(1:20, x, replace = T) # calling it Fr instead of F, since F is reserved for FALSE in R
# M = sample (40:1000, x)
# df = data.frame(customerID,R,Fr,M)
# df

#-------- For an actual analysis, load your data set instead of simulating data ---------------

## We now have a simulated data set for the purposes of this demonstration.
## In real projects, we do not simulate customer data, because we have them.
## To analyze data from real customers, you will need to load the data set into R:
## by first setting the working directory to the folder in which your dataset is located
## and then load it into R using the appropriate function such as read.csv().
## For this exercise, these lines of code are commented out as we won't use them.
## But you can either set your working directory manually here:

# setwd("Yourfoldername1/Yourfoldername2")

## or you can tell RStudio to set it to THIS open R Script's current directory

# if (!require(rstudioapi)) install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## And then load your RFM data set here:

# df = read.csv(file="Yourdatafilename.csv")


#' RFM Analysis
#'
#' @param mydata
#'
#' @return
#' @export
#'
#' @examples
#' Run_RFM_Analysis(yourRFMdataset)
Run_RFM_Analysis <- function(mydata){
    
    # cat("\014")
    clearconsole()
    df <- mydata
    
    if ((!("Recency" %in% names(df))) | (!("Frequency" %in% names(df))) | (!("Monetary" %in% names(df)))){
        stop(' \n You have not selected a valid data file:\n Your data file must be a csv file containing\n three columns named: Recency, Frequency, and\n Monetary with numeric values in each column.\n\n          NO Analysis Was Performed!')
        return()
    } else {
        bad1 = suppressWarnings(any(is.na(as.numeric(df$Recency))))
        bad2 = suppressWarnings(any(is.na(as.numeric(df$Frequency))))
        bad3 = suppressWarnings(any(is.na(as.numeric(df$Monetary))))
        if (bad1 | bad2 | bad3){
            stop(' \n Your Recency/Frequency/Monetary data contain non-numeric value(s). \n\n          NO Analysis Was Performed!\n\n')
            return()
        }
    }
    
    
    #---------------- Create empty columns for scores----------
    
    # Create empty columns for R, F, M, and RFM scores and attach them to the dataset:
    
    n = nrow(df)
    R_score = rep(NA, n)
    F_score = rep(NA, n)
    M_score = rep(NA, n)
    RFM_Score = rep(NA, n)
    df = cbind(df, R_score, F_score, M_score, RFM_Score)
    
    
    #-------------- Assign R, F, and M scores------------
    
    # Sort the dataset by R and assign R scores, sort it by F and assign F scores, then sort it by M and assign M scores.
    # R is sorted from smallest to largest: more recent purchases are more valuable.
    # Fr is sorted from largest to smallest: higher frequency of purcahse is more valuable.
    # M is sorted from largest to smallest: more spending is more valuable.
    #
    # After the dataset is ordered based on each of the above three variables,
    # scores of 5, 4, 3, 2, and 1 are assigned to the top 20%, the next 20%,and so on...
    #
    # Slight complication:
    #   If the number of customers is not divisible by 5, the bin sizes will not be equal to 20% each.
    # For this demonstration with 31 customers, 20% of 31 will be 6.2. So, we can have five bins of 6 people.
    # But we will have to have one bin of 7 customers to make sure all customers are assigned to one of
    # our 20% bins. That 7-person bin will be slightly larger than 20% which is OK.
    #
    #   The k variable below counts the number of customers that were not divisible by 5
    # and adjusts the bin sizes so that we have approximately 20% of customers in each
    # bin.
    #   for n=31, we will have group sizes of [7 6 6 6 6] = [6 6 6 6 6] + k, in which k is [1 0 0 0 0]
    #   for n=32, we will have group sizes of [7 7 6 6 6] = [6 6 6 6 6] + k, in which k is [1 1 0 0 0]
    #   for n=35, we will have group sizes of [7 7 7 7 7] = [6 6 6 6 6] + k, in which k is [0 0 0 0 0]
    
    
    k = rep(0,5)
    m = floor(0.2*n)
    if (n%%5>0) k[1:(n%%5)] = rep(1,n%%5)
    scores = c(rep(5, m + k[1]), rep(4, m + k[2]), rep(3, m + k[3]), rep(2, m + k[4]), rep(1, m + k[5]))
    
    df = df[order(df$Recency),]
    df$R_score = scores
    df = df[order(-df$Frequency),]
    df$F_score = scores
    df = df[order(-df$Monetary),]
    df$M_score = scores
    
    #------------- Combine R, F, and M scores to calculate RFM scores------
    
    # Now, we will combine the three scores to arrive at the RFM score using weights of 100, 10, and 1
    # based on an assumption on the relative importance of R > F > M
    # The weighting can change depending on the importance of R, F, and M in a given context
    # or a given product category based on experience and domain knowledge.
    
    
    df$RFM_Score = 100*df$R_score + 10*df$F_score + df$M_score
    
    # You can check that the scores have been correctly assigned.
    # To do that, we can separately sort each of R, Fr, and M in the correct
    # order and make sure that their corresponding Scores are all
    # sorted from 5 to 1. For example, after sorting Recency from
    # smallest to largest, the R scores should be displayed from
    # 5 to 1.
    # As another example, if you sort frequency (Fr) from highest
    # to lowest, then the Fr Scores should be displayed from 5 to 1.
    
    df[order(df$Recency),]
    df[order(-df$Frequency),]
    df[order(-df$Monetary),]
    
    #--------- Sort the data set based on RFM scores----------
    
    # Finally, we will sort the dataset based on RFM scores from largest to smallest.
    
    df = df[order(-df$RFM_Score),]
    # df
    
    # We can save the results in a CSV file to be used in Excel later.
    # It will be save in the current working directory. The getwd()
    # function will tell you what the current working directory is so
    # you can easily find the saved Excel file.
    
    # getwd()
    # library(crayon) #Colored Terminal Output
    
    # cat("\014")
    # clearconsole()
    # cat("\n")
    
    suppressWarnings(res <- try(write.csv(df, file ="RFM_Analysis_Results.csv", row.names = FALSE), silent = TRUE))
    
    
    cat("\nRFM analysis has been performed on these data!\n")
    
    if (!is.null(res)){
        cat("\n Warning:\n We were not able to save the results to RFM_Analysis_Results.csv")
        cat(" \n If a file with the same name is already open, make sure you close\n it, and then re-run the analysis.")
        cat(" \n Otherwise, make sure you have permission to create new\n files in the folder where your data file is located.\n")
        cat(" If you do not need to save the results to a file,\n you can safely ignore this warning.\n")
        
    } else {
        cat("\n Results are displayed below and also saved in a \n file named: RFM_Analysis_Results.csv\n")
        cat(" located in this folder:\n")
        cat(as.character(getwd()))
        cat("\n Note that the results have been sorted\n based on RFM Scores from highest to lowest.\n\n")
        
    }
    
    cat("                    RFM Analysis Results \n")
    
    
    
    print(df, quote = FALSE, row.names = FALSE)
    
    #--------- How to use the calculated RFM scores----------
    
    # These RFM scores create 5x5x5=125 cells or customer groups
    # with higher RFM values representing more valuable customers.
    #
    # The top groups are more likely to respond to direct marketing campaigns
    # but the bottom groups are not to be ignored. Managers can choose
    # to treat each group differently based on short- or long-term goals
    # with different marketing mix strategies (product, price, place, and promotion)
    # For example, you can target the top 5 groups (RFM scores 555,554,553,552,551)
    # or target the top 25%, 30%, 40%, or 50% of your customers based on RFM scores
    # or target specific groups (e.g, group 155) who have not recently purchased but
    # rank high on Frequency and Monetary value.
    
    
    
    #--------------- Final words------------------
    
    # RFM analysis is easy to do and only relies on customers' past purchase transactions.
    # It provides a practical way to create value-based customer segments and target them for direct
    # marketing campaigns (e.g., email campaigns, promotional offers email/mail).
    #
    # However, it has its drawbacks: the weights assigned to R, F, and M data are arbitrary and there may be
    # additional factors beyond RFM data that can measure customers' value to the firm or predict their purchase likelihood.
    #
    # A better alternative to RFM analysis for direct marketing would be to estimate a logistic regression (Logit) model.
    # However, using a Logit model requires additional individual-level customer data, including data on their past responses to
    # direct marketing campaigns.
    
}
#write.csv(df,file="RFMdata.csv", row.names = FALSE)
