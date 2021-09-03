###########################################################
#                Optimal Uniform pricing                  #
###########################################################
#    R Codes for Marketing Analytics                      #
#    Author: Moein Khanlari, PhD                          #
#    Version 1.0                                          #
#    Copyright? 2019, Moein Khanlari All rights reserved. #
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
# ####################################################

# # Specify the tested price levels
# p = c(1.99,3.99,7.99,10.99,15.99,21.99,26.99)
# # Specify the actual purchase probabilities
# app = c(0, 0, 0, 0.2, 0.5) #subjective estimate of actual purchase probabilities


#' Run Optimal Pricing
#'
#' @param survey_data pricing survey data
#' @param prices  vector of tested prices
#' @param probabilities actual purchase probabilities
#' @param marketsize total size of the market
#' @param resizepaper How much larger should the pdf paper size be to fit everything?
#'
#' @return a pdf file
#' @export
#' 
#' @examples
#' x=1:10 #just to get rid of the warning
Run_Optimal_Pricing = function(survey_data, prices, probabilities, marketsize =10000, resizepaper=1.2) {
  
s = survey_data
p = prices
app = probabilities
C = marketsize
larger = resizepaper
  
# Load the pricing survey dataset;

# s = read.csv(file="steelsoap_survey_allthedata.csv", header = T)

# If interested, you can also play around with the following data sets
# which represent students from different classes who have taken this survey:
# s = read.csv(file="steelsoap_survey_spring2019.csv", header = T)
# s = read.csv(file="steelsoap_survey_fall2019.csv", header = T)
# s = read.csv(file="steelsoap_survey_spring2020.csv", header = T)

# As a reminder, the Likert scale was used in the price survey.
# very unlikely = 1, ..., very likely = 5
Likert_scale = c(1,2,3,4,5)


utils::head(s)
s = s[,-1]
m = length(p)
number_of_respondents = nrow(s)

# q will represent the quantity demanded at each price point
# For each tested price, we calculate q by taking the number of people who have
# selected each Likert scale level and multiplying it by the subjective estimate of actual
# purchase probabilities and summing up the results for all Likert points at each price level.

q = rep(NA,m)


for (i in 1:m){
  # Fore each column i of the dataset s, count how many 1's, 2's,..., and 5's there are in that column
  howmany1s = sum(s[,i]==1)
  howmany2s = sum(s[,i]==2)
  howmany3s = sum(s[,i]==3)
  howmany4s = sum(s[,i]==4)
  howmany5s = sum(s[,i]==5)

  counts = c(howmany1s, howmany2s, howmany3s, howmany4s, howmany5s)
  q[i] = app%*%counts
}

# Plot observed demand
Likelihood = q/number_of_respondents
plot(p, q, pch=19, cex=1.1, cex.axis=1.1, cex.lab=1.1,
     cex.main=1.1, xlab = "Unit Price (P)",
     ylab = "Observed Demand (Q)",
     main="Observed Demand and Purchase likelihood\n at each tested price point",
     col="darkgreen", xlim=c(0, 1.2*max(p)), ylim=c(0, 1.2*max(q)))
graphics::xspline(p, q, shape=-0.5, lwd=2)
graphics::text(p, q, paste("Likelihood is ",round(Likelihood,2),"\n @ $",p, sep = ""),
     cex=1, pos=3, offset = 1, col="navyblue")


######################################
# Estimate demand using a Logit price response function
######################################

# set market size C
# If C is large enough (e.g., C > 10*max(q)),
# its influence on the optimal price will be negligible.
# C=10000

# C should not be smaller than the maximum observed demand
if (C<max(q)) {
  print("Market size (C) cannot be smaller than the maximum observed demand.")
  print("Increase the Value of C!")
}


# Next line is a technical precaution to make sure y can always be calculated
q[q==0] = 0.0000000000000000000000001

y = log((C-q)/q)


model1 = stats::lm(y ~ p)
model2 = stats::lm(y ~ p + log(p))
model3 = stats::lm(y ~ p + sqrt(p))
model4 = stats::lm(y ~ p + log(p) + sqrt(p))


BICs = c(stats::BIC(model1),stats::BIC(model2),stats::BIC(model3),stats::BIC(model4))
selected = which(BICs==min(BICs))

if (selected==1) selected_model = model1
if (selected==2) selected_model = model2
if (selected==3) selected_model = model3
if (selected==4) selected_model = model4


# Display the BICs and the selected model
BICs
summary(selected_model)


# Use the parameters of the selected model
temp = selected_model$coefficients
temp2 = round(temp,2)

b = rep(0,4)
try(b[1] <- temp[which(names(temp)=="(Intercept)")], silent = TRUE)
try(b[2] <- temp[which(names(temp)=="p")], silent = TRUE)
try(b[3] <- temp[which(names(temp)=="log(p)")], silent = TRUE)
try(b[4] <- temp[which(names(temp)=="sqrt(p)")], silent = TRUE)

predicted_y = function(x) b[1] + b[2]*x + b[3]*log(x) +b[4]*sqrt(x)


predicted_demand = function(p) {
  predicted_y = b[1] + b[2]*p + b[3]*log(p) +b[4]*sqrt(p)
  pred_q =  C*(1-(1/(1+exp(-predicted_y))))
  return(pred_q)
}

# Specify the revenue function
predicted_revenue = function(x) predicted_demand(x)*x

# Find the optimal price
optimal = stats::optimize(predicted_revenue, c(min(p), max(p)), maximum = TRUE)
op = optimal$maximum
op

# Calculate predicted revenue and demand to plot them
x = seq(min(p),max(p),0.01)
y = predicted_revenue(x)
z = predicted_demand(x)*(C/number_of_respondents)
maxrev = max(y)
op_demand = predicted_demand(op)*(C/number_of_respondents)


# Plot the estimated demand curve
plot(x,z, type="l", lwd=2, cex.main = 1, cex=1.1, cex.axis=1.1, cex.lab=1.1, xlab="Unit Price (P)", ylab="Quantity Demanded (Q)", main=paste("Estimated Demand Curve at different price levels\n based on specified market size(i.e., C=",C,")",sep=""), col="navy")
graphics::abline(v=op, col="darkgreen", lwd=2)
graphics::text(op, 0.9*max(z), cex=1, pos=4, offset = 0.2, col="darkgreen", paste("Optimal Price = $",round(op,2)))
graphics::text(0.6*max(p), 0.9*op_demand, col="#000000", cex=1,
     bquote(bolditalic(Q(P) ==
                         .(C) * (1 - frac(1, 1 + paste(e^- (paste(.(temp2[1]), " ", .(getsign(temp2[2])), " ", .(abs(temp2[2])),"P ", .(getsign(temp2[3])), " ", .(abs(temp2[3])),log(P), " ", .(getsign(temp2[4])), " ", .(abs(temp2[4])),sqrt(P)))))
                         ))))

# Plot the revenue curve
plot(x,y, cex.main=1, type="l", lwd=2, cex=1.1, cex.axis=1.1, cex.lab=1.1, xlab="Unit Price", ylab="Revenue", main="Revenue at different price levels\n from 100 customers", col="navy")
graphics::abline(v=op, col="darkgreen", lwd=2)
graphics::text(1.1*op, maxrev, cex=1, pos=4, offset = 0.2, col="darkgreen", paste("Optimal Price = $",round(op,2)))

graphics::text(op, maxrev/2, paste("Purchase likelihood at $", round(op,2)," is ",
                         round(predicted_demand(op)/number_of_respondents,2),
                         ".\nMaximum revenue from ",number_of_respondents," customers\nwill be: $",
                         round(maxrev,2)," which is calculated from: \noptimal price x \npurcahse likelihood x \nnumber of customers)",
                         sep = ""), cex=1, pos=4, offset = 0.2, col="darkgreen")


################# save results in a pdf file ############

# Set up some parameters
# Decide what to call your file name; make sure to put .pdf at the end of the name
filename =  "! Results_Optimal_Pricing.pdf"
# larger =  1.2
# pdf(filename, height=larger*8.5, width=larger*11)


suppressWarnings(res5 <- try(grDevices::pdf(filename, height=larger*8.5, width=larger*11), silent = TRUE))




# Plot the observed demand
plot(p, q, pch=19, cex=1.1, cex.axis=1.1, cex.lab=1.1,
     cex.main=1.1, xlab = "Unit Price (P)",
     ylab = "Observed Demand (Q)",
     main="Observed Demand and Purchase likelihood\n at each tested price point",
     col="darkgreen", xlim=c(0, 1.2*max(p)), ylim=c(0, 1.2*max(q)))
graphics::xspline(p, q, shape=-0.5, lwd=2)
graphics::text(p, q, paste("Likelihood is ",round(Likelihood,2),"\n @ $",p, sep = ""),
     cex=1, pos=3, offset = 1, col="navyblue")

# Plot the estimated demand curve
plot(x,z, type="l", lwd=2, cex.main = 1, cex=1.1, cex.axis=1.1, cex.lab=1.1, xlab="Unit Price (P)", ylab="Quantity Demanded (Q)", main=paste("Estimated Demand Curve at different price levels\n based on specified market size(i.e., C=",C,")",sep=""), col="navy")
graphics::abline(v=op, col="darkgreen", lwd=2)
graphics::text(op, 0.9*max(z), cex=1, pos=4, offset = 0.2, col="darkgreen", paste("Optimal Price = $",round(op,2)))
graphics::text(0.6*max(p), 0.9*op_demand, col="#000000", cex=1,
     bquote(bolditalic(Q(P) ==
                         .(C) * (1 - frac(1, 1 + paste(e^- (paste(.(temp2[1]), " ", .(getsign(temp2[2])), " ", .(abs(temp2[2])),"P ", .(getsign(temp2[3])), " ", .(abs(temp2[3])),log(P), " ", .(getsign(temp2[4])), " ", .(abs(temp2[4])),sqrt(P)))))
                         ))))

# Plot the revenue curve
plot(x,y, cex.main=1, type="l", lwd=2, cex=1.1, cex.axis=1.1, cex.lab=1.1, xlab="Unit Price", ylab="Revenue", main="Revenue at different price levels\n from 100 customers", col="navy")
graphics::abline(v=op, col="darkgreen", lwd=2)
graphics::text(1.1*op, maxrev, cex=1, pos=4, offset = 0.2, col="darkgreen", paste("Optimal Price = $",round(op,2)))

graphics::text(op, maxrev/2, paste("Purchase likelihood at $", round(op,2)," is ",
                         round(predicted_demand(op)/number_of_respondents,2),
                         ".\nMaximum revenue from ",number_of_respondents," customers\nwill be: $",
                         round(maxrev,2)," which is calculated from: \noptimal price x \npurcahse likelihood x \nnumber of customers)",
                         sep = ""), cex=1, pos=4, offset = 0.2, col="darkgreen")

grDevices::dev.off()


if (!is.null(res5)){
  cat("\n ERROR:\n The analysis was performed, but we were not able to\n save the results in \"! Results_Optimal_Pricing.pdf\"")
  cat(" \n This is probably due to a PDF file with the same name being open.\n")
  cat(" Make sure you close that file, and then run the previous line of code again.")
  
} else {
  cat("\n Optimal Pricing analysis has been performed on these data!\n")
  cat("\n Results have been saved in a file named: \"! Results_Optimal_Pricing.pdf\"\n")
  cat(" You can find this file in the same folder as your data file, which is here:\n ")
  cat(as.character(getwd()))
  cat(" \n\n ")
  cat(" If you see any warnings below, simply disregard them.\n\n ")
  
}

}