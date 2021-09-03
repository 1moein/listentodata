####################################################
#        CLV (Customer Lifetime Value) Analysis    #
####################################################
#    R Codes for Marketing Analytics               #
#    Author: Dr. Moein Khanlari                    #
#    Course: Marketing Analytics                   #
#    Version 2.0                                   #
#    Last updated: 2/13/2021                       #
####################################################



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
# if (!require(devtools)) install.packages("devtools")
# if (!require(mktgfunctions)) devtools::install_github("1moein/mktgfunctions")
# library(mktgfunctions)

####################################################


# # Load the data into variables of similar names
# profitgroups = read.csv("ProfitGroups_onlinestore.csv", header = TRUE)
# transitionmatrix = read.csv("TransitionMatrix_onlinestore.csv", header = TRUE)


#' Run CLV Analysis
#'
#' @param profitgroups_data data on profit groups
#' @param transitionmatrix_data  transition matrix
#' @param discount_rate discount rate
#' @param avg_new_customers_each_period Number of new customers added on avg. in each period
#' @param extra_customers_period1to10 Number of extra customers added due to promotions in certain period.
#' @param resizepaper How much larger should the pdf paper be for the results to fit?
#' @export
#' 
#' @examples
#' x=1:10 #just to get rid of the warning
Run_CLV_Analysis = function(profitgroups_data, transitionmatrix_data, discount_rate, avg_new_customers_each_period, extra_customers_period1to10, resizepaper=1.2) {
  
larger = resizepaper
avg_new_customers_each_period = avg_new_customers_each_period
discount_rate = discount_rate
extra = extra_customers_period1to10

profitgroups = profitgroups_data
transitionmatrix = transitionmatrix_data


# Set the parameters of the analysis

# # Specify the discount rate
# discount_rate = 0.15
# # Average number of new customers each period: Default value=0
# avg_new_customers_each_period = 0
# 
# # Additional expected new customers due to promotions
# # in the next 10 periods: Default values = 0
 # extra = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

# The value of avg_new_customers_each_period does not change CLV
# values if the average revenue and cost of the profit groups is
# assumed to remain constant.But it does change the valuation of
# the customer base.



## Some data preparation

# remove the column of customer group labels
pg = profitgroups[,-1]
tm = transitionmatrix[,-1]

pg
tm


# extract some customer data from pg
revenue = pg$RevenuePerPersonEachPeriod
cost = pg$CostPerPeriod
# extract initial number of customers from the data set
n = pg[,1]



# Make sure rows of the transition matrix sum to 1
if (any(rowSums(tm)!=1)) {
  rm(list=ls())
  stop(paste("STOP: Transition matrix rows should sum to 1!\n", "YOU CANNOT CONTINUE UNTIL YOU FIX THIS!"))
} #else print("Your transition matrix looks fine.")

# Maximum periods to analyze
periods = 240
# specify the number of periods to be displayed in tables
displayperiods = 10

# expand the extra vector to include all periods
# assuming zero extra customers are added after period 10
extra = c(extra, rep(0, times = (periods-length(extra))))

# Calculate number of customers in each future period
# The number of customers will decrease for all top
# groups according to the transition probabilities
# unless new customers are added

evolve = matrix(data=NA, nrow=periods+1, ncol=ncol(tm))
evolve[1,] = n
for (i in 2:(periods+1)){
  if (length(extra)!= periods) {
    rm(list=ls())
    stop(paste("ERROR: The number of values in the extra vector should not be larger than the value of displayperiods.", "You have to fix this issue before you can proceed. All your data have been deleted from memory."))
  }
  # calculate how new customers will be divided among groups.  
  # Assuming, none of them falls into the bottom group
  ncut = n[1:(length(n)-1)]
  # calculate weights and set zero for last group's weight
  percent_in_each_group = c(round(ncut/sum(ncut),2),0)
  new_customers = round(avg_new_customers_each_period * percent_in_each_group, 0)
  extra_customers = round(extra[i-1] * percent_in_each_group, 0)
  # calculate the new number of people in each group
  # using the transition matrix
  new_n = round(colSums(n*tm),0)
  # next, add new customers, if any
  new_n = new_n + new_customers + extra_customers
  evolve[i,] = new_n
  n = new_n
  
}


# Plot of the Number of Customers in each period for each customer group
graphics::matplot(evolve[1:displayperiods,], col=grDevices::rainbow(ncol(tm)), lwd=2, type = c("b"),pch=19, main="Number of customers in each period", ylab="Number of Customers", xlab="Periods")
graphics::legend("topleft", legend = names(tm), col=grDevices::rainbow(ncol(tm)), pch=19)


# Beautify evolve
evolve2 = data.frame(t(evolve[1:(displayperiods+1),]))
base::rownames(evolve2) = names(tm)          #******* added base, good?
names(evolve2) = c("Start", paste(rep("Period",ncol(evolve2)-1),1:displayperiods, sep=" "))

evolve2

# Do the CLV calcuations

# This section involves programming the CLV calculations.
# They mainly do the operations needed to calculate the CLVs
# using data matrices, data frames and lists.


# Now, we need to calculate CLV values for each customer group.
# Create a matrix on how each customer may change groups
# over time based on the probabilities in the transition matrix


# create the data frames and assign initial values
transitions = list()
netmargins = list()
groups = ncol(tm)
for (i in 1:groups){
  transitions[[i]] = data.frame(matrix(data=0, nrow = (periods+1), ncol = groups))
  # Added 1 to allow for the initial period.
  # At the beginning, everyone (100%) is in their own group
  transitions[[i]][1,i] = 1
  netmargins[[i]] = c(0,rep(0, periods))
}

# This loop not working in package ***************************************************************
# Calculate transitions and net margins
for (i in 1:groups){
  for (j in 1:(periods)){
    temp1 = rep(0,groups)
    for (k in 1:groups) temp1 = temp1 + transitions[[i]][j,k]*as.vector(tm[k,])
    transitions[[i]][j+1,] = temp1
    temp2 = 0
    temp3 = 0
    for (k in 1:groups){
      temp2 = temp2 + cost[k]*transitions[[i]][j,k]
      temp3 = temp3 + revenue[k]*transitions[[i]][j+1,k]
    }
    netmargins[[i]][j+1] =  temp3  - temp2
  }}

# Prepare the CLV table for each customer group
# This section takes results from the previous table, and combines them
# to arrive at the information we need to have.

discount_factor = (1/(1+discount_rate))^(0:(periods)) # Add discount factor
group_clv = rep(0,groups)
for (i in 1:groups){
  discounted_margin = round(netmargins[[i]]*discount_factor,2)
  cumulativedm = cumsum(discounted_margin)
  results = t(cbind(round(100*transitions[[i]],1), netmargins[[i]],discount_factor,discounted_margin,cumulativedm))
  results = data.frame(round(results,2))
  # results = cbind(results[,1:11],results[,ncol(results)])
  names(results) = c("Start", paste(rep("Period",ncol(results)-1),1:(periods), sep=" "))
  base::rownames(results)[1:groups] = paste(names(tm),rep("Customers",groups), sep = " ")
  base::rownames(results)[(groups+1):(groups+4)] = c("Net Profit", "Discount Factor", "Discounted Profits (DP)", "Cumulated DP = CLV")
  group_clv[i] = results[nrow(results),ncol(results)]
  u = results[1:groups,]
  results[1:groups,] = apply(u,2, function(u) sprintf( "%.1f%%", u ) )
  results[(nrow(results)-3):nrow(results),1] = " "
  transitions[[i]] = results
}


# Customer base valuation
# Calculate the current dollar value of the customer base
# and put them all in a table called valuation.
Customer_Base = data.frame(matrix(data=0, nrow=groups, ncol=(periods)))
# determine which row of the table should display the profits
profitrow = nrow(transitions[[1]])-3

# determine the cumulative number of customers in each group
# as new customers are added in the future. If no new customers
# or extras due to promotion are added, then the cumulative
# number of customers in each group will remain constant and
# will equal the initial size of that group

members = matrix(data=NA, nrow=periods+1, ncol=ncol(tm))
n2 = pg[,1]
members[1,] = n2
for (i in 2:(periods+1)){
  if (length(extra)!= periods) print("ERROR: The number of values in the extra vector should be equal to the value of periods.")
  # calculate how new customers will be divided among groups.  
  # Assuming, none of them falls into the bottom group
  ncut = n2[1:(length(n2)-1)]
  # calculate weights and set zero for last group's weight
  percent_in_each_group = c(round(ncut/sum(ncut),2),0)
  new_customers = round(avg_new_customers_each_period * percent_in_each_group, 0)
  extra_customers = round(extra[i-1] * percent_in_each_group, 0)
  # next, add new customers, if any
  new_n2 = n2 + new_customers + extra_customers
  members[i,] = new_n2
  n2 = new_n2

}


for (i in 1:groups){
  Customer_Base[i,] =  members[1:(nrow(members)-1),i]*as.numeric(transitions[[i]][profitrow,-1])
}


total_net_margin = colSums(Customer_Base)
discountd_total_net_margin = total_net_margin*discount_factor[-1]
cumulated = cumsum(discountd_total_net_margin)

valuation = rbind(Customer_Base,total_net_margin,discountd_total_net_margin,cumulated)
valuation = valuation[,c(1:10,ncol(valuation))]

# Beautify the valuation table
names(valuation) = c(paste(rep("Period",10),1:10, sep=" "), "Period 240")
somenames = paste(names(tm),rep("Group\n Profit Contribution",groups))
row.names(valuation) = c(somenames, "Profits", "Discounted Profits (DP)", "Cumulated DP")
valuation = round(valuation,-3)

valuation2 = apply(valuation,2,addcomma)

# Only keep the first 11 columns and the last column of the transitions data frames
transitions2 = transitions
for ( i in 1:groups) transitions2[[i]] =transitions[[i]][,c(1:11,periods+1)]

# Calculate the average CLV
# multiply the clv value of each group by the size
# of that group and divide by the total number of customers
# the rev() function ensures that our CLV values start
# with the platinum group to be aligned with CV for the
# inner product of the two vectors to work.
AvgCLV = pg[,1] %*% rev(group_clv) / sum(pg[,1])

# Plot a graph of CLV values
names(group_clv) = names(tm)
group_clv = rev(group_clv)
group_clv = group_clv[-1]

# Plot a graph of CLV values
myplot = graphics::barplot(group_clv, horiz=FALSE,
                 col = grDevices::rainbow(length(group_clv), alpha=0.45),
                 main = paste("CLV value for each customer group",
                              "\nDiscount Rate = ", discount_rate, sep=""),
                 ylab = "Customer Lifetime Value (dollars)",
                 xlab = "Profit Groups"
)
graphics::legend("topleft", inset =0.03, legend = rev(names(group_clv)), fill = rev(grDevices::rainbow(length(group_clv), alpha=0.45)))
graphics::text(x=myplot, y=0.5*group_clv,
     labels=paste(rep("$",length(group_clv)), round(group_clv,1), sep=""))
graphics::abline(h=AvgCLV, col="deeppink4", lwd=3)
graphics::text(x=(length(group_clv)/2 - 1), y=1.1*AvgCLV,
     labels=paste("Average CLV (weighted) = $",round(AvgCLV,1), sep=""),
     col = "deeppink4")

################# save results in a pdf file ############

# Set up some parameters
# Decide what to call your file name; make sure to put .pdf at the end of the name
filename =  paste("! Results_CLV_Analysis_Discount_Rate=",discount_rate, ".pdf", sep="")
# larger =  1.2
# # If the tables are not displayed properly in the pdf pages, change the
# # value for "larger" above to 1.5, 1.7, 1.8, 2, or larger values to get larger
# # page sizes that can contain the tables.


# # Create a theme for formatting our tables
# if (!require(gridExtra)) install.packages("gridExtra")
# library(gridExtra)
mytablecolors = c("#ccffcc", "#ffffcc")
mytheme = gridExtra::ttheme_minimal(
  core=list(bg_params = list(fill =  mytablecolors, col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="black", fontface=3L)))

# Create labels for some tables

tops = paste(rep("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n",groups),names(tm),
             rep(":\n CLV of one customer in the ",groups),
             names(tm),rep(" profit group.",groups), 
             sep = "")

mytitle = paste("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nThis Company's customer base is approximately worth\n $",
                valuation2[nrow(valuation2),ncol(valuation2)], sep="")
#Improve the looks of the transaction data for printing
base::rownames(transitionmatrix) = transitionmatrix[,1]
transitionmatrix = transitionmatrix[,-1]


# pdf(filename, height=larger*8.5, width=larger*11)
suppressWarnings(res4 <- try(grDevices::pdf(filename, height=larger*8.5, width=larger*11), silent = TRUE))


row.names(profitgroups) <- profitgroups$Labels
profitgroups <- profitgroups[,-1]# delete row numbers
gridExtra::grid.arrange(top="\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nProfit Groups", 
             gridExtra::tableGrob(profitgroups, theme=mytheme))

gridExtra::grid.arrange(top="\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nTransition Matrix", 
             gridExtra::tableGrob(transitionmatrix, theme=mytheme))

# Plot a graph of CLV values
myplot = graphics::barplot(group_clv, horiz=FALSE,
                 col = grDevices::rainbow(length(group_clv), alpha=0.45),
                 main = paste("CLV value for each customer group",
                              "\nDiscount Rate = ", discount_rate, sep=""),
                 ylab = "Customer Lifetime Value (dollars)",
                 xlab = "Profit Groups"
)
graphics::legend("topleft", inset =0.03, legend = rev(names(group_clv)), fill = rev(grDevices::rainbow(length(group_clv), alpha=0.45)))
graphics::text(x=myplot, y=0.5*group_clv,
     labels=paste(rep("$",length(group_clv)), round(group_clv,1), sep=""))
graphics::abline(h=AvgCLV, col="deeppink4", lwd=3)
graphics::text(x=(length(group_clv)/2 - 1), y=1.1*AvgCLV,
     labels=paste("Average CLV (weighted) = $",round(AvgCLV,1), sep=""),
     col = "deeppink4")

gridExtra::grid.arrange(top="\n\n\n\n\n\n\n\n\n\n\n\n\n\n\nNumber of customers in each period",
                        gridExtra::tableGrob(evolve2, theme=mytheme))

# Plot of the Number of Customers in each period for each customer group
graphics::matplot(evolve[1:displayperiods,], col=grDevices::rainbow(ncol(tm)), lwd=2, type = c("b"),pch=19, 
        main="Number of customers in each period", ylab="Number of Customers", xlab="Periods")
graphics::legend("topleft", legend = names(tm), col=grDevices::rainbow(ncol(tm)), pch=19)


gridExtra::grid.arrange(top=mytitle, gridExtra::tableGrob(valuation2, theme=mytheme))


for (i in 1:(groups-1)) gridExtra::grid.arrange(top=tops[i], gridExtra::tableGrob(transitions2[[i]], theme=mytheme))

grDevices::dev.off()

if (!is.null(res4)){
  cat("\n ERROR:\n The analysis was performed, but we were NOT able to save ")
  cat(paste("\n the results in: ", filename))
  cat(" \n This is probably due to a PDF file with the same name being open.\n")
  cat(" Make sure you close that file, and then run the previous line of code again.")
} else {
  cat("\n CLV analysis has been performed on these data!\n")
  cat("\n Results have been saved in a PDF file named: \n ")
  cat(paste(filename, " \n"))
  cat(" You can find this file in the same folder as your data files, which is here:\n ")
  cat(as.character(getwd()))
  cat(" \n\n ")
  cat(" If you see any warnings below, simply disregard them.\n\n ")
}


}