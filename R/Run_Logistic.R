#' Logistic Regression Analysis
#' 
#' This function conducts a Logistic regression analysis and provides the results in a pdf file. 
#' It can also perform predictions on a new csv data file and provide the results in a csv file.
#'
#' @param mydata csv data file
#' @param myformula formula for the glm() model
#' @param prediction_data csv data file with the same set of variable names as those in the mydata data set; intended for prediction. The da
#' The default value set at 0 tells the function that no prediction data set has been provided.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' # This is the sample code to be copied and used in a new R Script:
#' library(listentodata)
#' clear_console()
#' mydata = load_csv_data()
#' head(mydata)
#' str(mydata)
#' myformula = "visited ~ ."
#' Run_Logistics(mydata, myformula)
#' # If you have data for prediction:
#' prediction_data = load_csv_data()
#' Run_Logistic(mydata, myformula, prediction_data)
#' }

Run_Logistic = function(mydata,myformula, prediction_data=0) {
  # Companies constantly send out promotional emails to their customers hoping
  # to get them to click a link within these emails and visit their websites.
  # Such visits can engage customers with the company's website and potentially
  # lead to online purchases.
  #
  # In this example, we analyze some characteristics of promotional emails that
  # have been sent to 1000 customers. We will determine what factors increase
  # the probability that customers might click the link inside these promotional
  # emails to visit the company's website.
  #
  # The outcome of interest in this example is binary and can be coded as either
  # 0 or 1; there are two possibilities: the email recipients either visit the
  # website or not.
  
  
  # # For testing purposes
  # mydata = read.csv("T:\\MarketingAnalytics\\marketing_analytics\\Data for regression\\emailclickdata.csv")
  # myformula = "visited ~ ."
  # prediction_data = read.csv("T:\\MarketingAnalytics\\marketing_analytics\\Data for regression\\newemails.csv")
  
  # if (datafortraining<=0.9 & datafortraining>=0.2) {
  #     set.seed(1378)
  #     index <- sample(1:nrow(mydata),round(datafortraining*nrow(mydata)))
  #     d <- mydata[index,]
  #     df_test <- d[-index,]
  #   } else {
  #     stop("The value of datafortraining represents \n the percentage of the data you will use for\n training your model. It must be between 0.2 and 0.9.\n Analysis was aborted; Please chage that value and try again!\n We suggest using 0.75")
  #   }
  
  d = mydata
  ds = summary(d)
  showable = dim(d)[2]
  if (showable>14){
    showable = 14
  }
  dsample = d[1:10,1:showable]
  
  options(scipen=999) # to prevent the use of scientific notation in results
  
  
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
  fullcols = dim(ds)[2]
  firsthalf = ceiling(fullcols/2)
  secondhalf = firsthalf + 1
  
  # gridExtra::grid.arrange(top="\n\n\n\n\n\n View the first 10 rows of the data set \n a max. of 14 columns can be shown here.", gridExtra::tableGrob(dsample, theme=mytheme))
  # gridExtra::grid.arrange(top="\n\n\n\n\n\n Summary Statistics for the variables \n Page 1 of 2", gridExtra::tableGrob(ds[,1:firsthalf], theme=mytheme))
  # gridExtra::grid.arrange(top="\n\n\n\n\n\n Summary Statistics for the variables \n Page 2 of 2", gridExtra::tableGrob(ds[,(secondhalf:fullcols)], theme=mytheme))
  
  
  
  
  # correlations 
  
  nums <- unlist(lapply(d, is.numeric))
  numcols <- d[ , nums]
  
  
  # graphics::pairs(numcols, lower.panel = NULL, pch=16,cex=0.3)
  
  #Visualize the correlations among numeric variables as numbers or pie charts
  correlations = stats::cor(numcols)
  # corrplot::corrplot(correlations, method="pie")
  # corrplot::corrplot(correlations, method="number")
  
  
  ## Estimate the logistic regression model and show its summary
  
  logit1 = stats::glm(myformula, data=d, family = "binomial")
  
  
  # Display the results
  
  # summary of results from the full model
  rez1 = jtools::summ(logit1)
  
  
  
  # rez1_details =t(data.frame(analysistype= "Logistic Regression",
  #                            Dependent.Var=attributes(rez1)$dv,
  #                            Observations=nrow(d),
  #                            R_Squared=round(attributes(rez1)$rsq,2),
  #                            Adj_R_Squared=round(attributes(rez1)$arsq,2),
  #                            Missing_Values=attributes(rez1)$missing,
  #                            F_Stat=round(attributes(rez1)$fstat,2),
  #                            Model_P_Value = round(attributes(rez1)$modpval,2)))
  # 
  # 
  # 
  # metrics = c("Analysis:", "Dependent Variable:", "Observations:","R-Squared:", "Adjusted R-Squared:","Missing Values:", "Model F-stat:", "Model P-Value:"  )
  # rez1_deets = data.frame(metrics, rez1_details)
  # row.names(rez1_deets) = NULL
  # names(rez1_deets) = c("  ", "  ")
  
  coeftable1 = round(rez1$coeftable,2)
  attributes(coeftable1)$dimnames[[2]] = c("Estimate", "Std. error", "t-statistic", "p-value")
  
  
  
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
  suppressWarnings(jtools::plot_summs(logit1, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95))
  
  
  # Find the best model using stepwise regression.
  # What variables should remain and what variables should be deleted
  # from the equation?\
  
  # selected_model = stats::step(m6)
  
  # utils::capture.output({
  #   selected_model = stats::step(logit1)
  # })
  selected_model = logit1
  
  suppressWarnings(jtools::plot_summs(selected_model, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95))
  
  
  ## Predict the probability of visiting
  if (typeof(prediction_data)!="double"){
    df = prediction_data
    clickprobability = stats::predict(logit1, df, type="response")
    predictions = cbind(df,predicted_probability = round(clickprobability,2))
    sortedpredictions = predictions[order(predictions$predicted_probability, decreasing = TRUE),]
    suppressWarnings(res002 <- try(utils::write.csv(sortedpredictions, file ="! Results_Logistic_Predictions.csv", row.names = FALSE), silent = TRUE))
  }
  
  
  # ### titanic example
  # 
  # # A different example: Survivors of the Titanic
  # dd = read.csv(file="titanic.csv")
  # head(dd)
  # str(dd)
  # 
  # # drop the names column
  # dd = dd[ ,-3]
  # head(dd)
  # 
  # # Let us first do a model-free descriptive
  # # analysis on some of the variables:
  # 
  # ft = table(dd$Survived,dd$Sex)
  # barplot(ft[2 ,], col="cyan", main = "Survival by Sex")
  # 
  # ft = table(dd$Survived,dd$PassengerClass)
  # barplot(ft[2 ,], col="maroon", main = "Survival by Passenger Class")
  # 
  # 
  # # Estimate the logistic regression model and show its summary
  # 
  # titanic_Model = glm(Survived ~ ., data = dd, family = "binomial")
  # jtools::summ(titanic_Model)
  # 
  # # What do you think about these results?
  # # What other insights do these results provide?
  
  
  
  
  
  ################# save results in a pdf file ############
  
  # Set up some parameters
  # Decide what to call your file name; make sure to put .pdf at the end of the name
  filename =  "! Results_Logistic_Analysis.pdf"
  larger =  1
  # grDevices::pdf(filename, height=larger*8.5, width=larger*11)
  suppressWarnings(res00 <- try(grDevices::pdf(filename, height=larger*8.5, width=larger*11), silent = TRUE))
  
  
  gridExtra::grid.arrange(top="\n\n\n\n\n\n View the first 10 rows of the data set \n a max. of 14 columns can be shown here.", gridExtra::tableGrob(dsample, theme=mytheme))
  gridExtra::grid.arrange(top="\n\n\n\n\n\n Summary Statistics for the variables \n Page 1 of 2", gridExtra::tableGrob(ds[,1:firsthalf], theme=mytheme))
  gridExtra::grid.arrange(top="\n\n\n\n\n\n Summary Statistics for the variables \n Page 2 of 2", gridExtra::tableGrob(ds[,(secondhalf:fullcols)], theme=mytheme))
  # graphics::pairs(numcols, lower.panel = NULL, pch=16,cex=0.3)
  correlations = stats::cor(numcols)
  corrplot::corrplot(correlations, method="pie")
  corrplot::corrplot(correlations, method="number")
  
  
  
  gridExtra::grid.arrange(top="\n Logistic Regression Results: \n\n Coefficient Estiamtes", gridExtra::tableGrob(coeftable1, theme=mytheme))
  
  
  # jtools::export_summs(m1, m2, m3, m4, m5, m6)
  jtools::plot_summs(logit1, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)
  
  
  # # Display the best model
  # jtools::summ(selected_model)
  # 
  # # Display the best model visually
  # jtools::plot_summs(selected_model, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .95)
  
  # Model Diagnostics for the Best model
  # selected_model = stats::step(m6)
  # utils::capture.output({
  #   selected_model = stats::step(m6)
  # })
  
  
  
  utils::capture.output({
    
    grDevices::dev.off()
  })
  
  
  if (!is.null(res00)){
    cat("\n ERROR:\n Logistic Rgression analysis was performed, but we were not able to\n save the results in \"! Results_Logistic_Analysis.pdf\"")
    cat(" \n This is probably due to a PDF file with the same name being open.\n")
    cat(" Make sure you close that file, and then run the previous line of code again.")
    
  } else {
    cat("\n Logistic Regression analysis has been performed on these data!\n")
    cat("\n Results have been saved in a file named: \"! Results_Logistic_Analysis.pdf\"\n")
    cat(" You can find this file in the same folder as your data files, which is here:\n ")
    cat(as.character(getwd()))
    cat(" \n\n ")
    cat(" If you see any warnings below, simply disregard them.\n\n ")
    
  }
  
  if (typeof(prediction_data)!="double"){
    if (!is.null(res002)){
      cat("\n ERROR:\n Prediction for the new data set was also performed,\n but we were not able to save the results in:\n \"! Results_Logistic_Predictions.csv\"")
      cat(" \n This is probably due to a CSV file with the same name being open.\n")
      cat(" Make sure you close that file, and then run the previous line of code again.")
      
    } else {
      cat("\nPrediction for the new data set has been performed!\n")
      cat("\n Results have been saved in a file named: \"! Results_Logistic_Predictions.csv\"\n")
      cat(" You can find this file in the same folder as your data files, which is here:\n ")
      cat(as.character(getwd()))
      cat(" \n\n ")
      cat(" If you see any warnings below, simply disregard them.\n\n ")
      
    }
  }
  
}
