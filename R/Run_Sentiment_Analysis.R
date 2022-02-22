####################################################
#        Sentiment Analysis                         #
####################################################
#    Last updated: 11/23/2021                       #
####################################################


#' Sentiment Analysis
#'
#' This function conducts a Sentiment analysis on data from a csv file, comprised of one column of text. Each cell in that column  must be an independent tweet, review, or document.
#' 
#' @param mydata A .csv file to be analyzed. Each line must be a separate text: a tweet, review, or document.
#' @param words2remove A vector of words to remove from the analysis because of their irrelevance.
#' @param stemthis  To stem the documents or not to stem: default = FALSE, you can set it to TRUE
#' @param wcmf Word Cloud Minimum Frequency: default = 5. Words with frequencies lower than this amount will not be displayed in the word cloud
#' @param mostfrequent Number of most frequent words to display, default = 25
#' @export
#' 
#' @examples
#' \dontrun{
#' # You need to have one csv file containing one columne of text.
#' # Each cell in this file will be a tweet, review, or document...
#' # In Excel, press CTRL/Command H, to open the search and replace diaglog.
#' # With your cursor in the Find What box, press CTRL/Command J, then click Replace All.
#' # The above steps search for and replace all line breaks in your document which is
#' # necessary as a preparation step for this analysis.
#' # The next few lines prepare and run the analysis.
#' # This is the sample code to be copied and used in a new R Script:
#' library(listentodata)
#' clear_console()
#' mydata = load_csv_data()
#' words2remove = c("toremvoe1", "toremove2")
#' stemthis = FALSE
#' wcmf = 5 # Word Cloud Minimum Frequency: default = 5
#' mostfrequent = 25
#' Run_Sentiment_Analysis(mydata, stemthis, wcmf, mostfrequent)
#' }
Run_Sentiment_Analysis = function(mydata, words2remove, stemthis= FALSE, wcmf=5, mostfrequent = 25) {
  
  
  # Read the text file from local machine , choose file interactively
  # T:\MarketingAnalytics\marketing_analytics - Copy\data for sentiment
  
  # # filename = file.choose(new = FALSE)
  # # test <- readLines(filename)
  # 
  # textdata <- load_csv_data()
  larger = 1.2
  textdata = mydata
  firstone = as.vector(names(textdata))
  firstone = gsub("\\.", " ", firstone) 
  textdata = as.vector(unlist(textdata))
  textdata = c(firstone, textdata)
  
  # replace line breaks

    for (i in 1:length(textdata)){
    textdata[i] = gsub("\r?\n|\r", " ", textdata[i])
    
  }

  # words2remove = c("toremvoe1", "toremove2")
  # stemthis = FALSE
  # wcmf = 5 # Word Cloud Minimum Frequency: default = 5
  # mostfrequent = 25

  
  mfwords = mostfrequent
  # Load the data as a corpus
  TextDoc <- tm::Corpus(tm::VectorSource(textdata))
  
  #Replacing "/", "@" and "|" with space
  toSpace <- tm::content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, toSpace, "/"))
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, toSpace, "@"))
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, toSpace, "\\|"))
  # Convert the text to lower case
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::content_transformer(tolower)))
  # Remove numbers
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::removeNumbers))
  # Remove english common stopwords
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::removeWords, tm::stopwords("english")))
  
  
  # List standard English stop words
  # print(stopwords("en"))
  
  # Remove your own stop word
  # specify your custom stopwords as a character vector
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::removeWords, words2remove))
  # Remove punctuations
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::removePunctuation))
  # Eliminate extra white spaces
  suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::stripWhitespace))
  # Text stemming - which reduces words to their root form
  
  if (stemthis==TRUE) {
    suppressWarnings(TextDoc <- tm::tm_map(TextDoc, tm::stemDocument))
  }
  
  
  
  
  # Build a term-document matrix
  TextDoc_dtm <- tm::TermDocumentMatrix(TextDoc)
  dtm_m <- as.matrix(TextDoc_dtm)
  # Sort by descending value of frequency
  dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
  dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
  # Display the top 20 most frequent words
  # head(dtm_d, 20)
  
  
  # Plot the most frequent words
  oursubset = dtm_d[1:mfwords,]
  oursubset = oursubset[order(oursubset$freq, decreasing = FALSE),]
  myx = oursubset$freq
  myy = oursubset$word
  
 
  # #generate word cloud
  # set.seed(1234)
  # wordcloud::wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = wcmf,
  #                      max.words=100, random.order=FALSE, rot.per=0.40,
  #                      colors=RColorBrewer::brewer.pal(8, "Dark2"))
  # 
   # NEXT part moved to pdf
  # graphics::par(oma=c(3,3,3,3)) # all sides have 3 lines of space
  # graphics::barplot(myx, las = 2, names.arg = myy,
  #         col =grDevices::rainbow(50), main =paste("Top", mfwords, "most frequent words"),
  #         # ylab = "Word frequencies",
  #         horiz = TRUE)
  # 

  
  # # Find associations 
  # tm::findAssocs(TextDoc_dtm, terms = c("film","story"), corlimit = 0.3)			
  
  
  # 
  # # Find associations for words that occur at least 50 times
  # tm::findAssocs(TextDoc_dtm, terms = tm::findFreqTerms(TextDoc_dtm, lowfreq = 100), corlimit = 0.4)
  
  test = stringi::stri_trans_general(TextDoc$content, "latin-ascii")
  TextDoc$content = test
  cleanedText = data.frame(TextDoc$content)
  names(cleanedText) ="Cleaned Documents"
  
  suppressWarnings(res01 <- try(utils::write.csv(cleanedText, file="! Results_cleaned_documents.csv", row.names = FALSE)))
  
  # # regular sentiment score using get_sentiment() function and method of your choice
  # # please note that different methods may have different scales
  # syuzhet_vector <- syuzhet::get_sentiment(cleaned, method="syuzhet")
  # # see the first row of the vector
  # head(syuzhet_vector)
  # # see summary statistics of the vector
  # summary(syuzhet_vector)
  # 
  # 
  # # bing
  # bing_vector <- syuzhet::get_sentiment(cleaned, method="bing")
  # head(bing_vector)
  # summary(bing_vector)
  # #affin
  # afinn_vector <- syuzhet::get_sentiment(cleaned, method="afinn")
  # head(afinn_vector)
  # summary(afinn_vector)
  
  #affin
  NRC_sentiment_scores <- syuzhet::get_sentiment(cleanedText, method="nrc")
  # head(nrc_vector)
  # summary(nrc_vector)
  
  # #compare the first row of each vector using sign function
  # sents = data.frame(rbind(
  #   sign(head(syuzhet_vector)),
  #   sign(head(bing_vector)),
  #   sign(head(afinn_vector)),
  #   sign(head(nrc_vector))
  # ))
  # sents
  
  scores = data.frame(textdata,NRC_sentiment_scores)
  scores = scores[order(scores$NRC_sentiment_scores, decreasing = TRUE),]
  # utils::write.csv(scores, file="! Results_sentiment_scores.csv", row.names = FALSE)
  
  
  # run nrc sentiment analysis to return data frame with each row classified as one of the following
  # emotions, rather than a score: 
  # anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
  # It also counts the number of positive and negative emotions found in each row
  d<-syuzhet::get_nrc_sentiment(TextDoc$content)
  # head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
  # head (d,10)
  
  
  
  #transpose
  td<-data.frame(t(d))
  
  
  #The function rowSums computes column sums across rows for each level of a grouping variable.
  td_new <- data.frame(rowSums(td))
  #Transformation and cleaning
  names(td_new) <- "counts"
  sentiment = rownames(td_new)
  td_new <- cbind(sentiment, td_new)
  rownames(td_new) <- NULL
  td_new2<-td_new[1:nrow(td_new),]
  # #Plot One - count of words associated with each sentiment
  # # Plot Outputs commented out for the function
  # ggplot2::quickplot(sentiment, data=td_new2, weight=td_new2$counts, geom="bar", fill=sentiment, ylab="Count")
  
  DocumentNumber = paste("document_", seq(1:ncol(td)), sep='')
  tosave = data.frame(DocumentNumber,d)
  suppressWarnings(res02 <- try(utils::write.csv(tosave, file="! Results_sentiment_scores.csv", row.names = FALSE)))
  
  
  
  # #Plot two - count of words associated with each sentiment, expressed as a percentage
  # graphics::barplot(
  #   sort(colSums(prop.table(d[, 1:10]))), 
  #   horiz = TRUE, 
  #   cex.names = 0.7, 
  #   las = 1, 
  #   col = grDevices::rainbow(10), 
  #   main = "Emotions in Text", xlab="Percentage"
  # )
  # 
  # 
  ################# save results in a pdf file ############
  
  # Set up some parameters
  # Decide what to call your file name; make sure to put .pdf at the end of the name
  pdffilename =  "! Results_Sentiment_Analysis.pdf"
  # larger =  resizepaper
  # pdf(filename, height=larger*8.5, width=larger*11)
  
  suppressWarnings(res03 <- try(grDevices::pdf(pdffilename, height=larger*8.5, width=larger*11), silent = TRUE))
  
  graphics::par(oma=c(3,3,3,3)) # all sides have 3 lines of space
  graphics::barplot(myx, las = 2, names.arg = myy, 
                    col =grDevices::rainbow(50), main =paste("Top", mfwords, "most frequent words"),
                    # ylab = "Word frequencies",
                    horiz = TRUE)
  
  
  # #generate word cloud
  set.seed(1234)
  wordcloud::wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = wcmf,
                       max.words=100, random.order=FALSE, rot.per=0.40,
                       colors=RColorBrewer::brewer.pal(8, "Dark2"))
  
  
  #Plot One - count of words associated with each sentiment
  ggplot2::quickplot(sentiment, data=td_new2, weight=td_new2$counts, geom="bar", fill=sentiment, ylab="Count")
  

  #Plot two - count of words associated with each sentiment, expressed as a percentage
  graphics::barplot(
    sort(colSums(prop.table(d[, 1:10]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    col = grDevices::rainbow(10), 
    main = "Percentage of Emotions in the Documents", xlab="Percentage"
  )
  
  
  grDevices::dev.off()
  
  
  
  if (!is.null(res01) | !is.null(res02) | !is.null(res03)){
    cat("\n ERROR:\n The analysis was performed, but we were not able to\n save some of the results in the relevant files on your computer.")
    cat(" \n This is probably due to a PDF or CSV result file with the same name being open.\n")
    cat(" Make sure you close all result files from your previous anlaysis, and then run the analysis again.")
    
  } else {
    cat("\n Sentiment Analyis has been performed on these data!\n")
    cat("\n 1. Main results have been saved in a file named: \"! Results_Sentiment_Analysis.pdf\"\n")
    cat("\n 2. Cleaned documents have been saved in a file named: \"! Results_cleaned_documents.csv\"\n")
    cat("\n 3. Cleaned documents have been saved in a file named: \"! Results_sentiment_scores.csv\"\n")
    
    cat(" You can find these files in the same folder as your data file, which is here:\n ")
    cat(as.character(getwd()))
    cat(" \n\n ")
    cat(" If you see any warnings below, simply disregard them.\n\n ")
    
  }
  
}
