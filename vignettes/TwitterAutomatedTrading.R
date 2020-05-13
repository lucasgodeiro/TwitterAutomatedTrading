## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(TwitterAutomatedTrading)

## ----get_sentiment_tweets example, eval = FALSE-------------------------------
#  ntweets <- 500
#  time_tweet <- 6
#  terms_list <- c("IBOVESPA OR bovespa OR ibov OR petroleo OR $SPX OR $SPY OR $EWZ")
#  time_zone <- "Brazil/East"
#  positive_dictionary <- my_dictionary[['positive_terms']]
#  negative_dictionary <- my_dictionary[['negative_terms']]
#  sentiment_index <- get_sentiment_tweets(ntweets = ntweets,
#  terms_list = terms_list,
#  time_tweet = time_tweet,
#  time_zone = time_zone,
#  positive_dictionary = positive_dictionary,
#  negative_dictionary = negative_dictionary
#  )
#  
#  sent_idx <- sentiment_index[[1]]
#  sent_wrd <- sentiment_index[[2]]
#  sent_pos <- sentiment_index[[3]]
#  sent_neg <- sentiment_index[[4]]

## ----Start_Trading, eval=FALSE------------------------------------------------
#  Signal_File_Name <- 'Signal.txt'
#  ntweets <- 5000
#  time_tweet <- 6
#  terms_list <- c("IBOVESPA OR bovespa OR ibov OR petroleo OR $SPX OR $SPY OR $EWZ")
#  time_zone <- "Brazil/East"
#  positive_dictionary <- my_dictionary[['positive_terms']]
#  negative_dictionary <- my_dictionary[['negative_terms']]
#  
#  path_twits <- tempdir()
#  stock_symbol <- c("EWZ", "SPX", "SPY", "USO")
#  time_zone <- "Brazil/East"
#  
#  consumer_key <- "your consumer_key"
#  consumer_secret <- "your consumer_secret"
#  access_token <- "your access token"
#  access_secret <- " your access secret "
#  nap_time_error <- 7.7
#  path_decision <- tempdir()
#  path_twits <- 'your path'
#  initial_time <- 9
#  final_time <- 17
#  freq_trade <- 10
#  Day_Trade <- TRUE
#  Operation_Hours1 <- TRUE
#  start_time1 <- 9
#  end_time1 <- 17
#  w_twitter <- 0.9
#  w_stocktwits <- 0.1
#  Sentiment_Index_Threshold <- 0.5
#  
#  
#  Start_Trading(consumer_key = consumer_key,
#               consumer_secret = consumer_secret,
#               access_token = access_token,
#               access_secret = access_secret,
#               path_decision = path_decision,
#               ntweets = ntweets,
#               terms_list = terms_list,
#               time_tweet = time_tweet,
#               time_zone = time_zone,
#               positive_dictionary = positive_dictionary,
#               negative_dictionary = negative_dictionary,
#               stock_symbol = stock_symbol,
#               path_twits = path_twits,
#               Operation_Hours1 = TRUE,
#               Operation_Hours2 = FALSE,
#               Operation_Hours3 = FALSE,
#               start_time1 = start_time1,
#               start_time2 = start_time1,
#               start_time3 = start_time1,
#               end_time1 = end_time1,
#               end_time2 = end_time1,
#               end_time3 = end_time1,
#               Day_Trade = TRUE,
#               nap_time_error = nap_time_error,
#               initial_time = initial_time,
#               final_time = final_time,
#               freq_trade = freq_trade,
#               w_twitter = w_twitter,
#               w_stocktwits = w_stocktwits,
#               Sentiment_Index_Threshold = Sentiment_Index_Threshold,
#               Use_Delta_Sentiment = TRUE,
#               Signal_File_Name = Signal_File_Name
#               )

