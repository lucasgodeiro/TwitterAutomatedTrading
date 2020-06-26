#' Start_Trading
#'
#' This function starts the Algorithm and sends the ordes to txt file that will be read for the Expert Advisor in the Metatrader 5.
#'
#'
#'
#' @param consumer_key Api Twitter Consumer Key
#' @param consumer_secret Api Twitter Consumer Secret
#' @param access_token Api Twitter access token
#' @param access_secret Api Twitter access secret
#' @param path_decision The path where the txt file with the decision will be saved. Generally it is saved in the 'Common' file at Metaquotes folder(see vignette for instructions).
#' @param ntweets see get_sentiment_tweets.
#' @param terms_list see get_sentiment_tweets.
#' @param time_tweet see get_sentiment_tweets.
#' @param time_zone see get_sentiment_tweets.
#' @param positive_dictionary see get_sentiment_tweets.
#' @param negative_dictionary see get_sentiment_tweets.
#' @param stock_symbol see get_sentiment_Stocktwits.
#' @param path_twits see get_sentiment_Stocktwits.
#' @param Operation_Hours1 The operation hours 1 for day trade. TRUE or FALSE.
#' @param Operation_Hours2 The operation hours 2 for day trade. TRUE or FALSE.
#' @param Operation_Hours3 The operation hours 3 for day trade. TRUE or FALSE.
#' @param start_time1 The start time 1 for day trade.
#' @param start_time2 The start time 2 for day trade.
#' @param start_time3 The start time 3 for day trade.
#' @param end_time1 The end time 1 for day trade.
#' @param end_time2 The end time 2 for day trade.
#' @param end_time3 The end time 3 for day trade.
#' @param Day_Trade True for Day Trade. False for Swing Trade.
#' @param nap_time_error The time that the EA should take a nap in case of error.
#' @param initial_time The start of operation.
#' @param final_time The time which the position in day trade mode must be closed.
#' @param freq_trade The time in minutes the EA must recompute the sentiment index and take a decision.
#' @param w_twitter The weight of the twitter sentiment index.
#' @param w_stocktwits The weight of the stocktwits sentiment index.
#' @param Sentiment_Index_Threshold see trade_decision function.
#' @param Use_Delta_Sentiment see trade_decision function
#' @param Signal_File_Name The Signal File Name.
#'
#' @importFrom naptime naptime
#' @importFrom utils write.table
#'
#' @return The functions just activate the algorithm.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Signal_File_Name <- 'Signal.txt'
#' ntweets <- 5000
#' time_tweet <- 6
#' terms_list <- c("IBOVESPA OR bovespa OR ibov OR petroleo OR $SPX OR $SPY OR $EWZ")
#' time_zone <- "Brazil/East"
#' positive_dictionary <- my_dictionary[['positive_terms']]
#' negative_dictionary <- my_dictionary[['negative_terms']]
#'
#' path_twits <- 'your path'
#' stock_symbol <- c("EWZ", "SPX", "SPY", "USO")
#' time_zone <- "Brazil/East"
#'
#' consumer_key <- "your consumer_key"
#' consumer_secret <- "your consumer_secret"
#' access_token <- "your access token"
#' access_secret <- " your access secret "
#' nap_time_error <- 7.7
#' path_decision <- 'metatrader txt file path'
#' path_twits <- 'your path'
#' initial_time <- 9
#' final_time <- 17
#' freq_trade <- 10
#' Day_Trade <- TRUE
#' Operation_Hours1 <- TRUE
#' start_time1 <- 9
#' end_time1 <- 17
#' w_twitter <- 0.9
#' w_stocktwits <- 0.1
#' Sentiment_Index_Threshold <- 0.5
#'
#'
#' Start_Trading(consumer_key = consumer_key,
#'              consumer_secret = consumer_secret,
#'              access_token = access_token,
#'              access_secret = access_secret,
#'              path_decision = path_decision,
#'              ntweets = ntweets,
#'              terms_list = terms_list,
#'              time_tweet = time_tweet,
#'              time_zone = time_zone,
#'              positive_dictionary = positive_dictionary,
#'              negative_dictionary = negative_dictionary,
#'              stock_symbol = stock_symbol,
#'              path_twits = path_twits,
#'              Operation_Hours1 = TRUE,
#'              Operation_Hours2 = FALSE,
#'              Operation_Hours3 = FALSE,
#'              start_time1 = start_time1,
#'              start_time2 = start_time1,
#'              start_time3 = start_time1,
#'              end_time1 = end_time1,
#'              end_time2 = end_time1,
#'              end_time3 = end_time1,
#'              Day_Trade = TRUE,
#'              nap_time_error = nap_time_error,
#'              initial_time = initial_time,
#'              final_time = final_time,
#'              freq_trade = freq_trade,
#'              w_twitter = w_twitter,
#'              w_stocktwits = w_stocktwits,
#'              Sentiment_Index_Threshold = Sentiment_Index_Threshold,
#'              Use_Delta_Sentiment = TRUE,
#'              Signal_File_Name = Signal_File_Name)
#' }
#'
#'
Start_Trading <- function(consumer_key,
                          consumer_secret,
                          access_token,
                          access_secret,
                          path_decision,
                          ntweets,
                          terms_list,
                          time_tweet,
                          time_zone,
                          positive_dictionary,
                          negative_dictionary,
                          stock_symbol,
                          path_twits,
                          Operation_Hours1,
                          Operation_Hours2,
                          Operation_Hours3,
                          start_time1,
                          start_time2,
                          start_time3,
                          end_time1,
                          end_time2,
                          end_time3,
                          Day_Trade,
                          nap_time_error,
                          initial_time,
                          final_time,
                          freq_trade,
                          w_twitter,
                          w_stocktwits,
                          Sentiment_Index_Threshold,
                          Use_Delta_Sentiment,
                          Signal_File_Name){

  setup_twitter_oauth(consumer_key = consumer_key,
                      consumer_secret = consumer_secret,
                      access_token = access_token,
                      access_secret = access_secret)
  hour_freq <- generate_trade_frequency(initial_time = initial_time
                                        ,final_time = final_time,
                                        freq_trade = freq_trade)


  buy_sell_t1 <- 0
  decision <- "BUY IT NOW"
  path_decision <- path_decision




  repeat {
    tryCatch(

      expr = {

        internet <- havingIP()
        candlestick <- check_frequency(hours_frequency = hour_freq,
                                       time_zone = time_zone)

        print('Candlestick Control')
        print(candlestick)


        if(internet == TRUE && candlestick == TRUE) {
          print('Computing Twitter Sentiment Index')

          ###### Compute sentiment index #########################
          sentiment_index <- get_sentiment_tweets(ntweets = ntweets,
                                                  terms_list = terms_list,
                                                  time_tweet = time_tweet,
                                                  time_zone = time_zone,
                                                  positive_dictionary = positive_dictionary,
                                                  negative_dictionary = negative_dictionary)

          ###############################################################

          twitter_index <- sentiment_index[[1]]
          print('Twitter Sentiment Index')
          print( round(twitter_index,2))
          print('Computing Stocktwits Sentiment Index')
          suppressWarnings(
            stocktwits_index <- get_sentiment_stocktwits(stock_symbol = stock_symbol,
                                                         path_twits = path_twits)
          )
          print('Stocktwits Sentiment Index')
          print(round(stocktwits_index,2))

          w_index <- (w_stocktwits * stocktwits_index) + (w_twitter * twitter_index)
          buy_sell_t <-   round(w_index ,2)


          h=Sys.time()
          hh=paste(toString(round(buy_sell_t,2)), toString(round(buy_sell_t1,2)),h)
          print(hh)
          #tweet(hh)
          #buy_sell_dolar=LG_trade_DOLAR(ini=6,fin=24,tresh=0.15)
          #hh1=paste(buy_sell_dolar,h)
          # print(hh1)
          #tweet(hh1)


          ############Here define the operations hours #######################

          if(missing(Day_Trade)){
            Day_Trade <- FALSE
          }

          if(missing(Operation_Hours1)){
            Op_Hours1 <- TRUE
          }


          if(missing(Operation_Hours2)){
            Op_Hours2 <- TRUE
          }

          if(missing(Operation_Hours3)){
            Op_Hours3 <- TRUE
          }


          if(missing(start_time1)){
            start_time1 <- 0
          }


          if(missing(start_time2)){
            start_time2 <- 0
          }

          if(missing(start_time3)){
            start_time3 <- 0
          }





          if(missing(end_time1)){
            end_time1 <- 0
          }


          if(missing(end_time2)){
            end_time2 <- 0
          }

          if(missing(end_time3)){
            end_time3 <- 0
          }

          if(Day_Trade == FALSE){

            Op_Hours1 <- Op_Hours2 <- Op_Hours3 <- TRUE


          } else if(Day_Trade == TRUE &  Operation_Hours1 == TRUE &  Operation_Hours2 == TRUE & Operation_Hours3 == TRUE){
            Op_Hours1 <- operation_hours(start_time = start_time1,
                                         end_time = end_time1,
                                         time_zone = time_zone)
            Op_Hours2 <- operation_hours(start_time = start_time2,
                                         end_time = end_time2,
                                         time_zone = time_zone)

            Op_Hours3 <- operation_hours(start_time = start_time3,
                                         end_time = end_time3,
                                         time_zone = time_zone)

          } else if(Day_Trade == TRUE &  Operation_Hours1 == TRUE &  Operation_Hours2 == TRUE & Operation_Hours3 == FALSE){
            Op_Hours1 <- operation_hours(start_time = start_time1,
                                         end_time = end_time1,
                                         time_zone = time_zone)
            Op_Hours2 <- operation_hours(start_time = start_time2,
                                         end_time = end_time2,
                                         time_zone = time_zone)

            Op_Hours3 <- FALSE



          } else if(Day_Trade == TRUE &  Operation_Hours1 == TRUE &  Operation_Hours2 == FALSE & Operation_Hours3 == FALSE){
            Op_Hours1 <- operation_hours(start_time = start_time1,
                                         end_time = end_time1,
                                         time_zone = time_zone)
            Op_Hours2 <- FALSE
            Op_Hours3 <- FALSE

          } else {

            Op_Hours1 <- FALSE
            Op_Hours2 <- FALSE
            Op_Hours3 <- FALSE

          }



          op_hours <- c(Op_Hours1, Op_Hours2, Op_Hours3)


          print('operation hours')
          print(op_hours)




          if( sum(op_hours) != 0 ) {
            print("Getting Decision")

            decision <- Trade_Decision(Current_Sentiment_Index = buy_sell_t,
                                       Past_Sentiment_Index = buy_sell_t1,
                                       Use_Delta_Sentiment =  Use_Delta_Sentiment,
                                       Sentiment_Index_Threshold = Sentiment_Index_Threshold,
                                       past_decision = decision)
            print('Decision')
            print(decision)

          } else  {
            print('Closing Positions')
            decision <- Close_Position(actual_decision = decision)
          }


          print(decision)

          file_decision <- paste0(path_decision, Signal_File_Name)

          decision1 = "STOP"
          write.table(decision,
                      row.names=FALSE,quote=FALSE,col.names=FALSE,file = file_decision)

          Sys.sleep(1)

          write.table(decision1,
                      row.names=FALSE,quote=FALSE,col.names=FALSE,file = file_decision)

          buy_sell_t1 <- buy_sell_t



        } else {
          naptime(1)
        }
      },
      error = function(e){
        ####### Close position
        print('Some error is going on')
        decision <- Close_Position(actual_decision = decision)


        file_decision <- paste0(path_decision, Signal_File_Name)


        decision1 = "STOP"
        write.table(decision,
                    row.names=FALSE,quote=FALSE,col.names=FALSE,file = file_decision)

        Sys.sleep(1)

        write.table(decision1,
                    row.names=FALSE,quote=FALSE,col.names=FALSE,file = file_decision)

        naptime(60* nap_time_error)
      }
    )
  }






}


