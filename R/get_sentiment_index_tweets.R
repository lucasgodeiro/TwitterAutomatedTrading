#' get_sentiment_index_tweets
#'
#' @param ntweets Number of tweets to be searched
#' @param time_tweet Time in hours where the tweets will be filtered
#' @param terms_list Terms to be searched
#' @param time_zone The time zone
#' @param positive_dictionary The list of positive terms of the dictionary
#' @param negative_dictionary The list of negative terms of the dictionarya tibble with the words counting
#' @param sentiment_index_type  The sentiment type to be used according to the dictionary, positive, negative or both. Default is both, positive and negative
#' @param freq_tweets The frequency of the sentiment index in minutes.
#'
#' @return A list with: (1) - the sentiment index, (2) a tibble with the words counting, (3) a tibble with the negative words counting and (4) the positive and negative words in a xts object
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr tbl_df
#' @import twitteR
#' @importFrom purrr map_df
#' @importFrom tidytext unnest_tokens
#' @import magrittr
#' @import xts
#' @examples
#' \dontrun{
#' #Not run:
#' ntweets <- 500
#' time_tweet <- 6
#' terms_list <- c("IBOVESPA OR bovespa OR ibov OR petroleo OR $SPX OR $SPY OR $EWZ")
#' time_zone <- "Brazil/East"
#' positive_dictionary <- my_dictionary[['positive_terms']]
#' negative_dictionary <- my_dictionary[['negative_terms']]
#' freq_tweets <- '15 mins'
#' sentiment_index <- get_sentiment_tweets(ntweets = ntweets,
#' terms_list = terms_list,
#' time_tweet = time_tweet,
#' time_zone = time_zone,
#' positive_dictionary = positive_dictionary,
#' negative_dictionary = negative_dictionary,
#' freq_tweets = freq_tweets
#' )
#'
#' sent_idx <- sentiment_index[[1]]
#' sent_wrd <- sentiment_index[[2]]
#' sent_pos <- sentiment_index[[3]]
#' sent_neg <- sentiment_index[[4]]
#' }
#'

get_sentiment_index_tweets <- function(ntweets,
                                       time_tweet,
                                       terms_list,time_zone,
                                       positive_dictionary,
                                       negative_dictionary,
                                       sentiment_index_type,
                                       freq_tweets){
  n <- ntweets
  tm <- time_tweet*3600


  aa <- twitteR::searchTwitter(terms_list,n=n)
  aaa <- dplyr::tbl_df(purrr::map_df(aa, as.data.frame))


  if(missing(sentiment_index_type)){
    sentiment_index_type <- 'both'
  }





  #tweets_df=aaa
  tweets_df <- dplyr::bind_rows(aaa)
  tweets_df$created <- floor_date(tweets_df$created, freq_tweets)
  tweets_df$created <- format(tweets_df$created,tz = time_zone)
  st_date <- as.POSIXct(Sys.time()-tm)
  start_date <- format(st_date,tz= time_zone)
  e_date <- as.POSIXct(Sys.time())
  end_date <- format(e_date,tz= time_zone)
  tweets_df1 <- unique(tweets_df %>%
                         dplyr::filter(created >= start_date & created <= end_date)
  )

  tweets_df2 <- tweets_df1 %>%
    dplyr::filter(isRetweet == FALSE)




  words <- tweets_df2 %>%
    unnest_tokens(word,text)

  words <- words %>%
    anti_join(stop_words,by="word")

  words <- words %>%
    dplyr::filter(is.na(as.numeric(word)))


  words_daily <- words %>%
    dplyr::count(created,word)



  dtm <- words_daily %>%
    cast_dtm(created,word,n)

  dtm_m=as.matrix(dtm)


  data_words = dtm_m
  data_words_xts <- as.xts(ts(data_words))
  Dates=as.POSIXct(dtm$dimnames$Docs)
  index(data_words_xts) <- Dates




  LG_sent_score <- as.xts(ts(matrix(NA, nrow = nrow(data_words), ncol = 1)))
  index(LG_sent_score) <- Dates

  colnames(LG_sent_score) <- 'Twitter Sentiment Index'


  pos <-  positive_dictionary
  neg <- negative_dictionary


  idx_pos <- colnames(data_words_xts) %in%  pos
  idx_neg <- colnames(data_words_xts) %in%  neg

  pos_fil <- data_words_xts[,idx_pos]
  positive <-  as.xts(ts(apply(pos_fil , 1, sum)))
  index(positive) <- Dates
  colnames(positive) <- 'Sum of the Positive Words'

  neg_fil <- data_words_xts[,idx_neg]
  negative <-  as.xts(ts(apply(neg_fil , 1, sum)))
  index(negative) <- Dates
  colnames(negative) <- 'Sum of the Negative Words'



  if(sentiment_index_type == 'positive') {
    LG_sent_score = (positive)/(1 + (positive+negative) )
    LG_sent_score[is.na(LG_sent_score)] = 0
  } else if(sentiment_index_type == 'negative') {
    LG_sent_score=(negative)/(1 + (positive+negative) )
    LG_sent_score[is.na(LG_sent_score)] = 0
  } else {
    LG_sent_score=(positive-negative)/(1 + (positive+negative) )
    LG_sent_score[is.na(LG_sent_score)] = 0
  }

  colnames(LG_sent_score) <- 'Twitter Sentiment Index'

  outputs <- list(LG_sent_score , data_words_xts, pos_fil , neg_fil )
  return(outputs)
}
