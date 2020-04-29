#' get_sentiment_stocktwits
#'
#'
#' This function computes the sentiment based on bullish and bearish tag from stocktwits using the last 30 twits.
#'
#' @param stock_symbol A vector with the stocks symbols.
#' @param path_twits The path where the Json files will be stored.
#' @param sentiment_index_type The sentiment type to be used according to the dictionary, positive, negative or both. Default is both, positive and negative
#'
#' @importFrom curl curl_download
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_data_frame
#' @importFrom tibble tibble
#' @importFrom plyr rbind.fill
#'
#' @return A numeric value with the value of the sentiment index.
#' @export
#'
#' @exmples
#' \donttest{
#' path_twits <- 'C:/Users/Cliente/Dropbox/Fun/Stocktwits/'
#'   symbols <- c("EWZ", "SPX", "SPY", "USO")
#'
#'  stocktwits_index <- get_sentiment_stocktwits(stock_symbol = symbols,
#'  path_twits = path_twits)
#'    }
#'


get_sentiment_stocktwits <- function(stock_symbol,
                                     path_twits,
                                     sentiment_index_type){



  if(missing(sentiment_index_type)){
    sentiment_index_type <- 'both'
  }

  main_url <- 'https://api.stocktwits.com/api/2/streams/symbol/'
  full_url <- list()
  file_stock <- list()
  for(i in 1:length(stock_symbol)){
    full_url[[i]] <- paste0(main_url,stock_symbol[i],'.json')
    file_stock[[i]] <- paste0(path_twits, stock_symbol[i], '.json')
    #print(full_url[[i]])
  }




  for(i in 1:length(stock_symbol)){
    curl::curl_download(full_url[[i]], destfile = file_stock[[i]])
  }


  data_twits_new1 <- list()
  for(i in 1:length(stock_symbol)){
    data_twits <- fromJSON(file_stock[[i]])
    data_twits_tbl <- as_data_frame(data_twits[[4]])
    data_twits_new <- dplyr::select(data_twits_tbl, body, entities, symbols )
    data_twits_new1[[i]] <- tibble::tibble(body = data_twits_new$body, sentiment = data_twits_new$entities$sentiment)
    data_twits_new1[[i]]$sentiment <- as.matrix(data_twits_new1[[i]]$sentiment)
  }



  data_twits_bind <- do.call(rbind.fill, data_twits_new1)
  data_twits_unique <- unique(data_twits_bind)
  #print(nrow(data_twits_unique))

  bb <- data_twits_unique$sentiment
  bb[is.na(bb)] = 0


  idx_bull  = bb == "Bullish"
  idx_bull <- sum(idx_bull)

  idx_bear  = bb == "Bearish"
  idx_bear <- sum(idx_bear)


  if(sentiment_index_type == 'positive'){
  stocktwits_sentiment_index <- (idx_bull ) / ( 1 + ( idx_bull + idx_bear))
  } else if(sentiment_index_type == 'negative'){
    stocktwits_sentiment_index <- ( idx_bear) / ( 1 + ( idx_bull + idx_bear))
  } else {
    stocktwits_sentiment_index <- (idx_bull - idx_bear) / ( 1 + ( idx_bull + idx_bear))
    }

  return(stocktwits_sentiment_index)
}
