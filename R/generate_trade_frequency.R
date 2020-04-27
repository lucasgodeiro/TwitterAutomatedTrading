#' generate_trade_frequency function
#'
#'
#'
#'
#' @param initial_time The time the algorithm starts trading.
#' @param final_time The time the algorithm ends trading.
#' @param freq_trade The frequency which the algorithm recalculates the sentiment index.
#'
#' @return A vector containing the hours of operation.
#' @export
#'
#' @examples
#' hours_candle_10 <- generate_trade_frequency(9,17,10)
#' #For example, for 17:30, you should use minutes/60, i.e. 17.5
#' hours_candle_20 <- generate_trade_frequency(9,17.5,10)
generate_trade_frequency <- function(initial_time, final_time,freq_trade){
  hours_candlestick <- seq(from = initial_time, to = final_time, by = 1)
  minutes_candlestick <- seq(from = 0, to = 60, by = freq_trade)/60
  hours_candles_lst <-  list()

  for(i in 1:length(hours_candlestick)){
    hours_candles_lst[[i]] <- rep(hours_candlestick[i], length(minutes_candlestick)) + minutes_candlestick
  }
  hours_candles_new <- unique(unlist(hours_candles_lst))
  idx_ini <- which(hours_candles_new == initial_time)
  idx_fin <- which(hours_candles_new == final_time)
  hours_candles_new <- hours_candles_new[idx_ini:idx_fin]
  return(hours_candles_new)
}


