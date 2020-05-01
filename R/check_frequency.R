#' check_frequency function
#'
#'
#' This functions checks if the EA can send order to the plataform trading.
#'
#' @param hours_frequency The vector containing the hours of operations.
#' @param time_zone The time zone.
#'
#' @return A logical vector TRUE if the EA can compute the sentiment.
#' @import lubridate
#' @export
#'
#' @examples
#' time_zone <- "Brazil/East"
#' hour_freq <- generate_trade_frequency(9,17,10)
#' check_freq <- check_frequency(hours_frequency = hour_freq,
#'                              time_zone = time_zone)
check_frequency <- function(hours_frequency,time_zone){
  current_hour <- lubridate::hour(as.POSIXlt(Sys.time(), time_zone))
  current_minute <- lubridate::minute(as.POSIXlt(Sys.time(), time_zone))
  current_time <- current_hour + current_minute/60

  current_candle <- sum(current_time == hours_frequency)

  if(current_candle == 1  ){
    chk_candle <- TRUE
  } else {

    chk_candle <- FALSE
  }

  return(chk_candle)
}
