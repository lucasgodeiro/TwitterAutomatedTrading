#' operation_hours
#'
#' This function defines the operations hours of the EA.
#'
#' @param start_time The time that the EA should start to trade.
#' @param end_time The time that the EA should stop to trade and close the open positions.
#' @param time_zone The time zone.
#'
#'
#' @import lubridate
#'
#' @return A logical variable TRUE if the Expert Advisor can trade.
#' @export
#'
#' @examples
#' time_zone <- "Brazil/East"
#' op_hours<- operation_hours(start_time = 9.5,
#' end_time = 17,
#' time_zone = time_zone)
#'
#'
#'
#'
#'
operation_hours <- function(start_time,
                            end_time,
                            time_zone){
  current_hour <- lubridate::hour(as.POSIXlt(Sys.time(), time_zone))
  current_minute <- lubridate::minute(as.POSIXlt(Sys.time(), time_zone))
  current_time <- current_hour + current_minute/60
  if(current_time > start_time && current_time < end_time ){
    chk_time <- TRUE
  } else {

    chk_time <- FALSE
  }
  print(chk_time)
  return(chk_time)
}
