#' Close_Position
#'
#'
#' This functions closes a open position.
#'
#' @param actual_decision The current position status("BUY IT NOW","SELL IT NOW", "SELL IT NOW CLOSE", "BUY IT NOW CLOSE" ).
#'
#' @return A vector with the new decision.
#' @export
#'
#' @examples
#' decision <- 'SELL IT NOW'
#' decision <- Close_Position(actual_decision = decision)
#'
#'
#'
Close_Position <- function(actual_decision){

  actual_decision_types <- c("BUY IT NOW","SELL IT NOW", "SELL IT NOW CLOSE", "BUY IT NOW CLOSE" )
  '%notin%' <- Negate('%in%')
  if(actual_decision %notin% actual_decision_types){
    stop('This is not a correct trade decision. ')
  }


  if( actual_decision == "BUY IT NOW") {
    new_decision = "SELL IT NOW CLOSE"
  }  else if( actual_decision == "SELL IT NOW") {
    new_decision <- "BUY IT NOW CLOSE"
  }  else if( actual_decision == "SELL IT NOW CLOSE") {
    new_decision <- "SELL IT NOW CLOSE"
  }  else if( actual_decision == "BUY IT NOW CLOSE") {
    new_decision <- "BUY IT NOW CLOSE"
  } else  {
    new_decision = "HOLD IT NOW CLOSE"
  }
  return(new_decision)
}

