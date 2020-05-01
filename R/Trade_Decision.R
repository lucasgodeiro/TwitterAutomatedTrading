#' Trade_Decision
#'
#'
#' This function takes as arguments the sentiment indexes and returns the decision.
#'
#' @param Current_Sentiment_Index The current sentiment index
#' @param Past_Sentiment_Index The sentiment index in (t-1)
#' @param Use_Delta_Sentiment If True the fuction will consider the difference in the sentiment index in the decision.
#' @param Sentiment_Index_Threshold The threshold to define if the decision will be following or against the sentiment.
#' @param past_decision The last trade decision.
#'
#' @return The vector with the decision.
#' @export
#'
#' @examples
#'
#' buy_sell_t1 <- 0.2
#' buy_sell_t <- 0.5
#' Use_Delta_Sentiment <- TRUE
#' Sentiment_Index_Threshold <- 0.5
#'
#' decision <- Trade_Decision(Current_Sentiment_Index = buy_sell_t,
#'                           Past_Sentiment_Index = buy_sell_t1,
#'                           Use_Delta_Sentiment =  Use_Delta_Sentiment,
#'                           Sentiment_Index_Threshold = Sentiment_Index_Threshold,
#'                           past_decision = decision
#' )
#'
#'
#'
Trade_Decision <- function(Current_Sentiment_Index ,
                           Past_Sentiment_Index,
                           Use_Delta_Sentiment,
                           Sentiment_Index_Threshold,
                           past_decision) {

  if(abs(Current_Sentiment_Index) >= 0 & abs(Current_Sentiment_Index) <= Sentiment_Index_Threshold ){
    trend = "yes"
  } else {
    trend = "no"
  }
  print(trend)

  if(missing(past_decision)){
    past_decision <- 'HOLD IT NOW'
    }


  if(Use_Delta_Sentiment == TRUE) {


    buy_sell_t <- Current_Sentiment_Index
    buy_sell_t1 <- Past_Sentiment_Index

    if(trend == "yes") {
      if(buy_sell_t  > buy_sell_t1 ){
        decision = "BUY IT NOW"
      }
      if(buy_sell_t == buy_sell_t1){
        decision = past_decision            }
      if(buy_sell_t < buy_sell_t1){
        decision ="SELL IT NOW"
      }
    } else {

      buy_sell_t <- Current_Sentiment_Index
      buy_sell_t1 <- Past_Sentiment_Index

      if(buy_sell_t  > buy_sell_t1 ){
        decision = "SELL IT NOW"
      }
      if(buy_sell_t == buy_sell_t1){
        decision = past_decision
      }
      if(buy_sell_t < buy_sell_t1){
        decision ="BUY IT NOW"
      }
    }
  } else {



    buy_sell_t <- Current_Sentiment_Index
    buy_sell_t1 <- 0

    if(trend == "yes") {
      if(buy_sell_t  > buy_sell_t1 ){
        decision = "BUY IT NOW"
      }
      if(buy_sell_t == buy_sell_t1){
        decision = past_decision            }
      if(buy_sell_t < buy_sell_t1){
        decision ="SELL IT NOW"
      }
    } else {

      buy_sell_t <- Current_Sentiment_Index
      buy_sell_t1 <- Past_Sentiment_Index

      if(buy_sell_t  > buy_sell_t1 ){
        decision = "SELL IT NOW"
      }
      if(buy_sell_t == buy_sell_t1){
        decision = past_decision
      }
      if(buy_sell_t < buy_sell_t1){
        decision ="BUY IT NOW"
      }


    }




  }
  print(decision)
  return(decision)

}

