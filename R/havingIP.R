#' havingIP Function
#'
#'
#' Function to test if the internet connection is available
#'
#' @param operational_system The operational system.
#'
#' @value A logical vector TRUE if internet connection is available.
#'
#' @examples
#' internet <- havingIP()
#'
#' @export
#'
#'
havingIP <- function(operational_system) {

  if(missing(operational_system)){
    operational_system <- "windows"
    }

  if (.Platform$OS.type == operational_system) {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"

  return(any(grep(validIP, ipmessage)))
}
