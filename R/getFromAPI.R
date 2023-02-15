#' Herunterlade Daten von API
#'
#' \code{getFromAPI} Herunterlaedt die Daten von FDA von den Medikamenten in
#' den USA.
#'
#' Dies ist eine Funktion, die riesige Daten von allen Medikamenten in den USA
#'  liefert. Die Quelle ist \url{https://api.fda.gov/drug/ndc.json}. Diese
#'  Funktion haengt von \code{\link[httr]{GET}}
#' und \code{\link[jsonlite]{fromJSON}} ab.
#'
#' @return Ein data frame als tibble, das die Daten von \code{startYear} bis
#' \code{endYear} enthaelt. Die Anzahl von Daten ist \code{amount}.
#'
#' @param startYear Das erste Jahr
#' @param endYear Das letzte Jahr
#' @param amount Die Anzahl von Faellen, maximum 1000
#'
#' @examples
#' getFromAPI()
#' getFromAPI(2005, 2015, 500)
#' @export

getFromAPI <- function(startYear = 2015, endYear = 2018, amount = 500)
{
  if(!is.numeric(startYear)|!is.numeric(endYear)|!is.numeric(amount)|startYear<1000
     |endYear>=9999|startYear>endYear){
    print("please enter valid numbers as parameters!")
    return();
    }
  else if(amount>1000){print("amount cannot exceed 1000!")
    return()}
  else if(amount<1){print("amount cannot be smaller than 1!")
    return()}
  library(httr)
  library(jsonlite)
  library(dplyr)

  jdata<-GET(paste("https://api.fda.gov/drug/ndc.json?search=marketing_start_date:[",startYear,"0101+TO+",endYear,"1231]&limit=",amount,sep = ""))

  data<-fromJSON(rawToChar(jdata$content))
  return(as_tibble(data$results))
}
