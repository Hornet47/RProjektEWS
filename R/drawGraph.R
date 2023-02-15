#' Zeichne die Grafik
#'
#' \code{drawGraph} zeichnet die Grafik, die die eingegebene Daten benutzt.
#'
#'
#' Dies ist eine Funktion, die eine Grafik zeichnet, die Informationen ueber
#' die Relation zwischen count und marketing starting date darstellt.
#' Die Quelle ist \code{data}. Diese Funktion haengt von \code{\link[ggplot2]{ggplot}}
#' und \code{\link[parsedate]{parse_date}} ab.
#'
#' @return void.
#'
#' @param data die Daten, auf den die Grafik basiert.
#'
#' @examples
#' drawGraph()
#' drawGraph(getFromAPI())
#' @export

drawGraph <- function(data = getFromAPI()){
  library(ggplot2)
  library(parsedate)

  ggplot(data)+
  geom_histogram(aes(x = parse_date(marketing_start_date),fill = change(dosage_form)),bins = 20)+
  labs(title = "Count Of Drug Products With Marketing Start Date", x = "Marketing Start Date",y = "Count",fill = "Dosage Form")
}
