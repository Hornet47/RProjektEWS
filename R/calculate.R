#' Berechne die numerische Zusammenfassung
#'
#' \code{calculate} berechnet die numerische Zusammenfassung von \code{data}.
#'
#'
#' Dies ist eine Funktion, die die median von marketing_start_date berechnet in
#' den Untergruppen von finished und dosage_form.
#'
#' @return ein tibble von der numerische Zusammenfassung.
#'
#' @param data die Daten, auf den die Berechnung basiert.
#'
#' @examples
#' calculate()
#' calculate(getFromAPI())
#' @export
calculate<-function(data = getFromAPI()){
  library(dplyr)
  library(stats)
  library(parsedate)
  data%>%
    group_by(finished, change(dosage_form))%>%
    summarise(medianDate = median(parse_date(marketing_start_date)))%>%
    ungroup()%>%
    arrange(desc(medianDate))
}
