#' @title gemeindemutationen
#'
#' @description Mit der Funktion \code{gemeindemutationen} werden alle Mutationen im engeren Sinn, die Auswirkungen auf die Gemeindenummer haben, für einen gewünschten Zeitraum angezeigt.
#'
#'
#' @param datum.neu Datum, bis zu dem Mutationen im engeren Sinn berücksichtigt werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden.
#' @param datum.daten Ein optionales Argument für das Datum, ab dem Mutationen im engeren Sinn berücksichtigt werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden. Default ist NA. In diesem Fall werden alle Mutationen seit dem 01.01.1960 berücksichtigt.
#'
#' @details \code{datum.neu} muss jünger sein als \code{datum.daten}, ansonsten wird ein leerer Dataframe zurückgegeben.
#'
#'     Berücksichtigt werden können grundsätzlich alle Mutationen im engeren Sinn zwischen 01.01.1960 und 01.01.2020.
#'
#'     Als Mutationen im engeren Sinn werden alle Gemeindefusionen, Gebietsänderungen, Eingemeindungen, Ausgemeindungen, Kantonswechsel, Bezirkswechsel und Neunummerierungen ausgewiesen, die mit einer Gebietsänderung und/oder einer Änderung der Gemeindenummer einher gehen.
#'
#'     Die Daten stammen grundsätzlich vom Bundesamt für Statistik (Gemeindeverzeichnis), wurden aber für die vorliegenden Zwecke aufgearbeitet, erweitert und angepasst.
#'
#' @return Ein Dataframe mit allen Mutationsmeldungen im engeren Sinn ab einem gewissen Zeitpunkt (frühestens 01.01.1960) und bis zu einem gewissen Zeitpunkt (spätestens 01.01.2020).
#' @export
#'
#' @examples \dontrun{
#'
#'    gemeindemutationen(datum.daten = "01.01.1986", datum.neu = "01.01.1992")
#'
#'    }
#'
gemeindemutationen<-function(datum.neu, datum.daten=NA){
  datum.neu<-as.Date(datum.neu, format=c("%d.%m.%Y"))
  m2<-filter(mutation, !meldung %in% c("Recode Munizipal- auf politische Gemeinde (TG)",
                                       "Gemeinsame Urne"))
  #Auswahl/Reduktion Mutationsfile (falls KEIN Datum der Daten angegeben)
  if (is.na(datum.daten)){
    m2<-filter(m2, m2$datum <= datum.neu)
    #Auswahl/Reduktion Mutationsfile (falls Datum der Daten angegeben)
  } else {
    datum.daten<-as.Date(datum.daten, format=c("%d.%m.%Y"))
    m2<-filter(m2, between(m2$datum, datum.daten, datum.neu))
  }
  return(m2)
}
