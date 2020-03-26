#' @title gemeinsame.urne
#'
#' @description Mit der Funktion \code{gemeinsame.urne} können für einen gewünschten Zeitraum gemeinsame Urnen im Kontext von Eidgenössischen Wahlen und Abstimmungen angezeigt werden.
#'
#'
#' @param datum.neu Datum, bis zu dem gemeinsame Urnen berücksichtigt werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden.
#' @param datum.daten Ein optionales Argument für das Datum, ab dem gemeinsame Urnen berücksichtigt werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden. Default ist NA. In diesem Fall werden alle gemeinsamen Urnen seit dem 28.09.2014 berücksichtigt.
#'
#' @details Seit September 2014 haben einzelne Gemeinden im Kanton Bern kein eigenes Wahl-/Stimmbüro mehr. Stattdessen werden die Wahl-/Stimmzettel in einer anderen Gemeinde (dem "Urnenstandort") ausgezählt und ausgewiesen.
#'
#'     \code{datum.neu} muss jünger sein als \code{datum.daten}, ansonsten wird ein leerer Dataframe zurückgegeben.
#'
#'     Berücksichtigt werden können grundsätzlich alle gemeinsamen Urnen zwischen 28.09.2014 (der erstmaligen Einführung einer gemeinsamen Urne) und 01.01.2020.
#'
#'     Die Daten stammen von ausgewiesenen Abstimmungsresultaten auf Gemeindeebene vom Bundesamt für Statistik. Sie wurden für die vorliegenden Zwecke aufgearbeitet, erweitert und angepasst.
#'
#' @return Ein Dataframe mit allen gemeinsamen Urnen ab einem gewissen Zeitpunkt (frühestens 28.09.2014) und bis zu einem gewissen Zeitpunkt (spätestens 01.01.2020).
#' @export
#'
#' @examples \dontrun{
#'
#'    gemeinsame.urne(datum.daten = "01.01.1986", datum.neu = "01.01.1992")
#'
#'    }
#'
gemeinsame.urne<-function(datum.neu, datum.daten=NA){
  datum.neu<-as.Date(datum.neu, format=c("%d.%m.%Y"))
  gu<-filter(mutation, meldung=="Gemeinsame Urne")
  #Auswahl/Reduktion Mutationsfile (falls KEIN Datum der Daten angegeben)
  if (is.na(datum.daten)){
    gu<-filter(gu, gu$datum <= datum.neu)
    #Auswahl/Reduktion Mutationsfile (falls Datum der Daten angegeben)
  } else {
    datum.daten<-as.Date(datum.daten, format=c("%d.%m.%Y"))
    gu<-filter(gu, between(gu$datum, datum.daten, datum.neu))
  }
  return(gu)
}
