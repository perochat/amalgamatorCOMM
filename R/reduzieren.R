#' @title reduzieren
#'
#' @description Mit der Funktion \code{reduzieren} können alle Mutationsmeldungen im engeren Sinn, gemeinsame Urnen und Wechsel von Munizipal- zu politischen Gemeinden, die Auswirkungen auf die Gemeindenummern haben, für einen gewünschten Zeitraum angezeigt werden.
#'
#' @param datum.neu Datum, bis zu dem Mutationen berücksichtigt werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden.
#' @param datum.daten Ein optionales Argument für das Datum, ab dem Mutationen berücksichtigt werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden. Default ist NA. In diesem Fall werden alle Mutationen seit dem 01.01.1960 berücksichtigt.
#' @param gemeinsame.urne Ein optionales Argument. Es gibt an, ob gemeinsame Urnen angezeigt werden sollen oder nicht.
#' @param munizipalgemeinde Ein optionales Argument. Es gibt an, ob die Reorganisation von Munizipal- zu politischen Gemeinden im Kanton Thurgau angezeigt werden sollen oder nicht.
#'
#' @details \code{datum.neu} muss jünger sein als \code{datum.daten}, ansonsten wird ein leerer Dataframe zurückgegeben.
#'
#'     Berücksichtigt werden können grundsätzlich alle Mutationen zwischen 01.01.1960 und 01.01.2020.
#'
#'     Als Mutationen werden alle Ausgemeindungen, Bezirkswechsel, Eingemeindungen, Gebietsänderungen, Gemeindefusionen, gemeinsame Urnen, Kantonswechsel, Neunummerierungen und Reorganisationen von Munizipal- zu politischen Gemeinden (Kanton Thurgau) ausgewiesen, die mit einer Gebietsänderung und/oder einer Änderung der Gemeindenummer einher gehen.
#'
#'     Die Daten stammen von verschiedenen Quellen des Bundesamts für Statistik (Gemeindeverzeichnis, Abstimmungsresultate), wurden aber für die vorliegenden Zwecke aufgearbeitet, erweitert und angepasst.
#'
#' @return Ein Dataframe mit allen Mutationsmeldungen ab einem gewissen Zeitpunkt (frühestens 01.01.1960) und bis zu einem gewissen Zeitpunkt (spätestens 01.01.2020).
#' @export
#'
#' @examples \dontrun{
#'
#'    reduzieren(datum.daten = "01.01.1986", datum.neu = "01.01.1992", gemeinsame.urne=T,
#'    munizipalgemeinde=F)
#'
#'    }
#'
reduzieren<-function(datum.neu, datum.daten=NA, gemeinsame.urne=T, munizipalgemeinde=T){
  #Reduktion Mutationsmeldungen auf gewählten Zeitraum
  m2<-gemeindemutationen(datum.daten=datum.daten, datum.neu=datum.neu)
  #Gemeinsame Urne
  if (isTRUE(gemeinsame.urne)){
    gu<-gemeinsame.urne(datum.daten=datum.daten, datum.neu=datum.neu)
    m2<-rbind(m2, gu)
  }
  #Munizipalgemeinden im Kanton Thurgau
  if (isTRUE(munizipalgemeinde)){
    mg<-munizipalgemeinde()
    m2<-rbind(m2, mg)
  }
  return(m2)
}
