#' @title change.numcom
#'
#' @description \code{change.numcom} fügt einem Dataframe mit kommunalen Daten eine zusätzliche Spalte mit der Gemeindenummer zu einem späteren Zeitpunkt an.
#'
#' @param dataframe Ein Dataframe mit Daten mit kommunalem Bezug, die auf einen neuen, spätere Gemeindestand angepasst
#' werden sollen.
#' @param gemeindenummer character. Gibt den Namen der Spalte an, in dem die Gemeindenummer steht (Schlüsselvariable).
#' @param neuer.gemeindename optional. Wenn gewünscht, wird im Output der neue Name der jeweiligen Gemeinde abgedruckt. Default ist FALSE.
#' @param gemeindename character. Gibt den Namen der Spalte des Dataframes an, in der der Gemeindename steht. Falls der neue Gemeindename im Output abgedruckt werden soll, muss neuer.gemeindename = TRUE sein.
#' @param datum.daten Ein optionales Argument für das Datum, auf das sich die Daten im Dataframe beziehen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden. Default ist NA. In diesem Fall werden alle Mutationen seit dem 01.01.1960 berücksichtigt.
#' @param datum.neu Datum, bis zu dem Mutationen berücksichtigt werden sollen. Es entspricht folglich dem Datum des Gemeindestandes, auf den die Daten aggregiert werden sollen. Das Datum muss im Format tt.mm.JJJJ eingegeben werden.
#' @param gemeinsame.urne optional. Gibt an, ob gemeinsame Urnen berücksichtigt werden sollen. Default ist TRUE. (Siehe Details)
#' @param munizipalgemeinde optional. Gibt an, ob die Reorganisation von Munizipal- zu politischen Gemeinden im Kanton Thurgau berücksichtigt werden sollen. Default ist TRUE. (Siehe Details)
#'
#' @details \code{datum.neu} muss jünger sein als \code{datum.daten}, ansonsten wird die Berechnung abgebrochen.
#'
#'     Berücksichtigt werden können grundsätzlich alle Mutationen zwischen 01.01.1960 und 01.01.2020.
#'
#'     Falls zwischen \code{datum.daten} und \code{datum.neu} keine Mutationen stattgefunden haben, wird die Berechnung abgebrochen.
#'
#'     Als Mutationen werden zunächst alle Bezirkswechsel, Eingemeindungen, Gebietsänderungen, Gemeindefusionen, Kantonswechsel und Neunummerierungen ausgewiesen, die mit einer Gebietsänderung und/oder einer Änderung der Gemeindenummer einher gehen. Die Daten stammen von verschiedenen Quellen des Bundesamts für Statistik (Gemeindeverzeichnis, Abstimmungsresultate), wurden aber für die vorliegenden Zwecke aufgearbeitet, erweitert und angepasst.
#'
#'     Berücksichtigt werden können ferner alle gemeinsamen Urnen. Seit September 2014 haben einzelne Gemeinden im Kanton Bern kein eigenes Wahl-/Stimmbüro mehr. Stattdessen werden die Wahl-/Stimmzettel in einer anderen Gemeinde (dem "Urnenstandort") ausgezählt und ausgewiesen.
#'
#'     Darüber hinaus können auch alle Reorganisationen von Munizipal- zu politischen Gemeinden im Kanton Thurgau (ab 1995) berücksichtigt werden. Die Überführungen der aus mehreren Ortsgemeinden bestehenden Munizipalgemeinden in politische Gemeinden gingen mit Änderungen in den Gemeindenummerierungen einher.
#'
#'     Falls im gewählten Zeitraum Ausgemeindungen stattgefunden haben, erscheint eine entsprechende Warnung. Ausgemeindungen können nicht berücksichtigt werden und werden dementsprechend ignoriert.
#'
#' @return Ein Dataframe, in dem die eingelesenen Daten mit einer zusätzlichen Spalte versehen werden, die die jeweilige Gemeindenummer zu einem späteren Zeitpunkt (\code{datum.neu}) anzeigt. Es können alle Mutationsmeldungen ab dem 01.01.1960 bis zum 01.01.2020 berücksichtigt werden (ausser Ausgemeindungen).
#'
#'     Falls gewünscht, wird der Dataframe zusätzlich mit einer weiteren Spalte ergänzt, die den Namen der mutierten Gemeinde zum spätere Zeitpunkt (\code{datum.neu}) darstellt.
#'
#' @export
#'
#' @import dplyr
#'
#' @examples \dontrun{
#'
#'    # Ohne Gemeindename
#'
#'    change.numcom(dataframe = bspdata, gemeindenummer = "Code", neuer.gemeindename = F,
#'    datum.daten = "01.01.1980", datum.neu = "01.01.1990", gemeinsame.urne = T,
#'    munizipalgemeinde = T)
#'
#'
#'    # Mit Gemeindename, ohne gemeinsame Urne
#'
#'    change.numcom(dataframe = bspdata, gemeindenummer = "Code", neuer.gemeindename = T,
#'    gemeindename = "Gemeinden", datum.daten = "01.01.1980", datum.neu = "01.01.1990",
#'    gemeinsame.urne = F, munizipalgemeinde = T)
#'
#'    }
#'
change.numcom<-function(dataframe, gemeindenummer=as.character(), neuer.gemeindename=F, gemeindename=as.character(),
                        datum.daten=NA, datum.neu, gemeinsame.urne=T, munizipalgemeinde=T){
  df<-dataframe
  df$gemeindenummer.neu<-df[,gemeindenummer]
  #Reduktion auf relevante Mutationen
  m2<-reduzieren(datum.daten=datum.daten, datum.neu=datum.neu, gemeinsame.urne=gemeinsame.urne,
                 munizipalgemeinde=munizipalgemeinde)
  #Ausgemeindungen ignorieren
  if (sum(m2$gdenrvor[m2$meldung=="Ausgemeindung"] %in% df$gemeindenummer.neu)>=1){
    warning("\n \nEs sind Ausgemeindungen vorhanden. \nAusgemeindungen werden ignoriert\n\n")
    m2<-filter(m2, meldung != "Ausgemeindung")
  }
  #Abbruch falls m2 leer (keine Mutationen)
  if (length(m2$ID)==0){
    stop("\nFehler! \nEs haben keine Mutationen stattgefunden. Berechnung wird abgebrochen.
         \nBist Du sicher, dass datum.neu juenger ist als Datum der Daten?\n\n")
  }
  #Reihenfolge Mutationsfile (mehrfache Fusionen)
  m2<-arrange(m2, datum, gdenrvor)
  # Gemeindenummer zum Zeitpunkt datum.neu
  for (i in 1:length(m2$ID)){
    if (m2$gdenrvor[i] %in% df$gemeindenummer.neu){
      df$gemeindenummer.neu[df$gemeindenummer.neu==m2$gdenrvor[i]]<-m2$gdenrnach[i]
    }
  }
  #neuer Gemeindename im Output, falls gewuenscht
  if (isTRUE(neuer.gemeindename)){
    df$gemeindename.neu<-as.character(df[,gemeindename])
    for (i in 1:length(m2$ID)){
      if (m2$gdenrnach[i] %in% df$gemeindenummer.neu){
        df$gemeindename.neu[df$gemeindenummer.neu==m2$gdenrnach[i]]<-m2$gdenamenach[i]
      }
    }
  }
  #Reihenfolge der Spalten: Gemeindenummer alt, Gemeindenummer neu, Gemeindename alt, Gemeindename alt, Rest
  ## Falls neuer Gemeindename im Output erwuenscht:
  if (isTRUE(neuer.gemeindename)){
    df<-select(df, gemeindenummer, gemeindenummer.neu, gemeindename, gemeindename.neu, everything())
    # Falls neuer Gemeindename im Output nicht erwuenscht
  } else {
    df<-select(df, gemeindenummer, gemeindenummer.neu, everything())
  }
  return(df)
}
