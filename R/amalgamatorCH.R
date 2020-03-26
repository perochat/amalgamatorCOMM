#' @title amalgamatorCH
#'
#' @description \code{amalgamatorCH} aggregiert Daten mit kommunalem Bezug (Gemeindenummer) aus einem Dataframe auf den Gemeindestand zu einem späteren Zeitpunkt.
#'
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
#' @param ... Namen-Werte-Paare für summary-Funktionen. Der Name entspricht dem Namen der Variable im resultierenden Dataframe. Der Wert ist das Resultat einer summary-Funktion, die einen einzelnen Wert zurückgibt, z.B. min(), n() oder sum().
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
#'     Falls im gewählten Zeitraum Ausgemeindungen stattgefunden haben, erscheint eine entsprechende Warnung. Ausgemeindungen können nicht berücksichtigt werden und werden dementsprechend ignoriert. Es werden Missing Values (NA) ausgegeben.
#'
#'
#' @return Ein Dataframe, in dem die eingelesenen Daten auf den Gemeindestand zu einem späteren Zeitpunkt (\code{datum.neu}) aggregiert werden. Es können alle Mutationsmeldungen ab dem 01.01.1960 bis zum 01.01.2020 berücksichtigt werden (ausser Ausgemeindungen).
#'
#'     Falls gewünscht, wird der Dataframe zusätzlich mit einer weiteren Spalte ergänzt, die den Namen der mutierten Gemeinde zum spätere Zeitpunkt (\code{datum.neu}) darstellt.
#'
#'     Im Falle von Ausgemeindungen werden die Werte der entsprechenden Gemeinde als NA ausgegeben.
#'
#' @seealso \code{\link[dplyr]{summarize}}, \code{\link{mean}}, \code{\link{median}}, \code{\link{sd}}, \code{\link{IQR}}, \code{\link{mad}}, \code{\link{min}},
#'     \code{\link{max}}, \code{\link{quantile}}, \code{\link{first}}, \code{\link{last}}, \code{\link{nth}}, \code{\link{n}},
#'     \code{\link{any}}, \code{\link{all}}
#'
#' @export
#'
#' @import dplyr
#'
#' @examples \dontrun{
#'
#'    # Ohne Gemeindename
#'
#'    amalgamatorCH(dataframe = bspdata, gemeindenummer = "Code", neuer.gemeindename = F,
#'    datum.daten = "01.01.1980", datum.neu = "01.01.1990", gemeinsame.urne = T,
#'    munizipalgemeinde = T, variable1 = sum(Var1), variable2 = mean(Var2))
#'
#'
#'    # Mit Gemeindename, ohne gemeinsame Urne
#'
#'    amalgamatorCH(dataframe = bspdata, gemeindenummer = "Code", neuer.gemeindename = T,
#'    gemeindename = "Gemeinden", datum.daten = "01.01.1980", datum.neu = "01.01.1990",
#'    gemeinsame.urne = F, munizipalgemeinde = T,
#'    variable1 = sum(Var1), variable2 = mean(Var2))
#'
#'    }
#'
#'
#'
amalgamatorCH<-function(dataframe, gemeindenummer=as.character(), neuer.gemeindename=F,
                        gemeindename=as.character(), datum.daten=NA, datum.neu,
                        gemeinsame.urne=T, munizipalgemeinde=T, ...){
  # Reduktion Mutationsmeldungen auf gewaehlten Zeitraum
  m2<-reduzieren(datum.daten=datum.daten, datum.neu=datum.neu, gemeinsame.urne=gemeinsame.urne,
                 munizipalgemeinde=munizipalgemeinde)
  #Abbruch falls m2 leer (keine Mutationen)
  if (length(m2$ID)==0){
    stop("\nFehler! \nEs haben keine Mutationen stattgefunden. Berechnung wird abgebrochen.
         \nBist Du sicher, dass datum.neu juenger ist als Datum der Daten?\n\n")
  }
  # Ausgemeindungen
  ausg<-filter(m2, meldung=="Ausgemeindung")
  ausg<-unique(ausg$gdenrvor)
  # Gemeindenummer auf neuen Gemeindestand anpassen
  df<-change.numcom(dataframe=dataframe, gemeindenummer=gemeindenummer,
                    neuer.gemeindename=neuer.gemeindename, gemeindename=gemeindename,
                    datum.daten=datum.daten, datum.neu=datum.neu,
                    gemeinsame.urne=gemeinsame.urne, munizipalgemeinde=munizipalgemeinde)
  # Ab hier Imputation/Aggregation
  # summarize (als dataframe)
  dat<-data.frame(df %>%
                    group_by(gemeindenummer.neu) %>%
                    summarize(...))
  # neuer Gemeindename in output-df
  if (isTRUE(neuer.gemeindename)){
    for (i in 1:length(dat$gemeindenummer.neu)){
      dat$gemeinde[i]<-as.character(unique(df$gemeindename.neu[df$gemeindenummer.neu == dat$gemeindenummer.neu[i]]))
    }
    #Reihenfolge der Spalten: Gemeindenummer, Gemeindename, Rest
    dat<-select(dat, gemeindenummer.neu, gemeinde, everything())
  }
  # Daten im Falle von Ausgemeindungen auf NA setzen
  if (length(ausg>=1)){
    dat[dat$gemeindenummer.neu %in% ausg, 2:ncol(dat)]<-NA
  }
  return(dat)
}
