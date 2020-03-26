#' @title munizipalgemeinde
#'
#' @description Mit der Funktion \code{munizipalgemeinde} werden die im Kanton Thurgau vollzogenen Reorganisationen von Munizipal- zu politischen Gemeinden angezeigt.
#'
#' @details Ab 1995 wurden im Kanton Thurgau die aus mehreren Ortsgemeinden bestehenden Munizipalgemeinden in politische Gemeinden überführt. Dies ging mit einer Änderung der Gemeindenummer einher. Mit der vorliegenden Funktion können diese Wechsel in der Nummerierung nachvollzogen werden.
#'
#'     Die Daten stammen aus Vergleichen ausgewiesener Abstimmungsresultate auf Gemeindeebene mit den Thurgauer Gemeindenummern. Sie wurden für die vorliegenden Zwecke aufgearbeitet, erweitert und angepasst.
#'
#' @return Ein Dataframe mit allen Recodes von Munizipal- auf politische Gemeinden.
#' @export
#'
#' @examples \dontrun{
#'
#'    munizipalgemeinde()
#'
#'    }
#'
munizipalgemeinde<-function(){
  mutation<-mutation
  mg<-filter(mutation, meldung=="Recode Munizipal- auf politische Gemeinde (TG)")
  return(mg)
}
