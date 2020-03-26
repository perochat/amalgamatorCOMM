#' @title Mutationsmeldungen 1960-2020
#'
#' @description Dataframe mit allen kommunalen Mutationen der Schweiz zwischen 01.01.1960 und 01.01.2020.
#'
#'     Es wird unterschieden zwischen Mutationen im engeren Sinn, gemeinsamen Urnen und Reorganisationen von Munizipal- zu politischen Gemeinden:
#'
#'     \strong{Mutationen im engeren Sinn:} Als Mutationen im engeren Sinn werden alle Gemeindefusionen, Gebietsänderungen, Eingemeindungen, Ausgemeindungen, Kantonswechsel, Bezirkswechsel und Neunummerierungen ausgewiesen, die mit einer Gebietsänderung und/oder einer Änderung der Gemeindenummer einher gehen.
#'
#'     \strong{Gemeinsame Urnen:} Seit September 2014 haben einzelne Gemeinden im Kanton Bern kein eigenes Wahl-/Stimmbüro mehr. Stattdessen werden die Wahl-/Stimmzettel in einer anderen Gemeinde (dem "Urnenstandort") ausgezählt und ausgewiesen.
#'
#'     \strong{Munizipal- zu politischen Gemeinden:} Ab 1995 wurden im Kanton Thurgau die aus mehreren Ortsgemeinden bestehenden Munizipalgemeinden in politische Gemeinden überführt. Dies ging mit einer Änderung der Gemeindenummer einher.
#'
#' @name mutation
#'
#' @docType data
#'
#' @usage mutation
#'
#' @format Ein Dataframe mit 1527 Zeilen und 12 Spalten.
#'
#' @details Folgende Daten stehen zur Verfügung:
#'
#'     \strong{ID:} Individuelle Nummer für jeden Eintrag.
#'
#'     \strong{mutationsnummer:} Aufgeführt sind Mutationsnummern, die das Bundesamt für Statistik für Gemeindemutationen im engeren Sinn vergibt.
#'     Mit den Codes 8xxxx sind alle Reorganisationen von Munizipal- zu politischen Gemeinden im Kanton Thurgau gelistet.
#'     Mit den Codes 9xxxx sind alle gemeinsamen Urnen aufgeführt.
#'
#'     \strong{meldung:} Angabe des Typs von Mutation; unterschieden wird zwischen Ausgemeindungen, Bezirkswechseln,
#'     Eingemeindungen, Gebietsänderungen, Gemeindefusionen, Gemeinsamen Urnen, Kantonswechseln, Neunummerierungen und
#'     den Reorganisationen der Munizipal- in politische Gemeinden des Kantons Thurgau.
#'
#'     \strong{datum:} Datum des Inkrafttretens der jeweiligen Mutation, Format dd.tt.JJJJ.
#'
#'     \strong{kantonvor:} Kantonszugehörigkeit der jeweiligen Gemeinde vor der Mutation.
#'
#'     \strong{bezirkvor:} Bezirksnummer der jeweiligen Gemeinde vor der Mutation.
#'
#'     \strong{gdenrvor:} Gemeindenummer der jeweiligen Gemeinde vor der Mutation.
#'
#'     \strong{gdenamevor:} Name der jeweiligen Gemeinde vor der Mutation.
#'
#'     \strong{kantonnach:} Kantonszugehörigkeit der jeweiligen Gemeinde nach der Mutation.
#'
#'     \strong{bezirknach:} Bezirksnummer der jeweiligen Gemeinde nach der Mutation.
#'
#'     \strong{gdenrnach:} Gemeindenummer der jeweiligen Gemeinde nach der Mutation.
#'
#'     \strong{gdenamenach:} Name der jeweiligen Gemeinde nach der Mutation.
#'
#' @source Die Daten stammen von verschiedenen Quellen des Bundesamts für Statistik (Gemeindeverzeichnis, Resultate Eidgenössische Abstimmungen). Sie wurden für die vorliegenden Zwecke aufgearbeitet, erweitert und angepasst.
#'
NULL
