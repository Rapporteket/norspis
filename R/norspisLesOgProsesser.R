#' Les og preprosesser data for Norspis
#'
#' Leg nødvendige tabeller for Norspis og returner en utflatet dataramme
#'
#' @export
#'
norspisLesOgProsesser <- function() {

  AlleScorer <- norspis::queryAlleScorer("norspis")
  EnkeltLeddNum <- norspis::queryEnkeltLeddNum("norspis")
  ForlopsOversikt <- norspis::queryForlopsOversikt("norspis")

  AlleScorer <-
    AlleScorer[ , setdiff(names(AlleScorer),
                          norspis::variabeloversikt$Variabelnavn[norspis::variabeloversikt$Tabell == "AlleScorer" &
                                                                   norspis::variabeloversikt$status == "Utgaatt"])]
  EnkeltLeddNum <-
    EnkeltLeddNum[ , setdiff(names(EnkeltLeddNum),
                             norspis::variabeloversikt$Variabelnavn[norspis::variabeloversikt$Tabell == "EnkeltLeddNum" &
                                                                      norspis::variabeloversikt$status == "Utgaatt"])]

  RegData <- merge(EnkeltLeddNum,
                   ForlopsOversikt[, c("ForlopsID", "ForlopsType1Num",
                                       "ForlopsType1", "BasisRegStatus",
                                       "PasientAlder", "Fodselsdato")],
                   by = "ForlopsID") %>%
    dplyr::filter(BasisRegStatus == 1) %>%
    merge(AlleScorer, by = "ForlopsID") %>%
    merge(norspis::resh_voksen_barn[, c("AvdRESH", "AvdBUV",
                               "Kortnavn", "orgnr")],
          by = "AvdRESH", all.x = TRUE) %>%
    dplyr::mutate(AvdBUV = ifelse(AvdRESH == 109979 & ForlopsType1Num %in% c(1,3,5,7,98),
                                  "V", AvdBUV),
                  Kortnavn = ifelse(AvdRESH == 109979 & ForlopsType1Num %in% c(1,3,5,7,98),
                                    "OUS: Reg. V", Kortnavn),
                  Over18 = ifelse(PasientAlder >= 18, TRUE, FALSE))

  RegData_start <- RegData[RegData$ForlopsType1Num %in% 3:4, ]
  RegData_start <-
    RegData_start[,  setdiff(names(RegData_start),
                             norspis::variabeloversikt$Variabelnavn[norspis::variabeloversikt$status == "IkkeStart"])]
  RegData_slutt <- RegData[RegData$ForlopsType1Num %in% 5:6, ]
  RegData_slutt <-
    RegData_slutt[ , setdiff(names(RegData_slutt),
                             norspis::variabeloversikt$Variabelnavn[norspis::variabeloversikt$status == "IkkeSlutt"])]
  RegData_slutt <- RegData_slutt[ , setdiff(names(RegData_slutt),
                                            c("AvdRESH", "Kortnavn",
                                              "PasientID", "SykehusNavn",
                                              "orgnr"))]

  RegData <- merge(RegData_start, RegData_slutt,
                   by.x = "ForlopsID", by.y = "RegTilhorendeStartReg",
                   suffixes = c("_start", "_slutt"),
                   all.x = TRUE)

}


