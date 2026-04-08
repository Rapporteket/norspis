#' Les og preprosesser data for Norspis
#'
#' Leg nødvendige tabeller for Norspis og returner en utflatet dataramme
#'
#' @returns norspisdata En liste med registerdata i ulike former
#'
#' @export
#'
norspisLesOgProsesser <- function() {

  AlleScorer <- norspis::queryAlleScorer("data")
  EnkeltLeddNum <- norspis::queryEnkeltLeddNum("data")
  ForlopsOversikt <- norspis::queryForlopsOversikt("data")  |>
    dplyr::mutate(AvdRESH = as.numeric(AvdRESH)) |>
    dplyr::mutate(AvdRESH = min(AvdRESH), .by = SykehusNavn) |>
    dplyr::mutate(
      AvdBUV = dplyr::case_when(
        AvdRESH %in% c(109979, 4209804) & ForlopsType1Num %in% c(1,3,5,7,98) ~ "V",
        AvdRESH %in% c(109979, 4209804) & ForlopsType1Num %in% c(2,4,6,8,99) ~ "BU" ,
        .default = AvdBUV),
      Kortnavn = dplyr::case_when(
        AvdRESH == 109979 & ForlopsType1Num %in% c(1,3,5,7,98) ~ "OUS: Reg. V",
        AvdRESH == 109979 & ForlopsType1Num %in% c(2,4,6,8,99) ~ "OUS: Reg. BU",
        AvdRESH == 4209804 & ForlopsType1Num %in% c(1,3,5,7,98) ~ "MODUM: Spes.døgn V",
        AvdRESH == 4209804 & ForlopsType1Num %in% c(2,4,6,8,99) ~ "MODUM: Spes.døgn BU",
        .default = SykehusNavn),
      Over18 = ifelse(PasientAlder >= 18, TRUE, FALSE))


  AlleScorer <-
    AlleScorer[ , setdiff(
      names(AlleScorer),
      norspis::variabeloversikt$Variabelnavn[
        norspis::variabeloversikt$Tabell == "AlleScorer" &
          norspis::variabeloversikt$status == "Utgaatt"])]
  EnkeltLeddNum <-
    EnkeltLeddNum[ , setdiff(
      names(EnkeltLeddNum),
      norspis::variabeloversikt$Variabelnavn[
        norspis::variabeloversikt$Tabell == "EnkeltLeddNum" &
          norspis::variabeloversikt$status == "Utgaatt"])]

  RegData <- merge(
    EnkeltLeddNum[, names(EnkeltLeddNum) != "AvdRESH"],
    ForlopsOversikt[, c("AvdRESH", "AvdBUV", "ForlopsID",
                        "Kortnavn", "ForlopsType1Num",
                        "ForlopsType1", "BasisRegStatus",
                        "PasientAlder", "Fodselsdato",
                        "Over18")],
    by = "ForlopsID") |>
    dplyr::filter(BasisRegStatus == 1) |>
    merge(AlleScorer, by = "ForlopsID")

  RegData_start <- RegData[RegData$ForlopsType1Num %in% 3:4, ]
  RegData_start <-
    RegData_start[,  setdiff(
      names(RegData_start),
      norspis::variabeloversikt$Variabelnavn[
        norspis::variabeloversikt$status == "IkkeStart"])]
  RegData_slutt <- RegData[RegData$ForlopsType1Num %in% 5:6, ]
  RegData_slutt <-
    RegData_slutt[ , setdiff(
      names(RegData_slutt),
      norspis::variabeloversikt$Variabelnavn[
        norspis::variabeloversikt$status == "IkkeSlutt"])]
  RegData_slutt <- RegData_slutt[ , setdiff(names(RegData_slutt),
                                            c("AvdRESH", "Kortnavn",
                                              "PasientID", "SykehusNavn"))]

  RegData <- merge(RegData_start, RegData_slutt,
                   by.x = "ForlopsID", by.y = "RegTilhorendeReg",
                   suffixes = c("_start", "_slutt"),
                   all.x = TRUE) |>
    dplyr::mutate(
      StartAar = format(RegHendelsesdato_start, "%Y") |> as.numeric(),
      SluttAar = format(RegHendelsesdato_slutt, "%Y") |> as.numeric())

  ## Foreløpig, velg én sluttregistrering der det finnes flere
  RegData <- RegData[match(unique(RegData$ForlopsID), RegData$ForlopsID), ]
  ##########################################################################
  RegData <- merge(RegData,
                   ForlopsOversikt[, c("ForlopsID", "erMann", "Norsktalende",
                                       "Sivilstatus", "UtdanningSSB", "DognPol")])


  SkjemaOversikt <- norspis::querySkjemaOversikt("data") |>
    merge(ForlopsOversikt[, c("ForlopsID", "ForlopsType1",
                              "PasientAlder", "ForlopsType1Num",
                              "AvdRESH", "AvdBUV", "Kortnavn",
                              "DognPol"
                              )],
          by = "ForlopsID", all.x = T) |>
    dplyr::mutate(
      Over18 = ifelse(PasientAlder >= 18, TRUE, FALSE),
      SkjemaRekkeflg = ifelse(SkjemaRekkeflg==9, "5", SkjemaRekkeflg),
      Skjemanavn = factor(
        Skjemanavn,
        levels = Skjemanavn[match(sort(as.numeric(unique(SkjemaRekkeflg))),
                                  SkjemaRekkeflg)]))


  norspisdata <- list(RegData = RegData,
                      ForlopsOversikt = ForlopsOversikt,
                      SkjemaOversikt = SkjemaOversikt)
}


