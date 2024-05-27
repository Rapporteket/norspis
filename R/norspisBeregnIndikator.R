#' Beregn kvalitetsindikatorer for Norspis
#'
#' Beregn kvalitetsindikatorer for Norspis til bruk på Rapportalen
#'
#' @export
#'
norspisBeregnIndikator <- function(RegData, ind_id = "norspis_KI1_symptomreduksjon_EDEQ_V") {

  terskel=5
  minstekrav = NA
  maal = NA
  legPlass="top"
  minstekravTxt="Min."
  maalTxt="Mål"
  decreasing=F
  maalretn="hoy"


  if (ind_id == "norspis_KI1_symptomreduksjon_EDEQ_V") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(EDEQ60GlobalScore_start),
                    !is.na(EDEQ60GlobalScore_slutt),
                    AvdBUV_start == "V") %>%
      # Over18_start) %>%
      dplyr::mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::mutate(var = ifelse((EDEQ60GlobalScore_start-EDEQ60GlobalScore_slutt) >= 0.9,
                                 1, 0),
                    denominator = 1) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 50
    tittel <- c("Andelen med klinisk signifikant",  "bedring i spiseforstyrrelsessymptomer (V)")
  }

  if (ind_id == "norspis_KI1_symptomreduksjon_EDEQ_BU") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(EDEQ60GlobalScore_start),
                    !is.na(EDEQ60GlobalScore_slutt),
                    AvdBUV_start == "BU") %>%
      # Over18_start) %>%
      dplyr::mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::mutate(var = ifelse((EDEQ60GlobalScore_start-EDEQ60GlobalScore_slutt) >= 0.9,
                                 1, 0),
                    denominator = 1) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 70
    tittel <- c("Andelen med klinisk signifikant",  "bedring i spiseforstyrrelsessymptomer (BU)")
  }


  if (ind_id == "norspis_KI2_funksjonsbedring_CIA_V") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(CIA30GlobalScore_slutt),
                    !is.na(CIA30GlobalScore_start),
                    AvdBUV_start == "V") %>%
      # Over18_start) %>%
      dplyr::mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::mutate(var = ifelse((CIA30GlobalScore_start-CIA30GlobalScore_slutt) >= 7,
                                 1, 0),
                    denominator = 1) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 50
    tittel <- c("Andelen med bedring i",  "funksjon/livskvalitet (V)")
  }

  if (ind_id == "norspis_KI2_funksjonsbedring_CIA_BU") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(CIA30GlobalScore_slutt),
                    !is.na(CIA30GlobalScore_start),
                    AvdBUV_start == "BU") %>%
      # Over18_start) %>%
      dplyr::mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::mutate(var = ifelse((CIA30GlobalScore_start-CIA30GlobalScore_slutt) >= 7,
                                 1, 0),
                    denominator = 1) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 70
    tittel <- c("Andelen med bedring i",  "funksjon/livskvalitet (BU)")
  }



  if (ind_id == "norspis_KI3_undervektsreduksjon_V") {

    Indikator <- RegData %>%
      dplyr::mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                                 MedIsoBMIBGS_start, MedBMI_start),
                    bmi_slutt = ifelse(!is.na(MedIsoBMIBGS_slutt),
                                       MedIsoBMIBGS_slutt, MedBMI_slutt)) %>%
      dplyr::filter(!is.na(bmi),
                    bmi < 18.5,
                    !is.na(bmi_slutt),
                    AvdBUV_start == "V") %>%
      dplyr::mutate(var = ifelse(bmi_slutt >= 18.5, 1, 0),
                    denominator = 1,
                    year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 60
    tittel <- c("Andelen med endring av undervektstatus",  "til ikke undervektig ved slutt (V)")
  }

  if (ind_id == "norspis_KI3_undervektsreduksjon_BU") {

    Indikator <- RegData %>%
      dplyr::mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                                 MedIsoBMIBGS_start, MedBMI_start),
                    bmi_slutt = ifelse(!is.na(MedIsoBMIBGS_slutt),
                                       MedIsoBMIBGS_slutt, MedBMI_slutt)) %>%
      dplyr::filter(!is.na(bmi),
                    bmi < 18.5,
                    !is.na(bmi_slutt),
                    AvdBUV_start == "BU") %>%
      dplyr::mutate(var = ifelse(bmi_slutt >= 18.5, 1, 0),
                    denominator = 1,
                    year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 60
    tittel <- c("Andelen med endring av undervektstatus",  "til ikke undervektig ved slutt (BU)")
  }


  if (ind_id == "norspis_KI4_utfallsvurdering_V") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(PT03Utfallsvurd),
                    PT03Utfallsvurd != 9,
                    AvdBUV_start == "V") %>%
      # Over18_start) %>%
      dplyr::mutate(var = ifelse(PT03Utfallsvurd %in% 1:2, 1, 0),
                    denominator = 1,
                    year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 50
    tittel <- c("Andelen som rapporterer \"Klar bedring\"",  "eller \"Ikke noe problem lenger\" (V)")
  }

  if (ind_id == "norspis_KI4_utfallsvurdering_BU") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(PT03Utfallsvurd),
                    PT03Utfallsvurd != 9,
                    AvdBUV_start == "BU") %>%
      # Over18_start) %>%
      dplyr::mutate(var = ifelse(PT03Utfallsvurd %in% 1:2, 1, 0),
                    denominator = 1,
                    year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 70
    tittel <- c("Andelen som rapporterer \"Klar bedring\"",  "eller \"Ikke noe problem lenger\" (BU)")
  }

  if (ind_id == "norspis_involvering_famlie_venner_BU") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(PT02BleInvolv) & PT02BleInvolv!= 9,
                    !Over18_start) %>%
      dplyr::mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric(),
                    var = PT02BleInvolv,
                    denominator = 1) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 100
    tittel <- c("Andelen som rapporterer at familie og/eller",
                "venner ble involvert i behandlingen,",
                "av pasientene som ønsket involvering (BU).")
  }

  if (ind_id == "norspis_involvering_famlie_venner_V") {

    Indikator <- RegData %>%
      dplyr::filter(PT01OnsketInvolv == 1,
                    !is.na(PT02BleInvolv) & PT02BleInvolv != 9,
                    Over18_start) %>%
      dplyr::mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric(),
                    var = PT02BleInvolv,
                    denominator = 1) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 100
    tittel <- c("Andelen som rapporterer at familie og/eller",
                "venner ble involvert i behandlingen,",
                "av pasientene som ønsket involvering (V).")
  }

  if (ind_id == "norspis_undervekt_prodprove_V") {

    Indikator <- RegData %>%
      dplyr::mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                                 MedIsoBMIBGS_start, MedBMI_start)) %>%
      dplyr::filter(!is.na(bmi),
                    bmi < 18.5,
                    MedBlodprove_start %in% c(0,1),
                    AvdBUV_start == "V") %>%
      dplyr::mutate(var = MedBlodprove_start,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige pasienter (BMI < 18,5) ved ", "start, der det er tatt blodprøver ved start (V).")
  }

  if (ind_id == "norspis_undervekt_prodprove_BU") {

    Indikator <- RegData %>%
      dplyr::mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                                 MedIsoBMIBGS_start, MedBMI_start)) %>%
      dplyr::filter(!is.na(bmi),
                    bmi < 18.5,
                    MedBlodprove_start %in% c(0,1),
                    AvdBUV_start == "BU") %>%
      dplyr::mutate(var = MedBlodprove_start,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige pasienter (BMI < 18,5) ved ", "start, der det er tatt blodprøver ved start (BU).")
  }

  if (ind_id == "norspis_oppkast_prodprove") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(EDE16GgrOppkast_start),
                    EDE16GgrOppkast_start >= 32,
                    MedBlodprove_start %in% c(0,1)) %>%
      dplyr::mutate(var = MedBlodprove_start,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen med alvorlig eller ekstremt oppkast ved", "start, der det er tatt blodprøver ved start.")
  }

  if (ind_id == "norspis_oppkast_prodprove_V") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(EDE16GgrOppkast_start),
                    EDE16GgrOppkast_start >= 32,
                    MedBlodprove_start %in% c(0,1),
                    AvdBUV_start == "V") %>%
      dplyr::mutate(var = MedBlodprove_start,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen med alvorlig eller ekstremt oppkast ved", "start, der det er tatt blodprøver ved start (V).")
  }

  if (ind_id == "norspis_oppkast_prodprove_BU") {

    Indikator <- RegData %>%
      dplyr::filter(!is.na(EDE16GgrOppkast_start),
                    EDE16GgrOppkast_start >= 32,
                    MedBlodprove_start %in% c(0,1),
                    AvdBUV_start == "BU") %>%
      dplyr::mutate(var = MedBlodprove_start,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen med alvorlig eller ekstremt oppkast ved", "start, der det er tatt blodprøver ved start (BU).")
  }

  if (ind_id == "norspis_beintetthetsmaling_V") {

    Indikator <- RegData %>%
      dplyr::mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                                 MedIsoBMIBGS_start, MedBMI_start),
                    BeintetthMaling = ifelse(MedBeintetthMaling_start == 1 |
                                               MedBeintetthMaling_slutt == 1,
                                             1, 0)) %>%
      dplyr::filter(Over18_start,
                    bmi < 18.5,
                    MedBeintetthMaling_start %in% 0:1 |
                      MedBeintetthMaling_slutt %in% 0:1) %>%
      dplyr::mutate(BeintetthMaling = ifelse(is.na(BeintetthMaling), 0, BeintetthMaling)) %>%
      dplyr::mutate(var = BeintetthMaling,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige ved start der det", "oppgis at det er gjort en beintetthetsmåling (V)")
  }

  if (ind_id == "norspis_beintetthetsmaling_BU") {

    Indikator <- RegData %>%
      dplyr::mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                                 MedIsoBMIBGS_start, MedBMI_start),
                    BeintetthMaling = ifelse(MedBeintetthMaling_start == 1 |
                                               MedBeintetthMaling_slutt == 1,
                                             1, 0)) %>%
      dplyr::filter(!Over18_start,
                    bmi < 18.5,
                    MedBeintetthMaling_start %in% 0:1 |
                      MedBeintetthMaling_slutt %in% 0:1) %>%
      dplyr::mutate(BeintetthMaling = ifelse(is.na(BeintetthMaling), 0, BeintetthMaling)) %>%
      dplyr::mutate(var = BeintetthMaling,
                    denominator = 1,
                    year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      dplyr::select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      dplyr::rename(SykehusNavn = Kortnavn) %>%
      dplyr::mutate(ind_id = ind_id,
                    context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige ved start der det", "oppgis at det er gjort en beintetthetsmåling (BU)")
  }



  indikatordata <- list(indikator=Indikator, tittel=tittel, terskel=terskel,
                        minstekrav=minstekrav, maal=maal, legPlass=legPlass,
                        minstekravTxt=minstekravTxt,
                        maalTxt=maalTxt, decreasing=decreasing,
                        maalretn=maalretn)


}
