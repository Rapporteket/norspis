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
  skriftStr=1.0
  pktStr=1.4
  legPlass="top"
  minstekravTxt="Min."
  maalTxt="Mål"
  decreasing=F
  width=800
  height=700
  maalretn="hoy"


  if (ind_id == "norspis_KI1_symptomreduksjon_EDEQ_V") {

    Indikator <- RegData %>%
      filter(!is.na(EDEQ60GlobalScore_start),
             !is.na(EDEQ60GlobalScore_slutt),
             Over18_start) %>%
      mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      mutate(var = ifelse((EDEQ60GlobalScore_start-EDEQ60GlobalScore_slutt) >= 0.9,
                          1, 0),
             denominator = 1) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 50
    tittel <- c("Andelen med klinisk signifikant",  "bedring i spiseforstyrrelsessymptomer (V)")
  }

  if (ind_id == "norspis_KI1_symptomreduksjon_EDEQ_BU") {

    Indikator <- RegData %>%
      filter(!is.na(EDEQ60GlobalScore_start),
             !is.na(EDEQ60GlobalScore_slutt),
             !Over18_start) %>%
      mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      mutate(var = ifelse((EDEQ60GlobalScore_start-EDEQ60GlobalScore_slutt) >= 0.9,
                          1, 0),
             denominator = 1) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 70
    tittel <- c("Andelen med klinisk signifikant",  "bedring i spiseforstyrrelsessymptomer (BU)")
  }


  if (ind_id == "norspis_KI2_funksjonsbedring_CIA_V") {

    Indikator <- RegData %>%
      filter(!is.na(CIA30GlobalScore_slutt),
             !is.na(CIA30GlobalScore_start),
             Over18_start) %>%
      mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      mutate(var = ifelse((CIA30GlobalScore_start-CIA30GlobalScore_slutt) >= 7,
                          1, 0),
             denominator = 1) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 50
    tittel <- c("Andelen med bedring i",  "funksjon/livskvalitet (V)")
  }

  if (ind_id == "norspis_KI2_funksjonsbedring_CIA_BU") {

    Indikator <- RegData %>%
      filter(!is.na(CIA30GlobalScore_slutt),
             !is.na(CIA30GlobalScore_start),
             !Over18_start) %>%
      mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      mutate(var = ifelse((CIA30GlobalScore_start-CIA30GlobalScore_slutt) >= 7,
                          1, 0),
             denominator = 1) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 70
    tittel <- c("Andelen med bedring i",  "funksjon/livskvalitet (BU)")
  }



  if (ind_id == "norspis_KI3_undervektsreduksjon") {

    Indikator <- RegData %>%
      mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                          MedIsoBMIBGS_start, MedBMI_start),
             bmi_slutt = ifelse(!is.na(MedIsoBMIBGS_slutt),
                                MedIsoBMIBGS_slutt, MedBMI_slutt)) %>%
      filter(!is.na(bmi),
             bmi < 18.5,
             !is.na(bmi_slutt)) %>%
      mutate(var = ifelse(bmi_slutt >= 18.5, 1, 0),
             denominator = 1,
             year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 60
    tittel <- c("Andelen med endring av undervektstatus",  "til ikke undervektig ved slutt")
  }


  if (ind_id == "norspis_KI4_utfallsvurdering_V") {

    Indikator <- RegData %>%
      filter(!is.na(PT03Utfallsvurd),
             PT03Utfallsvurd != 9,
             Over18_start) %>%
      mutate(var = ifelse(PT03Utfallsvurd %in% 1:2, 1, 0),
             denominator = 1,
             year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 50
    tittel <- c("Andelen som rapporterer Klar bedring",  "eller Ikke noe problem lenger (V)")
  }

  if (ind_id == "norspis_KI4_utfallsvurdering_BU") {

    Indikator <- RegData %>%
      filter(!is.na(PT03Utfallsvurd),
             PT03Utfallsvurd != 9,
             !Over18_start) %>%
      mutate(var = ifelse(PT03Utfallsvurd %in% 1:2, 1, 0),
             denominator = 1,
             year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 70
    tittel <- c("Andelen som rapporterer Klar bedring",  "eller Ikke noe problem lenger (BU)")
  }

  if (ind_id == "norspis_involvering_famlie_venner_BU") {

    Indikator <- RegData %>%
      filter(!is.na(PT02BleInvolv) & PT02BleInvolv!= 9,
             !Over18_start) %>%
      mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric(),
             var = PT02BleInvolv,
             denominator = 1) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 100
    tittel <- c("Andelen som rapporterer at familie og/eller",  "venner ble involvert i behandlingen (BU)")
  }

  if (ind_id == "norspis_involvering_famlie_venner_V") {

    Indikator <- RegData %>%
      filter(PT01OnsketInvolv == 1,
             !is.na(PT02BleInvolv) & PT02BleInvolv != 9,
             Over18_start) %>%
      mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric(),
             var = PT02BleInvolv,
             denominator = 1) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 100
    tittel <- c("Andelen som rapporterer at familie og/eller",  "venner ble involvert i behandlingen (V)")
  }

  if (ind_id == "norspis_undervekt_prodprove") {

    Indikator <- RegData %>%
      mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                          MedIsoBMIBGS_start, MedBMI_start)) %>%
      filter(!is.na(bmi),
             bmi < 18.5,
             MedBlodprove_start %in% c(0,1)) %>%
      mutate(var = MedBlodprove_start,
             denominator = 1,
             year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige pasienter (BMI < 18,5) ved ", "start, der det er tatt blodprøver ved start.")
  }

  if (ind_id == "norspis_oppkast_prodprove") {

    Indikator <- RegData %>%
      filter(!is.na(EDE16GgrOppkast_start),
             MedBlodprove_start %in% c(0,1)) %>%
      mutate(var = MedBlodprove_start,
             denominator = 1,
             year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 90
    tittel <- c("Andelen med alvorlig eller ekstremt oppkast ved", "start, der det er tatt blodprøver ved start.")
  }

  if (ind_id == "norspis_beintetthetsmaling_V") {

    Indikator <- RegData %>%
      mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                          MedIsoBMIBGS_start, MedBMI_start),
             BeintetthMaling = ifelse(MedBeintetthMaling_start == 1 |
                                        MedBeintetthMaling_slutt == 1,
                                      1, 0)) %>%
      filter(Over18_start,
             bmi < 18.5,
             MedBeintetthMaling_start %in% 0:1 |
               MedBeintetthMaling_slutt %in% 0:1) %>%
      mutate(BeintetthMaling = ifelse(is.na(BeintetthMaling), 0, BeintetthMaling)) %>%
      mutate(var = BeintetthMaling,
             denominator = 1,
             year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige ved start der det", "oppgis at det er gjort en beintetthetsmåling (V)")
  }

  if (ind_id == "norspis_beintetthetsmaling_BU") {

    Indikator <- RegData %>%
      mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                          MedIsoBMIBGS_start, MedBMI_start),
             BeintetthMaling = ifelse(MedBeintetthMaling_start == 1 |
                                        MedBeintetthMaling_slutt == 1,
                                      1, 0)) %>%
      filter(!Over18_start,
             bmi < 18.5,
             MedBeintetthMaling_start %in% 0:1 |
               MedBeintetthMaling_slutt %in% 0:1) %>%
      mutate(BeintetthMaling = ifelse(is.na(BeintetthMaling), 0, BeintetthMaling)) %>%
      mutate(var = BeintetthMaling,
             denominator = 1,
             year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
      select(AvdRESH, year, var, denominator, Kortnavn, AvdBUV_start) %>%
      rename(SykehusNavn = Kortnavn) %>%
      mutate(ind_id = ind_id,
             context = "caregiver")
    maal <- 90
    tittel <- c("Andelen undervektige ved start der det", "oppgis at det er gjort en beintetthetsmåling (BU)")
  }



  indikatordata <- list(indikator=Indikator, tittel=tittel, terskel=terskel,
                        minstekrav=minstekrav, maal=maal, skriftStr=skriftStr,
                        pktStr=pktStr, legPlass=legPlass, minstekravTxt=minstekravTxt,
                        maalTxt=maalTxt, decreasing=decreasing,
                        width=width, height=height, maalretn=maalretn)


}
