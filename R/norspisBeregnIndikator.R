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



  indikatordata <- list(indikator=Indikator, tittel=tittel, terskel=terskel,
                        minstekrav=minstekrav, maal=maal, skriftStr=skriftStr,
                        pktStr=pktStr, legPlass=legPlass, minstekravTxt=minstekravTxt,
                        maalTxt=maalTxt, decreasing=decreasing,
                        width=width, height=height, maalretn=maalretn)



}
