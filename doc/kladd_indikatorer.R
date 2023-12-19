library(norspis)
library(dplyr)
library(tidyr)
rm(list = ls())

resh_voksen_barn <-
  readxl::read_xlsx("~/mydata/norspis/RESH, navn, BUV, org.nr. 18.12.23.xlsx",
                    sheet = 1)
resh_voksen_barn <- resh_voksen_barn[match(unique(resh_voksen_barn$RESH),
                                           resh_voksen_barn$RESH), ]
variabeloversikt <- readxl::read_xlsx("~/mydata/norspis/Variabler i forløp v2.xlsx",
                                      sheet = 1)


AlleScorer <- queryAlleScorer("norspis")
EnkeltLeddNum <- queryEnkeltLeddNum("norspis")
ForlopsOversikt <- norspis::queryForlopsOversikt("norspis")

AlleScorer <- AlleScorer[ , setdiff(names(AlleScorer),
        variabeloversikt$Variabelnavn[variabeloversikt$Tabell == "AlleScorer" &
                                        variabeloversikt$`Start/slutt/utgått` == "Utgått"])]
EnkeltLeddNum <- EnkeltLeddNum[ , setdiff(names(EnkeltLeddNum),
                                    variabeloversikt$Variabelnavn[variabeloversikt$Tabell == "EnkeltLeddNum" &
                                                                    variabeloversikt$`Start/slutt/utgått` == "Utgått"])]

RegData <- merge(EnkeltLeddNum,
                 ForlopsOversikt[, c("ForlopsID", "ForlopsType1Num",
                                     "ForlopsType1", "BasisRegStatus",
                                     "PasientAlder", "Fodselsdato")],
                 by = "ForlopsID") %>%
  filter(BasisRegStatus == 1) %>%
  merge(AlleScorer, by = "ForlopsID") %>%
  merge(resh_voksen_barn[, c("RESH", "Barn/unge (BU) eller voksen (V)",
                             "Kortnavn i årsrapporten", "Org.nr.")],
        by.x = "AvdRESH", by.y = "RESH", all.x = TRUE) %>%
  rename(AvdBUV = `Barn/unge (BU) eller voksen (V)`,
         Kortnavn = `Kortnavn i årsrapporten`,
         orgnr = Org.nr.) %>%
  mutate(AvdBUV = ifelse(AvdRESH == 109979 & ForlopsType1Num %in% c(1,3,5,7,98),
                         "V", AvdBUV),
         Over18 = ifelse(PasientAlder >= 18, TRUE, FALSE))

RegData_start <- RegData[RegData$ForlopsType1Num %in% 3:4, ]
RegData_start <- RegData_start[ , setdiff(names(RegData_start),
                                          variabeloversikt$Variabelnavn[variabeloversikt$`Start/slutt/utgått` == "Ikke start"])]
RegData_slutt <- RegData[RegData$ForlopsType1Num %in% 5:6, ]
RegData_slutt <- RegData_slutt[ , setdiff(names(RegData_slutt),
                                          variabeloversikt$Variabelnavn[variabeloversikt$`Start/slutt/utgått` == "Ikke slutt"])]
RegData_slutt <- RegData_slutt[ , setdiff(names(RegData_slutt),
                                          c("AvdRESH", "Kortnavn",
                                            "PasientID", "SykehusNavn",
                                            "orgnr"))]



RegData <- merge(RegData_start, RegData_slutt,
                 by.x = "ForlopsID", by.y = "RegTilhorendeStartReg",
                 suffixes = c("_start", "_slutt"),
                 all.x = TRUE)


indikatordata <- norspisBeregnIndikator(RegData, ind_id = "norspis_KI4_utfallsvurdering_BU")
norspisPlotIndikator(indikatordata)

# tmp <- RegData %>% filter(ForlopsID %in% (RegData %>% summarise(N=n(), .by = ForlopsID) %>% filter(N>1) %>% select(ForlopsID) %>% unlist()))

### Indikator 1: endring i symptomer

Indikator1 <- RegData %>%
  filter(!is.na(EDEQ60GlobalScore_start),
         !is.na(EDEQ60GlobalScore_slutt)) %>%
  mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
  mutate(var = ifelse((EDEQ60GlobalScore_start-EDEQ60GlobalScore_slutt) >= 0.9,
                      1, 0),
         denominator = 1) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)

### Indikator 5: involvering av familie, under 18

Indikator5 <- RegData %>%
  filter(!is.na(PT02BleInvolv) & PT02BleInvolv!= 9,
         !Over18_start) %>%
  mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric(),
         var = PT02BleInvolv,
         denominator = 1) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)


### Indikator 6: involvering av familie, over 18

Indikator6 <- RegData %>%
  filter(PT01OnsketInvolv == 1,
         !is.na(PT02BleInvolv) & PT02BleInvolv != 9,
         Over18_start) %>%
  mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric(),
         var = PT02BleInvolv,
         denominator = 1) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)

### Indikator 7: blodprøver ved undervekt

Indikator7 <- RegData %>%
  mutate(bmi = ifelse(!is.na(MedIsoBMIBGS_start),
                      MedIsoBMIBGS_start, MedBMI_start)) %>%
  filter(!is.na(bmi),
         bmi < 18.5,
         MedBlodprove_start %in% c(0,1)) %>%
  mutate(var = MedBlodprove_start,
         denominator = 1,
         year = format(RegHendelsesdato_start, "%Y") %>% as.numeric(),) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)


### Indikator 8: blodprøver ved oppkast

Indikator8 <- RegData %>%
  filter(!is.na(EDE16GgrOppkast_start),
         MedBlodprove_start %in% c(0,1)) %>%
  mutate(var = MedBlodprove_start,
         denominator = 1,
         year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)


### Indikator 9: beintetthetsmåling voksne

Indikator9 <- RegData %>%
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
  select(AvdRESH, year, var, MedBeintetthMaling_start,
         MedBeintetthMaling_slutt, denominator, SykehusNavn, AvdBUV_start)


### Indikator 10: beintetthetsmåling barn

Indikator10 <- RegData %>%
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
  select(AvdRESH, year, var, MedBeintetthMaling_start,
         MedBeintetthMaling_slutt, denominator, SykehusNavn, AvdBUV_start)


### Indikator 2: endring funksjon

Indikator2 <- RegData %>%
  filter(!is.na(CIA30GlobalScore_slutt),
         !is.na(CIA30GlobalScore_start)) %>%
  mutate(year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
  mutate(var = ifelse((CIA30GlobalScore_start-CIA30GlobalScore_slutt) >= 7,
                      1, 0),
         denominator = 1) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)


### Indikator 3: endring undervektsstatus

Indikator3 <- RegData %>%
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
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)


### Indikator 4: pasientvurdert bedring

Indikator4 <- RegData %>%
  filter(!is.na(PT03Utfallsvurd),
         PT03Utfallsvurd != 9) %>%
  mutate(var = ifelse(PT03Utfallsvurd %in% 1:2, 1, 0),
         denominator = 1,
         year = format(RegHendelsesdato_slutt, "%Y") %>% as.numeric()) %>%
  select(AvdRESH, year, var, denominator, SykehusNavn, AvdBUV_start)





aux <- Indikator4 %>%
  summarise(antall = sum(var),
            N = n(),
            andel = antall/N*100,
            .by = c(SykehusNavn, year))

aux %>% pivot_wider(id_cols = SykehusNavn,
                    names_from = year,
                    values_from = andel)














