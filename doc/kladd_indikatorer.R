library(norspis)
library(dplyr)
library(tidyr)
rm(list = ls())

# resh_voksen_barn <-
#   readxl::read_xlsx("~/mydata/norspis/RESH, navn, BUV, org.nr. 18.12.23.xlsx",
#                     sheet = 1)
# resh_voksen_barn <- resh_voksen_barn[match(unique(resh_voksen_barn$RESH),
#                                            resh_voksen_barn$RESH), ] %>%
#   rename(AvdRESH = RESH,
#          Kortnavn = `Kortnavn i årsrapporten`,
#          AvdBUV = `Barn/unge (BU) eller voksen (V)`,
#          orgnr = Org.nr.) %>%
#   select(AvdRESH, Kortnavn, AvdBUV, orgnr)
#
# write.csv(resh_voksen_barn, "~/mydata/norspis/tmp.csv", row.names = F)

resh_voksen_barn <-
  tibble::tribble(
    ~AvdRESH, ~Kortnavn, ~AvdBUV, ~orgnr,
    106854,"NKS Jæren DPS","V",333333,
    4210626,"HSYK: Allm.pol. BU Mosjøen","BU",875326562,
    4210303,"ST. OLAVS: Spes.pol. V","V",883974832,
    4208548,"DIAKONSYK: Allm.pol. V","V",974116804,
    700698,"UNN: Reg.døgn BU","BU",974547031,
    707383,"AHUS: EFS Spes.pol. V","V",974705168,
    4207041,"AHUS: EFS Spes.døgn V","V",974705168,
    105806,"HNT: Reg. V","V",974754142,
    700821,"NLSH: Reg. V","V",974795345,
    4210562,"NLSH: Spes.pol. BU","BU",975296008,
    4210825,"HSYK: Allm.pol. V Mosjøen","V",975326551,
    4204191,"CAPIO: Spes.døgn BU","BU",980524493,
    4208300,"SIHF: Spes.døgn V","V",983971709,
    4204275,"SOHF: Spes.døgn V","V",983971768,
    4207697,"HNT: MHOBY","V",983974791,
    102152,"HSYK: Allm.pol. V Sandnessjøen","V",983974929,
    102154,"HSYK: Allm.pol. BU Sandnessjøen","BU",983974929,
    104083,"SSHF: Spes.døgn V","V",983975240,
    105008,"SSHF: Allm.pol. V Arendal/Froland","V",983975240,
    108462,"SSHF Allm.pol. V Solvang","V",983975240,
    104364,"SIV: Spis.pol. V","V",983975259,
    4209009,"SIV: Spes.pol. BU","BU",983975259,
    109979,"OUS: Reg. BU","BU",987547243,
    107026,"HB: Reg. V","V",991992677,
    110361,"OUS: Spes.pol. V","V",998158923)


variabeloversikt <- readxl::read_xlsx("~/mydata/norspis/Variabler i forløp v2.xlsx",
                                      sheet = 1) %>%
  rename(status = `Start/slutt/utgått`,
         OppfolgingNum = `Også i tabellen OppfolgingNum`) %>%
  mutate(status = case_when(status == "Ikke slutt" ~ "IkkeSlutt",
                            status == "Ikke start" ~ "IkkeStart",
                            status == "Utgått" ~ "Utgaatt"))


AlleScorer <- queryAlleScorer("norspis")
EnkeltLeddNum <- queryEnkeltLeddNum("norspis")
ForlopsOversikt <- norspis::queryForlopsOversikt("norspis")

AlleScorer <- AlleScorer[ , setdiff(names(AlleScorer),
                                    variabeloversikt$Variabelnavn[variabeloversikt$Tabell == "AlleScorer" &
                                                                    variabeloversikt$status == "Utgaatt"])]
EnkeltLeddNum <- EnkeltLeddNum[ , setdiff(names(EnkeltLeddNum),
                                          variabeloversikt$Variabelnavn[variabeloversikt$Tabell == "EnkeltLeddNum" &
                                                                          variabeloversikt$status == "Utgaatt"])]

RegData <- merge(EnkeltLeddNum,
                 ForlopsOversikt[, c("ForlopsID", "ForlopsType1Num",
                                     "ForlopsType1", "BasisRegStatus",
                                     "PasientAlder", "Fodselsdato")],
                 by = "ForlopsID") %>%
  filter(BasisRegStatus == 1) %>%
  merge(AlleScorer, by = "ForlopsID") %>%
  merge(resh_voksen_barn[, c("AvdRESH", "AvdBUV",
                             "Kortnavn", "orgnr")],
        by = "AvdRESH", all.x = TRUE) %>%
  mutate(AvdBUV = ifelse(AvdRESH == 109979 & ForlopsType1Num %in% c(1,3,5,7,98),
                         "V", AvdBUV),
         Kortnavn = ifelse(AvdRESH == 109979 & ForlopsType1Num %in% c(1,3,5,7,98),
                           "OUS: Reg. V", Kortnavn),
         Over18 = ifelse(PasientAlder >= 18, TRUE, FALSE))

RegData_start <- RegData[RegData$ForlopsType1Num %in% 3:4, ]
RegData_start <- RegData_start[ , setdiff(names(RegData_start),
                                          variabeloversikt$Variabelnavn[variabeloversikt$status == "IkkeStart"])]
RegData_slutt <- RegData[RegData$ForlopsType1Num %in% 5:6, ]
RegData_slutt <- RegData_slutt[ , setdiff(names(RegData_slutt),
                                          variabeloversikt$Variabelnavn[variabeloversikt$status == "IkkeSlutt"])]
RegData_slutt <- RegData_slutt[ , setdiff(names(RegData_slutt),
                                          c("AvdRESH", "Kortnavn",
                                            "PasientID", "SykehusNavn",
                                            "orgnr"))]

RegData <- merge(RegData_start, RegData_slutt,
                 by.x = "ForlopsID", by.y = "RegTilhorendeStartReg",
                 suffixes = c("_start", "_slutt"),
                 all.x = TRUE)


indikatordata <- norspisBeregnIndikator(RegData, ind_id = "norspis_involvering_famlie_venner_V")
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
         year = format(RegHendelsesdato_start, "%Y") %>% as.numeric()) %>%
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














