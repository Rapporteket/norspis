#Script for Årsrapport 2021 (made by Carina 2022)


#Load packages----

library(tidyverse)
library(dplyr)
library(ggplot2)


#Load data ----
NorSpisForlop <- norspis::queryForlopsOversikt("norspis")
View(NorSpisForlop)
NorSpisEnkeltledd <- norspis::queryEnkeltLeddNum("norspis")
View(NorSpisEnkeltledd)
NorSpisAlleScorer <- norspis::queryAlleScorer("norspis")
View(NorSpisAlleScorer)
RegDataBeh <- norspis::queryBehandlingNum("norspis")
View(RegDataBeh)


#Select and adjust variables:"Fodselsdato", "KryptertFnr", "AvdodDato", "PasientAlder"----
NorSpisForlop <- norspis::queryForlopsOversikt("norspis") %>%
  dplyr::select(-c("Fodselsdato", "KryptertFnr", "AvdodDato"))
View(NorSpisForlop)

NorSpisForlop$PasientAlder <- round(NorSpisForlop$PasientAlder, digits = 0)
view(NorSpisForlop)


# Merge data ----
# Merge data from NorSpisForlop, NorSpisEnkeltledd and NorSpisAlleScorer and use suffixes to create
#only one "ForlopsID" within the merged dataset.

RegData <- merge(NorSpisForlop, NorSpisAlleScorer, suffixes = c('','y'),
                 by = "ForlopsID", all = FALSE) %>%
  merge(NorSpisEnkeltledd, suffixes = c('','X'), by = "ForlopsID", all = FALSE) %>%
  tibble::as_tibble() %>% dplyr::mutate(ForlopsID = as.numeric(ForlopsID))
View(RegData)

#Tibble RESH&AvdNavn----
RegDataBeh <- norspis::queryBehandlingNum("norspis") %>% tibble::as_tibble()

map_avdresh_avdnavn <-
  tibble::tribble(
    ~AvdRESH,      ~AvdNavn,
    104083, 'SSHF: Spes.Døgn.',
    104364, 'SIV: Spis.Pol.',
    4204275, 'SOHF: Spes.Døgn.',
    4208300, 'SIHF: Spes.Døgn.',
    4208548, 'Diakonsyk: Allm.Pol.',
    105806, 'HNT: Reg.',
    109979, 'OUS: Reg.',
    110361, 'OUS: Spes.Pol.',
    700698, 'UNN: Reg.Døgn.',
    700821, 'NLSH: Reg.',
    707383, 'AHUS: EFS.Spes.Pol.',
    4204191, 'CAPIO: Spes.Døgn.',
    4207041, 'AHUS: EFS.Spes.Døgn.',
    4209009, 'SIV: Spes.Pol.',
    4210562, 'NLSH: Spes.Pol.',
    4210626, 'HSYK: Allm.Pol.',
    4210825,  'HSYK: Allm.Pol.',
    107026, 'HB: Reg. ',
    4210303, 'St.Olavs: Spes.Pol.',
    4207697, "HNT: MHOBY",
  )
count(map_avdresh_avdnavn)
view(map_avdresh_avdnavn)


#Adding a new variable named "AvdNavn" in the data of RegData
RegData$AvdNavn <- map_avdresh_avdnavn$AvdNavn[match(RegData$AvdRESH, map_avdresh_avdnavn$AvdRESH)]
view(RegData$AvdNavn)

#Dividing VOP & BUP ----

#Dividing RASP in BUP & VOP:

#OUS_BUP:
RegData$AvdNavn[RegData$AvdNavn=="OUS: Reg." & RegData$ForlopsType1Num %in% c(99,8,6,4,2)]<-"OUS: Reg.B"

#OUS_VOP:
RegData$AvdNavn[RegData$AvdNavn=="OUS: Reg." & RegData$ForlopsType1Num %in% c(98,1,3,5,7)]<-"OUS: Reg.V"
table(RegData$ForlopsType1, RegData$ForlopsType1Num)
view(RegData)

#BUP:
RegDataBeh <- norspis::queryBehandlingNum("norspis") %>% tibble::as_tibble()

mapBUP_avdresh_BUPavdnavn <-
  tibble::tribble(
    ~AvdRESH,      ~BUPAvdNavn,
    109979, 'OUS: Reg.B.',
    700698, 'UNN: Reg.Døgn.',
    4204191, 'CAPIO: Spes.Døgn.',
    4209009, 'SIV: Spes.Pol.',
    4210562, 'NLSH: Spes.Pol.',
    4210626, 'HSYK: Allm.Pol.B.',
  )

count(mapBUP_avdresh_BUPavdnavn)
view(mapBUP_avdresh_BUPavdnavn)

#Adding a new variable named "AvdNavn" in the data of RegData
RegData$BUPAvdNavn <- mapBUP_avdresh_BUPavdnavn$BUPAvdNavn[match(RegData$AvdRESH, mapBUP_avdresh_BUPavdnavn$AvdRESH)]
view(RegData)


#VOP:
RegDataBeh <- norspis::queryBehandlingNum("norspis") %>% tibble::as_tibble()

mapVOP_avdresh_VOPavdnavn <-
  tibble::tribble(
    ~AvdRESH,      ~VOPAvdNavn,
    104083, 'SSHF: Spes.Døgn.',
    104364, 'SIV: Spis.Pol.',
    4204275, 'SOHF: Spes.Døgn.',
    4208300, 'SIHF: Spes.Døgn.',
    4208548, 'Diakonsyk: Allm.Pol.',
    105806, 'HNT: Reg.',
    109979, 'OUS: Reg.V',
    110361, 'OUS: Spes.Pol.',
    700821, 'NLSH: Reg.',
    707383, 'AHUS: EFS. Spes.Pol.',
    4207041, 'AHUS: EFS. Spes.Døgn.',
    4210825,  'HSYK: Allm.Pol.V.',
    107026, 'HB: Reg.',
    4210303, 'St.Olavs: Spes.Pol.',
    4207697, "HNT: MHOBY",
  )
count(mapVOP_avdresh_VOPavdnavn)
view(mapVOP_avdresh_VOPavdnavn)


#Adding a new variable named "VOPAvdNavn" in the data of RegData
RegData$VOPAvdNavn <- mapVOP_avdresh_VOPavdnavn$VOPAvdNavn[match(RegData$AvdRESH, mapVOP_avdresh_VOPavdnavn$AvdRESH)]
view(RegData)



#KI4:Pasientvurdert utbytte

#frequency table:
install.packages("plyr")
library(plyr)
library(dplyr)
table(RegData$PT03Utfallsvurd)
table(RegData$AvdNavn)

KI4_FTable <- table(RegData$PT03Utfallsvurd, RegData$AvdNavn)
View(KI4_FTable)


RegData %>%
  group_by(PT03Utfallsvurd)
  summarise(freq=n()) %>%
    arrange(rel.freq= count/sum(count))
