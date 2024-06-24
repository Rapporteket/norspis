library(norspis)
library(dplyr)
rm(list = ls())

rap_aar <- 2023

norspisdata <- norspisLesOgProsesser()
RegData <- norspisdata$RegData
ForlopsOversikt <- norspisdata$ForlopsOversikt
SkjemaOversikt <- norspisdata$SkjemaOversikt

tabfolder <- paste0("~/mydata/norspis/indikator_shuskvalitet/")
if (!dir.exists(tabfolder)) {
  dir.create(tabfolder)
}

ind_id <- c("Symptomreduksjon EDEQ, voksne" = "norspis_KI1_symptomreduksjon_EDEQ_V",
            "Symptomreduksjon EDEQ, barn/ungdom" = "norspis_KI1_symptomreduksjon_EDEQ_BU",
            "Bedring i livskvalitet, voksne" = "norspis_KI2_funksjonsbedring_CIA_V",
            "Bedring i livskvalitet, barn/ungdom" = "norspis_KI2_funksjonsbedring_CIA_BU",
            "Endring i undervektsstatus, voksne" = "norspis_KI3_undervektsreduksjon_V",
            "Endring i undervektsstatus, barn/ungdom" = "norspis_KI3_undervektsreduksjon_BU",
            "Utfallsvurdering, voksne" = "norspis_KI4_utfallsvurdering_V",
            "Utfallsvurdering, barn/ungdom" = "norspis_KI4_utfallsvurdering_BU",
            "Familie/venner involvert i behandlingen, voksne" = "norspis_involvering_familie_venner_V",
            "Familie/venner involvert i behandlingen, barn/ungdom" = "norspis_involvering_familie_venner_BU",
            "Blodprøver ved start, undervektige, voksne" = "norspis_undervekt_blodprove_V",
            "Blodprøver ved start, undervektige, barn/ungdom" = "norspis_undervekt_blodprove_BU",
            "Blodprøver ved start, oppkast, voksne" = "norspis_oppkast_blodprove_V",
            "Blodprøver ved start, oppkast, barn/ungdom" = "norspis_oppkast_blodprove_BU",
            "Beintetthetsmåling ved start, undervektige voksne" = "norspis_beintetthetsmaling_V",
            "Beintetthetsmåling ved start, undervektige barn/ungdom" = "norspis_beintetthetsmaling_BU")



indikatordata <- norspis::norspisBeregnIndikator(RegData = RegData,
                                                 ind_id = ind_id[1])
TabellData <- indikatordata$indikator
Indikator <- TabellData[which(TabellData$year <= rap_aar), ]

for (i in 2:length(ind_id)) {
  indikatordata <- norspis::norspisBeregnIndikator(RegData = RegData,
                                                   ind_id = ind_id[i])
  TabellData <- indikatordata$indikator
  Indikator <- dplyr::bind_rows(Indikator, TabellData[which(TabellData$year <= rap_aar), ])
}

Indikator <- merge(Indikator, norspis::resh_voksen_barn[, c("AvdRESH", "orgnr")], by = "AvdRESH") %>%
  dplyr::select(orgnr, year, var, denominator, ind_id, context)


DG2023 <- readxl::read_xls("~/mydata/norspis/NORSPIS - DG til sykehusveiviseren 2023.xls",
                           sheet = 1, col_names = T) %>%
  select(orgnr, year, Teller, Nevner) %>%
  rename(var = Teller,
         denominator = Nevner) %>%
  filter(denominator != "-") %>%
  mutate(ind_id = "norspis_dg",
         context = "caregiver",
         var = as.numeric(var),
         denominator = as.numeric(denominator))

DG_gml <- read.csv2("~/mydata/norspis/norspis_dg_gml.csv") %>%
  dplyr::filter(ind_id == "norspis_dg")

DG <- dplyr::bind_rows(DG_gml, DG2023)

write.csv2(DG, "~/mydata/norspis/norspis_dg2023.csv", row.names = F)


Indikator <- dplyr::bind_rows(Indikator, DG2023)

write.csv2(Indikator, paste0(tabfolder, "indikatorer_norspis_", Sys.Date(), ".csv"),
           row.names = F)

