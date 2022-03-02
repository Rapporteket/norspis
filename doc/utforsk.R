
aux <- ForlopsOversikt %>%
  dplyr::filter(BasisRegStatus %in% 1) %>%
  dplyr::group_by(PasientID, ForlopsType1) %>%
  dplyr::summarise(antall = dplyr::n()) %>%
  dplyr::filter(antall > 1)

flerforlop<- ForlopsOversikt[ForlopsOversikt$PasientID %in% aux$PasientID, ]

utforsk <- flerforlop[, c("AvdRESH", "SykehusNavn", "PasientID", "Avdod", "AvdodDato",
                          "ForlopsType1", "ForlopsType1Num", "ForlopsID", "BasisRegStatus",
                          "HovedDato")] %>%
  dplyr::filter(ForlopsType1 %in% "Startregistrering voksen") %>%
  merge(ForlopsOversikt[ForlopsOversikt$ForlopsType1 %in% "Sluttregistrering voksen",
                        c("PasientID", "Avdod", "AvdodDato",
                          "ForlopsID", "BasisRegStatus",
                          "HovedDato")],
        by = "PasientID", suffixes = c("_start", "_slutt"), all.x = TRUE) %>%
  dplyr::filter(BasisRegStatus_start == 1 & BasisRegStatus_slutt == 1) %>%
  dplyr::filter(HovedDato_slutt > HovedDato_start) %>%
  dplyr::mutate("tid_start_slutt" = difftime(HovedDato_slutt, HovedDato_start, units = "days")) %>%
  dplyr::mutate("tid_start_slutt_diffaar" = abs(tid_start_slutt-365))

tmp <- utforsk %>% dplyr::group_by(ForlopsID_start) %>%
  dplyr::summarise(ForlopsID = ForlopsID_slutt[tid_start_slutt_diffaar == min(tid_start_slutt_diffaar)],
                   tid_start_slutt_diffaar = min(tid_start_slutt_diffaar))




SkjemaOversikt_forlop <-
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 1,
                       c("ForlopsType1", "ForlopsType1Num", "SkjemaStatus",
                         "ForlopsID", "HovedDato", "shusnavn")],
        SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 3,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_bakgrunn"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 5,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_sterk_svak"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 11,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_scl90"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 13,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_rand36"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 15,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_kidscreen27"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 17,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_ede_q"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 19,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_cia3"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 21,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_pas_erfaring"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 23,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_pas_tilfreds"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 25,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_multiaksial"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 27,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_diag_voksen"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 29,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_med_oppl"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 31,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_honos"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 33,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_honosca"), all = T) %>%
  merge(SkjemaOversikt[SkjemaOversikt$SkjemaRekkeflg == 35,
                       c("SkjemaStatus", "ForlopsID")], by = "ForlopsID",
        suffixes = c("", "_beh_oppsum"), all = T) %>%
  merge(forlopsoversikt[, c("ForlopsID", "ForlopsType1", "ForlopsType1Num")],
        by = "ForlopsID", all = T)







