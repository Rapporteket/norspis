
NorSpisForlop <- norspis::queryForlopsOversikt("norspis") %>%
  dplyr::select(-c("Fodselsdato", "KryptertFnr", "AvdodDato"))
NorSpisForlop$PasientAlder <- round(NorSpisForlop$PasientAlder, digits = 1)
NorSpisEnkeltledd <- norspis::queryEnkeltLeddNum("norspis")
NorSpisAlleScorer <- norspis::queryAlleScorer("norspis")
#Merge data
RegData <- merge(NorSpisForlop, NorSpisAlleScorer, suffixes = c('','y'),
                 by = "ForlopsID", all = FALSE) %>%
  merge(NorSpisEnkeltledd, suffixes = c('','X'), by = "ForlopsID", all = FALSE) %>%
  tibble::as_tibble() %>% dplyr::mutate(ForlopsID = as.numeric(ForlopsID))

RegDataBeh <- norspis::queryBehandlingNum("norspis") %>% tibble::as_tibble()

map_avdresh_avdnavn <-
  tibble::tribble(
    ~AvdRESH,      ~AvdNavn,        ~orgnr,
    105806, 'Levanger (regional)', 974754142,
    109979, 'Oslo (regional)', 987547243,
    110361, 'Oslo, Gaustad (spes.pol.)', 998158923,
    700698, 'Tromsø (regional, BU)', 974547031,
    700821, 'Bodø (regional, V)', 974795345,
    707383, 'Ski (DPS, pol.)', 974705168,
    4204191, 'Fredrikstad (Capio, døgn)', 980524493,
    4207041, 'Ski (DPS, døgn)', 974705168,
    4209009, 'Tønsberg (BUP, s.team)', 975294714,
    4210562, 'Bodø (BUP, s.team)', 975296008,
    4210626, 'Mosjøen (BUP)', 875326562,
    4210825,  'Mosjøen (DPS)', 975326551,
    107026, 'Bergen (regional)', 991992677,
    4210303, 'Tr.heim (spes.pol.)', 974748959,
    4207697, "Stjørdal (DPS)", 884038162,
    104083, "Post C SSA", 999,
    4204275, "DPS Halden-Sarpsborg", 999,
    4208548, "Vinderen", 999
  )

#############

RegData$AvdNavn <- map_avdresh_avdnavn$AvdNavn[match(RegData$AvdRESH, map_avdresh_avdnavn$AvdRESH)]

#2:
RegDataBeh <- norspis2::fun2_2_RegDataBeh_newVarGlobal(myInData1=RegData, myInData2=RegDataBeh)

#1:
RegData <- norspis2::fun2_1_2_RegData_newVarFrmt(myInData=RegData)
RegData <- norspis2::fun2_1_3_RegData_newVarMAsNA(myInData = RegData)
RegData <- norspis2::fun2_1_4_RegData_newVarDich(myInData=RegData)

#4:
RegDataStartEnd <- norspis2::fun2_4_1_RegDataStartEnd(RegData)
RegDataStartEnd <- norspis2::fun2_4_2_RegDataStartEnd_newVar(RegDataStartEnd)


################

# Filer til RP: Sykehusnavn og RESH
#filter data
data <-
  norspis2::fun3_2_filter_RegDataStartEnd(
    RegData = RegDataStartEnd,
    dateFrom.y =
      "2012-01-01",
    dateTo.y =
      "2021-12-31",
    BasisRegStatus.y = 1) #only complete reg
#filter data, only end
data_only_end <-
  norspis2::fun3_1_filter_RegData(
    RegData = RegData,
    dateFrom =
      "2012-01-01",
    dateTo =
      "2021-12-31",
    regStatus = 1)#only complete reg
#Make table
output_tibble_KI1_3 <- tibble() #just making an empty tibble where output of the "for loop" can go
for(vars in c(quo(EDEQ60GlobalScore_CHANGE_PROP),
              quo(CIA30GlobalScore_RCI_01),
              quo(MedBMI_start18.5_slutt18.5)
              #,
              #quo(PROP_PT03Utfallsvurd)
)
){
  table_KI1_3 <-
    data %>%
    group_by(Year.y, AvdNavn.y) %>%
    summarise(var = sum(!!vars, na.rm=T),
              denominator = sum(!is.na(!!vars))) %>%
    #add indicator name
    mutate(ind_id = as.character(rlang::quo_get_expr(vars))) %>%
    rename(Year= "Year.y",
           AvdNavn = "AvdNavn.y")
  output_tibble_KI1_3 <-
    dplyr::bind_rows(
      output_tibble_KI1_3,
      table_KI1_3)
}
table_KI4 <-
  data_only_end %>%
  group_by(Year, AvdNavn) %>%
  summarise(
    var = sum(PROP_PT03Utfallsvurd, na.rm = T),
    denominator = sum(!is.na(PROP_PT03Utfallsvurd))) %>%
  #add indicator name
  mutate(ind_id = "norspis_KI4_utfallsvurdering")
data_to_resultatportalen_KI1_4 <-
  rbind(output_tibble_KI1_3,
        table_KI4)%>%
  relocate(ind_id) %>%
  mutate(ind_id= case_when(ind_id == "EDEQ60GlobalScore_CHANGE_PROP" ~ "norspis_KI1_symptomreduksjon_EDEQ",
                           ind_id == "CIA30GlobalScore_RCI_01" ~ "norspis_KI2_funksjonsbedring_CIA",
                           ind_id == "MedBMI_start18.5_slutt18.5" ~ "norspis_KI3_undervektsreduksjon",
                           ind_id == "norspis_KI4_utfallsvurdering" ~ "norspis_KI4_utfallsvurdering")
  )

#join orgnr
data_to_resultatportalen_KI1_4_orgnr <-
  left_join(data_to_resultatportalen_KI1_4,
            dplyr::select(map_avdresh_avdnavn, -AvdRESH),
            by ="AvdNavn") %>%
  relocate(c("orgnr","var","denominator"), .before=AvdNavn ) %>%
  rename(year = "Year")
#write csv file
# write.table(
#   data_to_resultatportalen_KI1_4_orgnr,
#   file = "F:/2020 (2021) to resultatportalen/data_to_resultatportalen_KI1_4.csv",
#   sep=';',
#   col.names = NA)



# Til RP: Indikatorene (KI1, KI2 og KI3)

#data
# RegData <- import11_data()
#
# RegData <- make21_data_newVarGlobal()
# RegData <- make22_data_newVarFrmt()
# RegData <- make23_data_newVar()
# RegData <- make24_data_newVar01()
# RegDataNatVal <- make25_dataNatVal() #used in KI4 and KI5
# RegDataStartEnd <- make26_dataStartEnd()
# RegDataStartEnd <- make27_dataStartEnd_NewVar()
# RegDataStartEndNatVal <- make28_dataStartEnd_NatVal()#used in KI1, KI2 and KI3
# #indikatorfil KI1:
# NorSpisTilOffEDEQForbedretFriske <-
#   RegDataStartEnd %>%
#   filter (HovedDato_FRMT.y <= "2019-12-31", BasisRegStatus.y==1) %>%
#   mutate(Indikator= "Ind1",
#          "Nevner Ind1" = 1,
#          "Teller Ind1" = EDEQ60GlobalScore_CHANGE_PROP) %>%
#   select(Year.y,Indikator, "Nevner Ind1", "Teller Ind1",AvdRESH.x) %>%
#   mutate(AarID = paste0(Year.y,AvdRESH.x)) %>%
#   rename(Aar=Year.y, ReshID=AvdRESH.x) %>%
#   filter(!is.na(.[, "Teller Ind1"]))
# # write.table(NorSpisTilOffEDEQForbedretFriske, file = "F:/Til Resultatportalen 2020/NorSpisTilOffEDEQForbedretFriske.csv", sep=';', col.names = NA)
# #indikatorfil KI2:
# NorSpisTilOffCIAForbedretFriske <-
#   RegDataStartEnd %>%
#   filter (HovedDato_FRMT.y <= "2019-12-31", BasisRegStatus.y==1) %>%
#   mutate(Indikator= "Ind2",
#          "Nevner Ind2" = 1,
#          "Teller Ind2" = CIA30GlobalScore_RCI_01) %>%
#   select(Year.y,Indikator, "Nevner Ind2", "Teller Ind2",AvdRESH.x) %>%
#   mutate(AarID = paste0(Year.y,AvdRESH.x)) %>%
#   rename(Aar=Year.y, ReshID=AvdRESH.x) %>%
#   filter(!is.na(.[, "Teller Ind2"]))
# # write.table(NorSpisTilOffCIAForbedretFriske, file = "F:/Til Resultatportalen 2020/NorSpisTilOffCIAForbedretFriske.csv", sep=';', col.names = NA)
# #indikatorfil KI3:
# NorSpisTilOffIkkeUndervektSlutt <-
#   RegDataStartEnd %>%
#   filter (HovedDato_FRMT.y <= "2019-12-31", BasisRegStatus.y==1) %>%
#   mutate(Indikator= "Ind3",
#          "Nevner Ind3" = 1,
#          "Teller Ind3" = MedBMI_start18.5_slutt18.5) %>%
#   select(Year.y,Indikator, "Nevner Ind3", "Teller Ind3",AvdRESH.x) %>%
#   mutate(AarID = paste0(Year.y,AvdRESH.x)) %>%
#   rename(Aar=Year.y, ReshID=AvdRESH.x) %>%
#   filter(!is.na(.[, "Teller Ind3"]))
# # write.table(NorSpisTilOffIkkeUndervektSlutt, file = "F:/Til Resultatportalen 2020/NorSpisTilOffIkkeUndervektSlutt.csv", sep=';', col.names = NA)
#
#
# # Til RP: Indikatorene (KI4 og KI5)
#
# #indikatorfil KI4:
# RegData <- import11_data()
#
# RegData <- make21_data_newVarGlobal()
# RegData <- make22_data_newVarFrmt()
# RegData <- make23_data_newVar()
# RegData <- make24_data_newVar01()
# #KI4
# NorSpisTilOffPO09Utbytte <-
#   RegData %>%
#   filter(HovedDato_FRMT<="2019-12-31", #HovedDato_FRMT>="2019-01-01",
#          BasisRegStatus == 1) %>%
#   mutate(Indikator= "Ind4",
#          "Nevner Ind4" = 1,
#          "Teller Ind4" = PROP_PO09Utbytte) %>%
#   select(Year,Indikator, "Nevner Ind4", "Teller Ind4",AvdRESH) %>%
#   mutate(AarID = paste0(Year,AvdRESH)) %>%
#   rename(Aar=Year, ReshID=AvdRESH) %>%
#   filter(!is.na(.[, "Teller Ind4"]))
# # write.table(NorSpisTilOffPO09Utbytte, file = "F:/Til Resultatportalen 2020/NorSpisTilOffPO09Utbytte.csv", sep=';', col.names = NA)
# #KI5
# NorSpisTilOffPT03Utfallsvurd <-
#   RegData %>%
#   filter(HovedDato_FRMT<="2019-12-31", #HovedDato_FRMT>="2019-01-01",
#          BasisRegStatus == 1) %>%
#   mutate(Indikator= "Ind5",
#          "Nevner Ind5" = 1,
#          "Teller Ind5" = PROP_PT03Utfallsvurd) %>%
#   select(Year,Indikator, "Nevner Ind5", "Teller Ind5",AvdRESH) %>%
#   mutate(AarID = paste0(Year,AvdRESH)) %>%
#   rename(Aar=Year, ReshID=AvdRESH) %>%
#   filter(!is.na(.[, "Teller Ind5"]))
# # write.table(NorSpisTilOffPT03Utfallsvurd, file = "F:/Til Resultatportalen 2020/NorSpisTilOffPT03Utfallsvurd.csv", sep=';', col.names = NA)
