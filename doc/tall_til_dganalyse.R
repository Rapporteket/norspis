#Preparation of data to "dekningsgrad"
library(dplyr)
rm(list = ls())

#READ 1
NorSpisForlop <- rapbase::loadRegData(registryName = "data",
                                      query = "SELECT * FROM forlopsoversikt") |>
  dplyr::select("AvdRESH","ForlopsID" ,
                "HovedDato", "BasisRegStatus",
                "ForlopsType1Num", "ForlopsType1") |>
  dplyr::mutate(ForlopsID = as.numeric(ForlopsID))


#READ 2
NorSpisEnkeltLeddNum <-
  rapbase::loadRegData(registryName = "data",
                       query = "SELECT * FROM enkeltleddnum") |>
  dplyr::select("PasientID", "RegHendelsesdato",
                "RegHenvMottattDato", "ForlopsID",
                "RegRegtype") |>
  dplyr::mutate(ForlopsID = as.numeric(ForlopsID))



#READ 3
#PID, SSN
NorSpisKoblingstabell <-
  read.table("C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norspis/NorSpis_koblingstabell_datadump_25.03.2025.csv",
             sep=';',
             header=T, encoding = 'UTF-8', stringsAsFactors = FALSE, fill = TRUE,
             col.names = c("PID", "SSN") , #variables to read
             colClasses = c("character"))  |>  #"character" so that leading zeros are kept
  mutate(SSN = dplyr::case_when(nchar(SSN) == 6 ~ paste0(SSN,"00000"),
                                TRUE ~ SSN))  |>
  mutate(PID = as.integer(PID))


#MERGE and filter basisRegStatus and date and regtype:
NorSpisEnkeltLeddNum_JOINED <-
  left_join(NorSpisEnkeltLeddNum,
            NorSpisForlop,
            by= "ForlopsID") |>
  filter(BasisRegStatus == 1,
         RegHendelsesdato >= "2023-01-01" & RegHendelsesdato <= "2024-12-31",
         RegRegtype %in% c(1,2,3,4))

#WRITE FILE :
koblingsfil <- NorSpisKoblingstabell |>
  dplyr::filter(PID %in% NorSpisEnkeltLeddNum_JOINED$PasientID)

write.csv2(koblingsfil,
           file ="C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norspis/koblingsfil.csv",
           row.names = FALSE)


#MAKE FILE 2:
aktivitetsdata <- NorSpisEnkeltLeddNum_JOINED %>%
  select(PasientID,
         AvdRESH,
         HovedDato,
         RegHenvMottattDato) |>
  mutate(AvdRESH = as.numeric(AvdRESH)
  ) |>
  rename(RESHid = AvdRESH,
         hoved_dato = HovedDato,
         henvisning_mottatt_dato = RegHenvMottattDato
  )

#WRITE FILE 2:
write.csv2(aktivitetsdata,
           file ="C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norspis/aktivitetsdata.csv",
           row.names = FALSE) #excludes first row

