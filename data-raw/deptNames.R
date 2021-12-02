## code to prepare `deptNames` dataset goes here

reshId <- c(105806,
            109979,
            110361,
            700698,
            700821,
            707383,
            4204191,
            4207041,
            4209009,
            4210562,
            4210626,
            4210825,
            107026)

deptName <- c("RKSF, Levanger, Helse Nord-Trøndelag HF",
              "RASP, OUS HF",
              "Spiseforstyrrelsespoliklinikken, Gaustad, OUS HF",
              "RSS, UNN HF",
              "RESSP, NLSH HF",
              "Follo, enhet for SF, poliklinikk, Akershus u. HF",
              "Capio Anoreksi Senter",
              "Follo, Enhet for SF, døgn, Akershus u. HF",
              "BUPA poliklinikk Nordre Vestfold, SiV HF",
              "BUP, Bodø, NLSH HF",
              "BUP, Mosjøen, NLSH HF",
              "DPS,Mosjøen, NLSH HF",
              "Seksjon for SF, Helse Bergen HF")

deptNames <- data.frame(reshId = reshId, deptName = deptName)

usethis::use_data(deptNames, overwrite = TRUE)
