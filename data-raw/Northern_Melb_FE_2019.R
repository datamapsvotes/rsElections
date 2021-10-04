north_melb_electorate_outlines <-  dplyr::filter(sf::st_read("data-raw/federal election 2019/national-esri-fe2019/COM_ELB_region.shp"),Elect_div %in% c("Calwell", "Mcewen", "Scullin", "Jagajaga", "Wills","Cooper"))
north_melb_polling_booths <- utils::read.csv("data-raw/federal election 2019/GeneralPollingPlacesDownload-24310.csv", skip=1)
north_melb_TCP_results <- utils::read.csv("data-raw/federal election 2019/HouseTcpByCandidateByPollingPlaceDownload-24310.csv", skip=1)

Northern_Melb_FE_2019 <- list(electorate_outlines = north_melb_electorate_outlines, polling_booths = north_melb_polling_booths, TCP_results = north_melb_TCP_results)

usethis::use_data(Northern_Melb_FE_2019, overwrite = TRUE)
