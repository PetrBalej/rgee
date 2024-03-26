###
# 1) stažení nálezů z NDOP
###


# https://opengeolabs.github.io/qgis-ndop-downloader/
# bin/ndop -h

wd <- "C:/Users/Balej/Documents/ndop_downloader2"

# adresář a název sloupce
wd.cd <- "druh" # "celed" "druh"

wd <- paste0(wd, "/", wd.cd, "/")


library(tidyverse)
library(magrittr)
library(readxl)
library(readr)

seznam.druhu <- read_excel(paste0(wd, "seznam_druhu.xlsx"))
seznam.druhu <- as_tibble(seznam.druhu)
seznam.druhu[[wd.cd]] %<>% as.factor
seznam.druhu$skupina %<>% as.factor
seznam.druhu$reakce_pozitivni %<>% as.factor

# !!!
# nutná instalace (respektive stažení a instalace z lokálu + úprava datumu v set_search_payload.R [rfDatumOd]) vlastní úpravené verze knihovny rndop, řeší některé chyby
# library(devtools)
# install_github("PetrBalej/rndop", force = TRUE, ref="quickFix", build=TRUE)

library(rndop)
isop_login()
# .Renviron v aktuálním adresáři?
# NDOP_USER='xxx'
# NDOP_PWD='xxx'

for (taxon in seznam.druhu[[wd.cd]]) {
  print(taxon)

  # plus omezení datumem posledních 5 let - nutno dát napevno do set_search_payload.R [rfDatumOd]
  if (wd.cd == "druh") {
    csv <- ndop_download(taxon)
  } else {
    csv <- ndop_download(NA, taxon)
  }

  write_csv(csv, paste0(wd, taxon, ".csv"))
}

# u čeledí je musím domapovat podle názvu souboru .csv - v exportu čeleď není!
# plus nahradím název druhu čeledí
if (wd.cd == "celed") {
  csv_list <-
    list.files(
      wd,
      pattern = paste0("csv$"),
      ignore.case = TRUE,
      full.names = FALSE
    )

  for (csv.name in csv_list) {
    csv.item <- read_csv(paste0(wd, csv.name))

    if (nrow(csv.item) < 1) {
      file.rename(paste0(wd, csv.name), paste0(wd, csv.name, "X")) # přejmenuju ať se dále nezpracovává jako csv, jen pro zachování infa o nestažených čeledích
      next
    }

    csv.item$celed <- substr(csv.name, 1, nchar(csv.name) - 4)
    csv.item$DRUH2 <- csv.item$DRUH
    csv.item$DRUH <- csv.item$celed # nahradím názvy druhů čeledí
    # přepíšu původní
    write_csv(csv.item, paste0(wd, csv.name))
  }
}



###
# 2) lehká dofiltrace a exporty
###
source(paste0(wd, "../ndop_ugc.R")) # bacha na určení desetinné čárky či tečky podle toho co je na vstupu!

ndop.parsed <-
  ndop_ugc(
    years_range = list(from = "2019-01-01", to = "2024-12-31"),
    season_months_range = list(from = 1, to = 12),
    import_path_ndop = wd,
    res_crs = 5514,
    presicion = 1000
  )


ndop.parsed$SITMAP %<>% as.factor
ndop.parsed$GARANCE %<>% as.factor
ndop.parsed$VALIDACE %<>% as.factor
ndop.parsed$VEROH %<>% as.factor


print(nrow(ndop.parsed))
ndop.parsed %<>% filter(!is.na(X)) %>% filter(!is.na(Y))
print(nrow(ndop.parsed))

# # exporty
# write_csv(ndop.parsed, paste0(wd,"ndop-", wd.cd,".v2.csv"))
# saveRDS(ndop.parsed, paste0(wd,"ndop-", wd.cd,".v2.rds"))
# ndop.parsed.geo <- ndop.parsed %>% st_as_sf(coords = c("X", "Y"), crs = 5514)
# st_write(ndop.parsed.geo, paste0(wd,"ndop-", wd.cd,".v2.shp"))


ndop.v1 <- ndop.parsed

ndop.v1.druhy <- ndop.v1 %>%
  group_by(DRUH) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
print(ndop.v1.druhy, n = 300)


# jen druhy nad 100 nálezů
ndop.v1.druhy100 <- ndop.v1.druhy %>% filter(n > 100)
ndop.v1.druhy100 <- as.vector(unlist(ndop.v1.druhy100$DRUH))

ndop.v1 %<>% filter(DRUH %in% ndop.v1.druhy100)



library(lubridate)
ndop.v1.roky <- ndop.v1 %>%
  group_by(year(DATUM_OD)) %>%
  summarise(n = n())
print(ndop.v1.roky, n = 300)
plot(ndop.v1.roky)





#
# UPDATE 2023-11-13, početnější druhy trvají při zpracování ecospat.occ.desaggregation delší dobu, i 15 minut... to vše pro každou minDist
#

library(sf)
library(ecospat)
library(tidyverse)
minDists <- c(1:3, 5, 10) * 1000 # m
# ndop.v1 <- readRDS("/home/petr/Downloads/delete/DM/ndop_v1/ndop.v1.rds")

# kvůli ecospat.occ.desaggregation
ndop.v1$x <- ndop.v1$X
ndop.v1$y <- ndop.v1$Y

occs.thinned <- list()

ndop.v1.druh <- as.data.frame(ndop.v1 %>% dplyr::select(DRUH, x, y) %>% group_by(DRUH) %>% dplyr::distinct(x, y))
for (minDist in minDists) {
  print(paste0(rep("-", 100), collapse = ""))
  print(minDist)
  occs.thinned[[as.character(minDist)]] <- ecospat.occ.desaggregation(xy = ndop.v1.druh, min.dist = minDist, by = "DRUH")

  md <- as.character(minDist)
  # uložení průběžných výstupů per minDist
  write_csv(occs.thinned[[md]], paste0(wd, "ndop.v2_", md, ".csv"))
  saveRDS(occs.thinned[[md]], paste0(wd, "ndop.v2_", md, ".rds"))
  geometry <- occs.thinned[[md]] %>% st_as_sf(coords = c("x", "y"), crs = 5514)
  st_write(geometry, paste0(wd, "ndop.v2_", md, ".shp"))

  druhy.stat <- occs.thinned[[md]] %>%
    group_by(DRUH) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  # rychlá statistika po každém thinningu
  print(minDist)
  print(druhy.stat, n = 300)
}
saveRDS(occs.thinned, paste0(wd, "ndop.v2_all.rds"))
