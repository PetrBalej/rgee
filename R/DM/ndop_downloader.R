###
# 1) stažení nálezů z NDOP
###


# https://opengeolabs.github.io/qgis-ndop-downloader/
# bin/ndop -h

wd <- "C:/Users/Petr/Documents/ndop_downloader/"

library(tidyverse)
library(magrittr)
library(readxl)

seznam.druhu <- read_excel(paste0(wd,"seznam_druhu.xlsx"))
seznam.druhu <- as_tibble(seznam.druhu)
seznam.druhu$druh_lat_ndop %<>% as.factor
seznam.druhu$skupina %<>% as.factor
seznam.druhu$reakce_pozitivni %<>% as.factor

path.ndop.downloader <- "C: && cd C:/Users/balej/AppData/Roaming/QGIS/QGIS3/profiles/default/python/plugins/ndop_downloader"
path.python <- "C:/Users/balej/AppData/Local/Microsoft/WindowsApps/python.exe"

# uložení přihlašovacích údajů do "C:\Users\USER_NAME\.ndop.cfg"
# bin/ndop --user "user_name" --password "heslo" -s

use.cfg <- FALSE

credentials <- ""

if(!use.cfg){
  ndop.user <- "user_name"
  ndop.heslo <- "heslo"
    
  credentials <- paste0("--user \"", ndop.user, 
                        "\" --password \"", ndop.heslo,
                        "\"")
}


for(taxon in seznam.druhu$druh_lat_ndop){
  print(taxon)
    
  cmd <- paste0(path.ndop.downloader, " && \"", path.python, 
                "\" bin/ndop  ", credentials ," --output \"", wd, "export/", taxon,
                "\" --taxon \"", taxon,"\"")
  # -loc_only 
  # přestal fungovat (při spuštění přes R shell), proč?!
  
  
  shell(cmd)
}


###
# 2) lehká dofiltrace a exporty
###


source(paste0(wd, "ndop_ugc.R"))

ndop.parsed <-
  ndop_ugc(years_range = list(from = "1990-01-01", to = "2023-12-31"),
           season_months_range = list(from = 1, to = 12),
           import_path_ndop = paste0(wd,"/export"),
           res_crs = 5514,
           presicion = 1000) 


ndop.parsed$SITMAP %<>% as.factor
ndop.parsed$GARANCE %<>% as.factor
ndop.parsed$VALIDACE %<>% as.factor
ndop.parsed$VEROH %<>% as.factor


# # exporty
# write_csv(ndop.parsed, "ndop.v1.csv")
# saveRDS(ndop.parsed, "ndop.v1.rds")
# ndop.parsed.geo <- ndop.parsed %>% st_as_sf(coords = c("X", "Y"), crs = 5514)
# st_write(ndop.parsed.geo, "ndop.v1.shp")


ndop.v1.druhy <- ndop.v1 %>% 
  group_by(DRUH) %>% 
  summarise(n = n()) %>% arrange(desc(n))
print(ndop.v1.druhy, n = 100)


library(lubridate)
ndop.v1.roky <- ndop.v1 %>% 
  group_by(year(DATUM_OD)) %>% 
  summarise(n = n()) 
print(ndop.v1.roky, n = 100)
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
  write_csv(occs.thinned[[md]], paste0("ndop.v1_", md, ".csv"))
  saveRDS(occs.thinned[[md]], paste0("ndop.v1_", md, ".rds"))
  geometry <- occs.thinned[[md]] %>% st_as_sf(coords = c("x", "y"), crs = 5514)
  st_write(geometry, paste0("ndop.v1_", md, ".shp"))

  druhy.stat <- occs.thinned[[md]] %>%
    group_by(DRUH) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  # rychlá statistika po každém thinningu
  print(minDist)
  print(druhy.stat, n = 100)
}
saveRDS(occs.thinned, "ndop.v1_all.rds")
