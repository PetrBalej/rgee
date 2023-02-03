# https://opengeolabs.github.io/qgis-ndop-downloader/
# bin/ndop -h

wd <- "C:/Users/balej/Documents/ndop_downloader/"

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

# uložení pøihlašovacích údajù do "C:\Users\USER_NAME\.ndop.cfg"
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
  # pøestal fungovat (pøi spuštìní pøes R shell), proè?!
  
  shell(cmd)
}
