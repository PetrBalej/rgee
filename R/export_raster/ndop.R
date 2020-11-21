# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("tidyverse", "sf")
install.packages(setdiff(required_packages, rownames(installed.packages())))

library(tidyverse)

# adresář pro exportované csv z NDOP pro další zpracování (pomocí QGIS pluginu https://github.com/OpenGeoLabs/qgis-ndop-downloader)
import_path_ndop <- paste0(getwd(), "/../ndop/csv")

set_cols <- cols(PORADI = "i", ID_LOKAL = "i", STRUKT_POZN = "c", DATUM_OD = col_date("%Y%m%d"), DATUM_DO = col_date("%Y%m%d"), VEROH = "i", ID_NALEZ = "i")

csv_ndop_ll <- read_csv(paste0(import_path_ndop, "/Locustella_luscinioides_tab.csv"), col_types = set_cols, locale = locale("cs", decimal_mark = ","))

problems(csv_ndop_ll)

# csv_ndop_ll <- as_tibble(csv_ndop_ll) # netřeba?

# VEROH: 0, 1, 3
# VALIDACE: věrohodný záznam, méně věrohodný záznam, záznam k opravě
# NEGATIV: 0, 1
# & GARANCE == "Garantováno" & VEROH == 0 & NEGATIV == 0 
# VEROH == 1 odpovídá GARANCE == "Garantováno" ???

# Rozmezí datumů (sezóny a let) musí být stejné jako u filtru prediktorů! - Dodělat! - Ideálně přebírat společnou hodnotu jednoho parametru?

# přesnost by měla být nižší hodnota než velikost cellsize prediktoru => měnit dynamicky?

csv_ndop_ll_filter <- csv_ndop_ll %>% filter(DATUM_OD > "2015-01-01" & VEROH == 1 & NEGATIV == 0 & CXPRESNOST > 100)

csv_ndop_ll_selected_cols <- select(csv_ndop_ll_filter, PORADI, DRUH, DATUM_OD, X, Y)

# 
# převod souřadnic z S-JTSK do WGS 84 (přidání nových sloupců: lat, lon)
# 

library(sf)

wqs84 <- csv_ndop_ll_filter %>%
  st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()  %>%
  rename(lat = Y, lon = X)

# Dodělat filtraci polygonem Česka.

options(pillar.sigfig = 7) # jen pro případnou vizualizaci
csv_ndop_ll_s_wgs84 <- csv_ndop_ll_filter %>% mutate(wqs84) %>% select(PORADI, DRUH, lat, lon)