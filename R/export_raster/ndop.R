# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("tidyverse", "sf")
install.packages(setdiff(required_packages, rownames(installed.packages())))

library(tidyverse)

# adresář pro exportované csv z NDOP pro další zpracování (pomocí QGIS pluginu https://github.com/OpenGeoLabs/qgis-ndop-downloader)
import_path_ndop <- paste0(getwd(), "/../ndop/csv")

set_cols <- cols(PORADI = "i", ID_LOKAL = "i", STRUKT_POZN = "c", DATUM_OD = col_date("%Y%m%d"), DATUM_DO = col_date("%Y%m%d"), VEROH = "i", ID_NALEZ = "i")

csv_ndop_ll <- read_csv(paste0(import_path_ndop, "/Locustella_luscinioides_tab.csv"), col_types = set_cols, locale = locale("cs", decimal_mark = ","))

# vypíše špatně rozparsované řádky
problems(csv_ndop_ll)

# csv_ndop_ll <- as_tibble(csv_ndop_ll) # netřeba?

# VEROH: 0, 1, 3
# VALIDACE: věrohodný záznam, méně věrohodný záznam, záznam k opravě
# NEGATIV: 0, 1
# & GARANCE == "Garantováno" & VEROH == 0 & NEGATIV == 0 
# VEROH == 1 odpovídá GARANCE == "Garantováno" ???

# Rozmezí datumů (sezóny a let) musí být stejné jako u filtru prediktorů! - Dodělat!!! - Ideálně přebírat společnou hodnotu jednoho parametru?

# přesnost by měla být nižší hodnota než velikost cellsize prediktoru => měnit dynamicky?

# základní dofiltrovaní nálezů z NDOPu
csv_ndop_ll_filter <- csv_ndop_ll %>% filter(DATUM_OD > "2015-01-01" & VEROH == 1 & NEGATIV == 0 & CXPRESNOST > 100)

#
# převod souřadnic z S-JTSK do WGS 84 (přidání nových sloupců: lat, lon) a filtrace polygonem (Česko)
#

library(sf)

# načtení shapefile polygonu Česka
shpPath <- "shp/ne_50m_admin_0_countries/czechia/cz_4326.shp"
czechia <- st_read(shpPath)

# převod souřadnic S-JTSK do WGS84
wgs84 <- csv_ndop_ll_filter %>%
  st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
  st_transform(4326) 

# označení záznamů se souřadnicemi uvnitř polygonu Česka (T/F) a přidání jako samostatného sloupce
wgs84_czechia <- wgs84$geometry %>% 
  st_intersects(czechia) %>% lengths > 0 
csv_ndop_ll_filter <- csv_ndop_ll_filter %>% mutate(wgs84_czechia)

# vytvoření sloupců s WGS84 souřadnicemi - nebo raději jako sf geometrii typu POINT?
wgs84_coords <- wgs84 %>%
  st_coordinates() %>% 
  as_tibble()  %>%
  rename(lat = Y, lon = X)

options(pillar.sigfig = 7) # jen pro případnou vizualizaci

# přidání sloupců s WGS84 souřadnicemi, výběr záznamů z polygonu a potřebných sloupců
csv_ndop_ll_s_wgs84 <- csv_ndop_ll_filter %>% 
  mutate(wgs84_coords) %>% 
  filter(wgs84_czechia == TRUE) %>% 
  select(PORADI, DRUH, lat, lon)