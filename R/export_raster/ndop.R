ndop <- function(years_range = list(from = '2017-01-01', to = '2019-12-31'), season_months_range = list(from = 4, to = 7), import_path_ndop = "/../ndop/csv", import_path_ndop = "/../ndop/csv") {

  # kontrola (do)instalace všech dodatečně potřebných balíčků
  required_packages <- c("tidyverse", "sf", "lubridate")
  install.packages(setdiff(required_packages, rownames(installed.packages())))

  library(tidyverse)
  library(lubridate)

  # # # # # # # # # # # # # # # # # # # # # #
  # nastavení základních parametrů [start]  #
  # # # # # # # # # # # # # # # # # # # # # #

  ## časové rozsahy

  # Rozmezí datumů (sezóny a let) musí být stejné jako u filtru prediktorů! Ideálně přebírat společnou hodnotu jednoho parametru?

  # rozsah snímků od/do
  # years_range <- list(from = '2017-01-01', to = '2019-12-31')

  # rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
  # season_months_range <- list(from = 4, to = 7)


  # adresář pro exportované csv z NDOP pro další zpracování (pomocí QGIS pluginu https://github.com/OpenGeoLabs/qgis-ndop-downloader)
  # import_path_ndop <- paste0(getwd(), "/../ndop/csv")

  # # # # # # # # # # # # # # # # # # # # # #
  # nastavení základních parametrů [konec]  #
  # # # # # # # # # # # # # # # # # # # # # #


  set_cols <- cols(PORADI = "i", ID_LOKAL = "c", STRUKT_POZN = "c", DATUM_OD = col_date("%Y%m%d"), DATUM_DO = col_date("%Y%m%d"), VEROH = "i", ID_NALEZ = "i")

  # csv_ndop_ll <- read_csv(paste0(import_path_ndop, "/Locustella_luscinioides_tab.csv"), col_types = set_cols, locale = locale("cs", decimal_mark = ","))

  # načte všechny *.csv z import_path_ndop
  csv_ndop_ll <- list.files(path = import_path_ndop, pattern = "*.csv", full.names = T) %>%
  map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  filter(X != "<i>Skrytá lokalizace</i>") %>%
  type_convert(col_types = set_cols, locale = locale("cs", decimal_mark = ","))

  # vypíše špatně rozparsované řádky
  # problems(csv_ndop_ll)

  # csv_ndop_ll <- as_tibble(csv_ndop_ll) # netřeba?

  # VEROH: 0, 1, 3
  # VALIDACE: věrohodný záznam, méně věrohodný záznam, záznam k opravě
  # NEGATIV: 0, 1
  # & GARANCE == "Garantováno" & VEROH == 0 & NEGATIV == 0 
  # VEROH == 1 odpovídá GARANCE == "Garantováno" ???

  # přesnost by měla být nižší hodnota než velikost cellsize prediktoru => měnit dynamicky?

  # základní dofiltrovaní nálezů z NDOPu
  csv_ndop_ll_filter <- csv_ndop_ll %>%
  filter(DATUM_OD >= years_range$from & DATUM_OD <= years_range$to &
    DATUM_DO >= years_range$from & DATUM_DO <= years_range$to &
    between(month(DATUM_OD), season_months_range$from, season_months_range$to) &
    between(month(DATUM_DO), season_months_range$from, season_months_range$to) &
    (VEROH == 1 | VALIDACE == "věrohodný záznam") &
    NEGATIV == 0 &
    CXPRESNOST <= 100)

  #
  # převod souřadnic z S-JTSK do WGS 84 (přidání nových sloupců: lat, lon) a filtrace polygonem (Česko)
  #

  library(sf)

  # načtení shapefile polygonu Česka (časem možná možnost i předání parametrem jiný shapefile nebo geometrii polygonu)
  shpPath <- "shp/ne_50m_admin_0_countries/czechia/cz_4326.shp" # zjednodušený polygon Česka
  czechia <- st_read(shpPath)

  # převod souřadnic S-JTSK do WGS84
  wgs84 <- csv_ndop_ll_filter %>%
  st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
  st_transform(4326)

  # označení záznamů se souřadnicemi uvnitř polygonu Česka (T/F) a přidání jako samostatného sloupce
  wgs84_czechia <- wgs84$geometry %>%
  st_intersects(czechia) %>% length > 0
  csv_ndop_ll_filter <- csv_ndop_ll_filter %>% mutate(wgs84_czechia)

  # vytvoření sloupců s WGS84 souřadnicemi - nebo raději jako sf geometrii typu POINT?
  wgs84_coords <- wgs84 %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lat = Y, lon = X)

  options(pillar.sigfig = 7) # jen pro případnou vizualizaci

  # přidání sloupců s WGS84 souřadnicemi, výběr záznamů z polygonu a potřebných sloupců
  csv_ndop_ll_s_wgs84 <- csv_ndop_ll_filter %>%
  mutate(wgs84_coords) %>%
  filter(wgs84_czechia == TRUE) %>%
  select(ID_NALEZ, DRUH, lat, lon) %>%
  rename(key = ID_NALEZ, scientificName = DRUH, decimalLatitude = lat, decimalLongitude = lon)

  # print(as_tibble(csv_ndop_ll_s_wgs84), n = 10)
  return(csv_ndop_ll_s_wgs84)
}
