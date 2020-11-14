# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("tidyverse")
install.packages(setdiff(required_packages, rownames(installed.packages())))

library(tidyverse)

# adresář pro exportované csv z NDOP pro další zpracování (pomocí QGIS pluginu https://github.com/OpenGeoLabs/qgis-ndop-downloader)
import_path_ndop <- paste0(getwd(), "/../ndop/csv")

# csv_list <- list.files(path = import_path_ndop, pattern="*.csv", full.names = TRUE)
# csv_ndop <- lapply(csv_list, read.delim)
# ll <- csv_ndop[1]

set_cols <- cols(PORADI = "i", ID_LOKAL = "i", STRUKT_POZN = "c", DATUM_OD = col_date("%Y%m%d"), DATUM_DO = col_date("%Y%m%d"))

csv_ndop_ll <- read_csv(paste0(import_path_ndop, "/Locustella_luscinioides_tab.csv"), col_types = set_cols)

problems(csv_ndop_ll)

csv_ndop_ll <- as_tibble(csv_ndop_ll)

csv_ndop_ll_f <- filter(csv_ndop_ll, DATUM_OD > "2019-01-01")

csv_ndop_ll_s <- select(csv_ndop_ll_f, PORADI, DRUH, DATUM_OD)

# 
# převod souřadnic z S-JTSK do WGS 84
# 