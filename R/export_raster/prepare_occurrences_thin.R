# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)
export_path <-
  paste0(getwd(), "/../export/schuzka2-total-gbif-ndop4/")

source(paste0(getwd(), "/R/export_raster/functions.R"))
source(paste0(getwd(), "/R/export_raster/gbif.R"))
source(paste0(getwd(), "/R/export_raster/ndop_divland.R"))
source(paste0(getwd(), "/R/export_raster/prepare_occurrences.R"))
source(paste0(getwd(), "/R/export_raster/ndop_top.R"))


ndop_top <- ndop_top(paste0(getwd(), "/species/ndop/ndop-top-2021-03-21.xlsx"))
# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
# species <- pull(ndop_top %>% select(species1, species2))
species <- ndop_top %>% select(species1, species2)

# species_u <- gsub(" ", "_", species)
px_size <- c(100) # 100, 200, 1000, 2000, 10000


# předem si načíst .csv nálezů do proměnných a předávat rovnou je!
set_cols1 <-
  cols(
    gbifID = "c",
    coordinateUncertaintyInMeters = "d",
    coordinatePrecision = "d",
    day = "i",
    month = "i",
    year = "i"
  )
ptaci_gbif <-
  read_tsv(
    paste0(
      getwd(),
      "/../new-species/gbif/0209125-200613084148143-redukce4.csv"
    ),
    col_types = set_cols1
  )

set_cols2 <-
  cols(
    ID_ND_NALEZ = "c",
    DRUH = "c",
    AUTOR = "c",
    DATUM_OD = col_date("%Y-%m-%d"),
    DATUM_DO = col_date("%Y-%m-%d"),
    CXLOKAL_TYP = "c",
    NEGATIVNI = "i",
    VEROH = "i",
    PRESNOST = "i"
  )
ptaci_ndop <-
  read_csv(
    paste0(
      getwd(),
      "/../new-species/ndop/ptaci_ndop_reduction/ptaci_ndop_utf8-redukce.csv"
    ),
    col_types = set_cols2
  )

for (px_size_item in px_size) {
  for (sindex in 1:nrow(species)) {
    gc()

    res_ndop <-
      ndop_divland(
        list(from = "2016-01-01", to = "2020-12-31"),
        list(from = 4, to = 6),
        paste0(getwd(), "/../new-species/ndop/ptaci_ndop_reduction"),
        NULL,
        px_size_item,
        ptaci_ndop
      )
    # res_ndop <- ndop(list(from = '2016-01-01', to = '2020-12-31'), list(from = 4, to = 6), paste0(getwd(), "/../ndop/csv"), NULL, px_size)
    # print(as_tibble(res_ndop), n = 10)
    gc()

    res_gbif <-
      gbif(
        list(from = "2016-01-01", to = "2020-12-31"),
        list(from = 4, to = 6),
        paste0(getwd(), "/../new-species/gbif"),
        "0209125-200613084148143-redukce4.csv",
        NULL,
        px_size_item,
        ptaci_gbif
      )
    # res_gbif <- gbif(list(from = '2016-01-01', to = '2020-12-31'), list(from = 4, to = 6), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv", NULL, px_size)
    # print(as_tibble(res_gbif), n = 10)

    gc()

    s2 <- species[sindex, 2]

    if (!is.na(species[sindex, 2])) {
      s2 <- as.character(species[sindex, 2])
    }

    occ_prepared <-
      prepare_occurrences(
        c(as.character(species[sindex, 1]), s2),
        (px_size_item / 1000),
        paste0(export_path, "species/"),
        res_ndop,
        res_gbif,
        3035
      )
  }
}