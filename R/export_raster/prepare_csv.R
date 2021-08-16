

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


#
# NDOP
#

# iconv -f Windows-1250 ptaci_ndop.csv > ptaci_ndop_utf8.csv

# set_cols <- cols(
#     ID_ND_NALEZ = "i", DRUH = "c", AUTOR = "c", DATUM_OD = col_date("%d.%m.%Y"), DATUM_DO = col_date("%d.%m.%Y"), CXLOKAL_TYP = "c", X = "i", Y = "i",  NEGATIVNI = "i", VEROH = "i", PRESNOST = "i", VALIDACE = "c",
#     ID_ND_LOKALIZACE = "_", LOKALITA  = "_", GARANCE = "_", POZN_GAR = "_", POZN_VAL = "_")
# ptaci_ndop <- read_csv(paste0(getwd(), "/../new-species/ndop/ptaci_ndop/ptaci_ndop_utf8.csv"), col_types = set_cols) # , locale = locale(encoding = "latin2")
# #write_csv(ptaci_ndop, "ptaci_ndop_utf8-redukce.csv")


# ptaci_ndop_cc <- ptaci_ndop %>% filter(DRUH == "Cinclus cinclus")
# ptaci_ndop_cc_f <- ptaci_ndop_cc %>% filter(AUTOR != "iNaturalist uživatel" & CXLOKAL_TYP == "B" & VALIDACE == "validováno - věrohodný záznam" & NEGATIVNI == 0 & PRESNOST <= 1000)


#
# GBIF
#

# gbifID - nelze dát "i" - má moc dlouhé integery...
set_cols <-
  cols(
    gbifID = "c",
    datasetKey = "_",
    occurrenceID = "_",
    kingdom = "_",
    phylum = "_",
    class = "_",
    order = "_",
    family = "_",
    genus = "_",
    species = "c",
    infraspecificEpithet = "_",
    taxonRank = "_",
    scientificName = "_",
    verbatimScientificName = "_",
    verbatimScientificNameAuthorship = "_",
    countryCode = "_",
    locality = "_",
    stateProvince = "_",
    occurrenceStatus = "_",
    individualCount = "_",
    publishingOrgKey = "_",
    decimalLatitude = "d",
    decimalLongitude = "d",
    coordinateUncertaintyInMeters = "d",
    coordinatePrecision = "d",
    elevation = "_",
    elevationAccuracy = "_",
    depth = "_",
    depthAccuracy = "_",
    eventDate = "_",
    day = "i",
    month = "i",
    year = "i",
    taxonKey = "_",
    speciesKey = "_",
    basisOfRecord = "_",
    institutionCode = "_",
    collectionCode = "_",
    catalogNumber = "_",
    recordNumber = "_",
    identifiedBy = "_",
    dateIdentified = "_",
    license = "_",
    rightsHolder = "_",
    recordedBy = "_",
    typeStatus = "_",
    establishmentMeans = "_",
    lastInterpreted = "_",
    mediaType = "_",
    issue = "_"
  )
gbif <-
  read_tsv(
    paste0(getwd(), "/../new-species/gbif/0209125-200613084148143.csv"),
    col_types = set_cols,
    quote = ""
  )
write_tsv(gbif, "0209125-200613084148143-redukce4.csv")

# # 
# # statistika záznamů podle zemí (sloupec countyCode)
# # 
# # výše ještě změnit: 
# # countryCode = "c",
# csv_gbif_cc <- gbif %>%
#   dplyr::filter(
#     (coordinateUncertaintyInMeters <= 300 | is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters == "NA" | coordinateUncertaintyInMeters == NA | is.null(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters == " ") &
#       (coordinatePrecision <= 300 | is.na(coordinatePrecision) | coordinatePrecision == "NA" | coordinatePrecision == NA | is.null(coordinatePrecision) | coordinatePrecision == " ") &
#       dplyr::between(year, 2010, 2020)
#   ) %>%
#   dplyr::select(countryCode)

# records_per_countries <- csv_gbif_cc %>% count(countryCode, sort = TRUE)

# # write_csv(records_per_countries, "records-per-countries_300.csv")