gbif <- function(years_range = list(from = "2017-01-01", to = "2019-12-31"), season_months_range = list(from = 4, to = 7), path = "/../gbif/", csv_name = NULL, res_crs = 3035, presicion = 100, loaded_csv = NULL) {
  # kontrola (do)instalace všech dodatečně potřebných balíčků
  required_packages <- c("tidyverse", "rgbif", "sf", "lubridate", "magrittr", "dplyr")
  install.packages(setdiff(required_packages, rownames(installed.packages())))

  # načte všechny požadované knihovny jako dělá jednotlivě library()
  lapply(required_packages, require, character.only = TRUE)

  # # # # # # # # # # # # # # # # # # # # # #
  # nastavení základních parametrů [start]  #
  # # # # # # # # # # # # # # # # # # # # # #

  ## výběr regionu

  # Předávat parametricky???
  # definice obálek (bounding box) různě velkých území pro testování
  sz_cechy <- list(xmin = 13.0, xmax = 13.5, ymin = 50.0, ymax = 50.5)
  cesko <- list(xmin = 12.0, xmax = 19.0, ymin = 48.5, ymax = 51.5)
  str_evropa <- list(xmin = 8.5, xmax = 22.0, ymin = 46.0, ymax = 53.5)
  str_evropa2 <-
    list(
      xmin = 8.6,
      xmax = 21.9,
      ymin = 46.4,
      ymax = 53.1
    )
  # výběr konkrétního území
  bb <- str_evropa2

  ## časové rozsahy

  # Rozmezí datumů (sezóny a let) musí být stejné jako u filtru prediktorů! Ideálně přebírat společnou hodnotu jednoho parametru?

  # rozsah snímků od/do
  # years_range <- list(from = '2017-01-01', to = '2019-12-31')

  # rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
  # season_months_range <- list(from = 4, to = 7)

  # # # # # # # # # # # # # # # # # # # # # #
  # nastavení základních parametrů [konec]  #
  # # # # # # # # # # # # # # # # # # # # # #

  xmin <- bb$xmin
  xmax <- bb$xmax
  ymin <- bb$ymin
  ymax <- bb$ymax

  boundingBox <- cbind(c(xmin, xmin, xmax, xmax, xmin), c(ymin, ymax, ymax, ymin, ymin))
  boundingBox_wkt <- st_as_text(st_polygon(list(boundingBox)))


  # Předávat parametricky???
  scientificNamesList <- list("Charadrius dubius" = 7937336, "Cinclus cinclus" = 2495093, "Locustella luscinioides" = 2493551, "Loxia curvirostra" = 9629160)
  # jen klíče
  taxonKeys <- as.vector(unlist(scientificNamesList))



  # pouze do 500 záznamů
  # do cyklu províce druhů

  # scientificNames <- c("Charadrius dubius", "Cinclus cinclus", "Locustella luscinioides", "Loxia curvirostra")
  # gbif <- occ_data(
  # scientificName = scientificNames,
  # geometry = boundingBox_wkt,
  # hasCoordinate = TRUE,
  # hasGeospatialIssue = FALSE,
  # limit = 10
  # )
  # print(as_tibble(gbif$data), n = 10)
  # return(gbif)



  if (is.null(csv_name)) {

    #
    # Authentication (GBIF), nezbytná pro stažení > 500 záznamů
    #

    # For user, pwd, and email parameters, you can set them in one of three ways:
    # •  Set them in your .Rprofile file with the names gbif_user, gbif_pwd, and gbif_email
    # •  Set  them  in  your .Renviron/.bash_profile (or  similar)  file  with  the  names GBIF_USER, GBIF_PWD, and GBIF_EMAIL
    # •  Simply pass strings to each of the parameters in the function call

    # /home/USER/.Renviron

    # "prefiltr" už při downloadu, mohl bych ale chtít uložit vše pro daný druh a datum filtrovat až dodatečně (postfiltr), pak bych nemusel pokaždé stahovat z GBIFu. Dodělat???

    rd <- occ_download(
      # pred("taxonKey", 2493551),
      pred_in("taxonKey", taxonKeys),
      # pred("scientificName", "Locustella luscinioides"), # nefunguje
      # pred("species", "Locustella luscinioides"), # nefunguje
      # pred_gte("eventDate", years_range$from), # nefunguje, chybí přidat "T00:00:00"?
      # pred_lte("eventDate", years_range$to), # nefunguje, chybí přidat "T00:00:00"?
      pred_gte("year", year(years_range$from)),
      pred_lte("year", year(years_range$to)),
      pred_gte("month", season_months_range$from),
      pred_lte("month", season_months_range$to),
      # pred("geometry", boundingBox_wkt),
      pred_within(boundingBox_wkt),
      pred("hasCoordinate", TRUE),
      pred("hasGeospatialIssue", FALSE),
      format = "SIMPLE_CSV"
    )

    occ_download_wait(rd)

    # uložení exportu
    occ_download_get(rd, path = path)

    # rd[1] # obsahuje klíč (unikátní identifikátor) ke stažení, nebo k identifikaci .zip-u
    zip <- paste0(path, "/", rd[1], ".zip")
    csv <- paste0(path, "/", rd[1], ".csv")

    unzip(zip, exdir = path)
  } else {
    csv <- paste0(path, "/", csv_name)
  }

  set_cols <- cols(gbifID = "c", coordinateUncertaintyInMeters = "d", coordinatePrecision = "d", day = "i", month = "i", year = "i")


  if (is.null(loaded_csv)) {
    csv_gbif <- read_tsv(csv, col_types = set_cols)
  } else {
    csv_gbif <- loaded_csv
  }




  # coordinateUncertaintyInMeters, coordinatePrecision - problematické, většinou neuvedeno vůbec...
  csv_gbif_filter <- csv_gbif %>%
    filter(
      (coordinateUncertaintyInMeters <= presicion | is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters == "NA" | coordinateUncertaintyInMeters == NA | is.null(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters == " ") &
        (coordinatePrecision <= presicion | is.na(coordinatePrecision) | coordinatePrecision == "NA" | coordinatePrecision == NA | is.null(coordinatePrecision) | coordinatePrecision == " ") &
        between(month, season_months_range$from, season_months_range$to) &
        between(year, year(years_range$from), year(years_range$to))
    ) %>%
    dplyr::select(gbifID, species, decimalLatitude, decimalLongitude) %>%
    drop_na(decimalLatitude) %>%
    drop_na(decimalLongitude)


  if (is.null(res_crs)) {
    csv_gbif_filter %<>%
      rename(key = gbifID, latitude = decimalLatitude, longitude = decimalLongitude)
  } else {
    csv_gbif_filter_coords <- csv_gbif_filter %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
      st_transform(res_crs) %>%
      st_coordinates() %>%
      as_tibble()

    csv_gbif_filter %<>%
      mutate(csv_gbif_filter_coords) %>%
      dplyr::select(gbifID, species, X, Y) %>%
      rename(key = gbifID, latitude = Y, longitude = X) %>%
      drop_na(latitude) %>%
      drop_na(longitude)

    csv_gbif_filter$latitude %<>%
      as.integer
    csv_gbif_filter$longitude %<>%
      as.integer
  }

  return(csv_gbif_filter)
}
# res <- gbif(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv")
# print(as_tibble(res), n = 10)