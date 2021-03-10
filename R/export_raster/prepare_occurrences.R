# vybere konkrétní druh z nálezovek NDOP a GBIF (ndop_occurrences+gbif_occurrences; bere předpřipravená data z ndop.R a gbif.R) a použite spThin o zadané délce (thin_par)
prepare_occurrences <-
  function(select_species = "Locustella luscinioides",
           thin_par = 0.1,
           export_path = "",
           ndop_occurrences = NULL,
           gbif_occurrences = NULL,
           res_crs = 3035) {
    # kontrola (do)instalace všech dodatečně potřebných balíčků
    required_packages <-
      c("tidyverse", "spThin", "sf", "lubridate", "magrittr")
    install.packages(setdiff(required_packages, rownames(installed.packages())))
    
    # načte všechny požadované knihovny jako dělá jednotlivě library()
    lapply(required_packages, require, character.only = TRUE)
    
    reps <-
      1 # původně 10, TODO: zvýšit až budu dělat více opakování modelů a pak i počítat průměry z jednotlivých thinnovaných subsetů - nutné změnit logiku výpočtů!!!
    # + coordinateCleaner a spThin na NDOP/GBIF data...
    
    # kontrola jestli v ndop_occurrences gbif_occurrences něco je, pak (ne)spojovat
    
    species <- select_species
    species_col <- gsub(" ", "_", species)
    gc()
    
    # NDOP
    res_ndop_ll <- ndop_occurrences %>% filter(species == !!species)
    if (nrow(res_ndop_ll) > 0) {
      res_ndop_ll_spthin <-
        thin(
          loc.data = res_ndop_ll,
          lat.col = "latitude",
          long.col = "longitude",
          spec.col = "species",
          thin.par = thin_par,
          reps = reps,
          locs.thinned.list.return = TRUE,
          write.files = FALSE,
          write.log.file = FALSE
        )
      if (is.null(res_crs)) {
        res_ndop_ll_spthin <-
          as_tibble(res_ndop_ll_spthin[[1]]) %>% add_column(species = !!species_col, .before = 1)
      } else{
        res_ndop_ll_spthin <-
          as_tibble(res_ndop_ll_spthin[[1]]) %>% add_column(species = !!species_col, .before = 1)
        
        
        res_ndop_ll_spthin_coords <- res_ndop_ll_spthin %>%
          st_as_sf(coords = c("Longitude", "Latitude"),
                   crs = 4326) %>%
          st_transform(res_crs) %>%
          st_coordinates() %>%
          as_tibble()
        
        res_ndop_ll_spthin %<>%
          mutate(res_ndop_ll_spthin_coords) %>%
          dplyr::select(species, X, Y) %>%
          rename(latitude = Y, longitude = X)
        
        res_ndop_ll_spthin$latitude %<>% as.integer
        res_ndop_ll_spthin$longitude %<>% as.integer
        
      }
      
      write_csv(res_ndop_ll_spthin,
                paste0(
                  export_path,
                  (thin_par * 1000),
                  "_ndop_",
                  species_col,
                  ".csv"
                ))
    } else{
      write_csv(data.frame(),
                paste0(
                  export_path,
                  "xxx_",
                  (thin_par * 1000),
                  "_ndop_",
                  species_col,
                  ".csv"
                ))
      res_ndop_ll_spthin <- NULL
    }
    gc()
    
    # GBIF
    res_gbif_ll <- gbif_occurrences %>% filter(species == !!species)
    
    if (nrow(res_gbif_ll) > 0) {
      res_gbif_ll_spthin <-
        thin(
          loc.data = res_gbif_ll,
          lat.col = "latitude",
          long.col = "longitude",
          spec.col = "species",
          thin.par = thin_par,
          reps = reps,
          locs.thinned.list.return = TRUE,
          write.files = FALSE,
          write.log.file = FALSE
        )
      
      
      if (is.null(res_crs)) {
        res_gbif_ll_spthin <-
          as_tibble(res_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
      } else{
        res_gbif_ll_spthin <-
          as_tibble(res_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
        
        
        res_gbif_ll_spthin_coords <- res_gbif_ll_spthin %>%
          st_as_sf(coords = c("Longitude", "Latitude"),
                   crs = 4326) %>%
          st_transform(res_crs) %>%
          st_coordinates() %>%
          as_tibble()
        
        res_gbif_ll_spthin %<>%
          mutate(res_gbif_ll_spthin_coords) %>%
          dplyr::select(species, X, Y) %>%
          rename(latitude = Y, longitude = X)
        
        res_gbif_ll_spthin$latitude %<>% as.integer
        res_gbif_ll_spthin$longitude %<>% as.integer
        
      }
      
      write_csv(res_gbif_ll_spthin,
                paste0(
                  export_path,
                  (thin_par * 1000),
                  "_gbif_",
                  species_col,
                  ".csv"
                ))
    } else{
      write_csv(data.frame(),
                paste0(
                  export_path,
                  "xxx_",
                  (thin_par * 1000),
                  "_gbif_",
                  species_col,
                  ".csv"
                ))
      res_gbif_ll_spthin <- NULL
    }
    
    # čistě spojení už thinovaných datasetů - asi ne, thinning znovu až nad spojeným datasetem?
    # ndop_gbif <- res_gbif_ll_spthin %>% add_row(res_ndop_ll_spthin)
    gc()
    
    
    # GBIF+NDOP
    if (nrow(res_ndop_ll) > 0 & nrow(res_gbif_ll) > 0) {
      ndop_gbif <- res_ndop_ll %>% add_row(res_gbif_ll)
      
      
      ndop_gbif_ll <-
        ndop_gbif #ndop_gbif %>% filter(species == !!species)
      
      ndop_gbif_ll_spthin <-
        thin(
          loc.data = ndop_gbif_ll,
          lat.col = "latitude",
          long.col = "longitude",
          spec.col = "species",
          thin.par = thin_par,
          reps = reps,
          locs.thinned.list.return = TRUE,
          write.files = FALSE,
          write.log.file = FALSE
        )
      
      if (is.null(res_crs)) {
        ndop_gbif_ll_spthin <-
          as_tibble(ndop_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
      } else{
        ndop_gbif_ll_spthin <-
          as_tibble(ndop_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
        
        ndop_gbif_ll_spthin_coords <- ndop_gbif_ll_spthin %>%
          st_as_sf(coords = c("Longitude", "Latitude"),
                   crs = 4326) %>%
          st_transform(res_crs) %>%
          st_coordinates() %>%
          as_tibble()
        
        ndop_gbif_ll_spthin %<>%
          mutate(ndop_gbif_ll_spthin_coords) %>%
          dplyr::select(species, X, Y) %>%
          rename(latitude = Y, longitude = X)
        
        ndop_gbif_ll_spthin$latitude %<>% as.integer
        ndop_gbif_ll_spthin$longitude %<>% as.integer
        
      }
      
      write_csv(ndop_gbif_ll_spthin,
                paste0(export_path,
                       (thin_par * 1000),
                       "_all_",
                       species_col,
                       ".csv"))
    } else{
      write_csv(data.frame(),
                paste0(
                  export_path,
                  "xxx_",
                  (thin_par * 1000),
                  "_all_",
                  species_col,
                  ".csv"
                ))
      ndop_gbif_ll_spthin <- NULL
    }
    
    
    return(list(
      res_ndop_ll_spthin,
      res_gbif_ll_spthin,
      ndop_gbif_ll_spthin
    ))
  }
# res <- prepare_occurrences(select_species = "Locustella luscinioides", export_path = wd,  ndop_occurrences = NULL, gbif_occurrences = NULL)
# print(as_tibble(res), n = 10)
