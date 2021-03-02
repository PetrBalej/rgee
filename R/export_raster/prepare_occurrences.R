prepare_occurrences <-
  function(select_species = "Locustella luscinioides",
           thin_par = 100,
           export_path = "",
           ndop_occurrences = NULL,
           gbif_occurrences = NULL) {
    # kontrola (do)instalace všech dodatečně potřebných balíčků
    required_packages <-
      c("tidyverse", "spThin", "sf", "lubridate", "magrittr")
    install.packages(setdiff(required_packages, rownames(installed.packages())))
    
    # načte všechny požadované knihovny jako dělá jednotlivě library()
    lapply(required_packages, require, character.only = TRUE)
    
    
    # + coordinateCleaner a spThin na NDOP/GBIF data...
    
    # kontrola jestli v ndop_occurrences gbif_occurrences něco je, pak (ne)spojovat
    
    species <- select_species
    species_col <- gsub(" ", "_", species)
    
    # NDOP
    res_ndop_ll <- ndop_occurrences %>% filter(species == !!species)
    
    res_ndop_ll_spthin <-
      thin(
        loc.data = res_ndop_ll,
        lat.col = "latitude",
        long.col = "longitude",
        spec.col = "species",
        thin.par = thin_par,
        reps = 10,
        locs.thinned.list.return = TRUE,
        write.files = FALSE,
        write.log.file = FALSE
      )
    
    res_ndop_ll_spthin <-
      as_tibble(res_ndop_ll_spthin[[1]]) %>% add_column(species = !!species_col, .before = 1)
    write_csv(
      res_ndop_ll_spthin,
      paste0(
        export_path,
        thin_par,
        "_res_ndop_ll_spthin_",
        species_col,
        ".csv"
      )
    )
    
    # GBIF
    res_gbif_ll <- gbif_occurrences %>% filter(species == !!species)
    
    res_gbif_ll_spthin <-
      thin(
        loc.data = res_gbif_ll,
        lat.col = "latitude",
        long.col = "longitude",
        spec.col = "species",
        thin.par = thin_par,
        reps = 10,
        locs.thinned.list.return = TRUE,
        write.files = FALSE,
        write.log.file = FALSE
      )
    
    res_gbif_ll_spthin <-
      as_tibble(res_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
    write_csv(
      res_gbif_ll_spthin,
      paste0(
        export_path,
        thin_par,
        "_res_gbif_ll_spthin_",
        species_col,
        ".csv"
      )
    )
    
    # čistě spojení už thinovaných datasetů - asi ne, thinning znovu až nad spojeným datasetem?
    # ndop_gbif <- res_gbif_ll_spthin %>% add_row(res_ndop_ll_spthin)
    
    # GBIF+NDOP
    ndop_gbif <- ndop_occurrences %>% add_row(gbif_occurrences)
    
    ndop_gbif_ll <- ndop_gbif %>% filter(species == !!species)
    
    ndop_gbif_ll_spthin <-
      thin(
        loc.data = ndop_gbif_ll,
        lat.col = "latitude",
        long.col = "longitude",
        spec.col = "species",
        thin.par = thin_par,
        reps = 10,
        locs.thinned.list.return = TRUE,
        write.files = FALSE,
        write.log.file = FALSE
      )
    
    ndop_gbif_ll_spthin <-
      as_tibble(ndop_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
    write_csv(
      ndop_gbif_ll_spthin,
      paste0(
        export_path,
        thin_par,
        "_res_ndop_gbif_ll_spthin_",
        species_col,
        ".csv"
      )
    )
    
    return(list(
      res_ndop_ll_spthin,
      res_gbif_ll_spthin,
      ndop_gbif_ll_spthin
    ))
  }
# res <- prepare_occurrences(select_species = "Locustella luscinioides", export_path = wd,  ndop_occurrences = NULL, gbif_occurrences = NULL)
# print(as_tibble(res), n = 10)
