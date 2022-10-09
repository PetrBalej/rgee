# průběžné mazání temp (hlavně zbytečných tiffů jako meziproduktů k .asc)
clear_temp <- function() {
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
}

# gc() # Force R to release memory it is no longer using


# převod gee_datasets do přístupnějšího listu
gee_datasets_list <- function(gee_datasets_path_csv) {
  # parametry použitých datasetů z GEE
  gee_datasets <- read.csv(gee_datasets_path_csv)
  # cell <- subset(gee_datasets, short == "landsat", select = "geeSnippet")

  gdl <- list()
  for (i in 1:nrow(gee_datasets)) {
    sublist <-
      list(
        short = toString(gee_datasets[i, "short"]),
        geeSnippet = toString(gee_datasets[i, "geeSnippet"]),
        type = toString(gee_datasets[i, "type"])
      )
    gdl[toString(gee_datasets[i, "short"])] <- list(sublist)
  }
  return(gdl)
}


# odstranění stínů a oblačnosti Landsat 8, LANDSAT_LC08_C01_T1_SR
mask_L8_sr <- function(image) {
  # Get the pixel QA band.
  qa <- image$select("pixel_qa")

  # https://www.usgs.gov/media/files/landsat-8-collection-1-land-surface-reflectance-code-product-guide
  # Landsat 8 Collection 1 (C1) Land Surface Reflectance Code (LaSRC) Product Guide
  # LSDS-1368 Version 3.0
  # Table 6-3. Landsat 8 Pixel Quality Assessment (pixel_qa) Values
  # 322: Clear terrain, low-confidence cloud, low-confidence cirrus
  # 324: Water, low-confidence cloud, low-confidence cirrus
  mask <- qa$eq(322)$bitwiseOr(qa$eq(324))


  return(image$updateMask(mask))
}


# odstranění stínů a oblačnosti Sentinel 2, COPERNICUS_S2_SR
maskS2clouds <- function(image) {
  # nefunkční?!
  # Get the pixel QA band.
  qa <- image$select("QA60")

  # Bits 10 and 11 are clouds and cirrus, respectively.
  mask <- qa$bitwiseAnd(10)$eq(0)$and(qa$bitwiseAnd(11)$eq(0))
  return(image$updateMask(mask)$divide(10000))
}

# export rasterů z GEE image
export_gee_image <-
  function(image,
           region,
           scale,
           dsn = "default_file_name",
           format = NULL,
           bands = c(),
           set_extent = NULL,
           set_res = NULL,
           res_proj_epsg = 3035,
           use_google_drive = TRUE,
           retype = FALSE) {
    export_raster <- format != ""

    if (export_raster == TRUE) {
      bands_count <- length(bands)

      multiband_support <-
        list(
          grd = "raster",
          nc = "CDF",
          tif = "GTiff",
          envi = "ENVI",
          bil = "EHdr",
          img = "HFA"
        )
      multiband_not_support <-
        list(
          asc = "ascii",
          sdat = "SAGA",
          rst = "IDRISI"
        )

      multiband_support_ext <- names(multiband_support)
      multiband_not_support_ext <- names(multiband_not_support)

      all <- c(multiband_support, multiband_not_support)
      all_ext <- c(multiband_support_ext, multiband_not_support_ext)


      if (is.element(format, all)) {
        w_t <- which(format == all)
        ext <- names(all[w_t[[1]]])
        ext_wr <- format
      } else if (is.element(format, all_ext)) {
        ext <- format
        ext_wr <- all[[format]]
      } else {
        stop(paste0("Output file format ", format, " not supported!"))
      }

      if (is.element(ext, multiband_not_support_ext) &&
        bands_count > 1) {
        stop(paste0("Output file format ", format, " have not multiband support!"))
      }
    }

    if (res_proj_epsg == 5514) {
      # https://epsg.io/5514 - export definition
      # ESRI WKT
      proj <- 'PROJCS["S_JTSK_Krovak_East_North",GEOGCS["GCS_S-JTSK",DATUM["D_S_JTSK",SPHEROID["Bessel_1841",6377397.155,299.1528128]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Krovak"],PARAMETER["latitude_of_center",49.5],PARAMETER["longitude_of_center",24.83333333333333],PARAMETER["azimuth",30.28813972222222],PARAMETER["pseudo_standard_parallel_1",78.5],PARAMETER["scale_factor",0.9999],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["Meter",1]]'

      # OGC WKT - nerozpozná ArcMap, ale zobrazuje správně
      # proj <- 'PROJCS["S-JTSK / Krovak East North",GEOGCS["S-JTSK",DATUM["System_Jednotne_Trigonometricke_Site_Katastralni",SPHEROID["Bessel 1841",6377397.155,299.1528128,AUTHORITY["EPSG","7004"]],TOWGS84[589,76,480,0,0,0,0],AUTHORITY["EPSG","6156"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4156"]],PROJECTION["Krovak"],PARAMETER["latitude_of_center",49.5],PARAMETER["longitude_of_center",24.83333333333333],PARAMETER["azimuth",30.28813972222222],PARAMETER["pseudo_standard_parallel_1",78.5],PARAMETER["scale_factor",0.9999],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["X",EAST],AXIS["Y",NORTH],AUTHORITY["EPSG","5514"]]'
    } else {
      proj <- ee$Projection(paste0("EPSG:", res_proj_epsg))
    }

    # if (!is.null(clip)) {

    #   # im_m <- image$reproject(proj, NULL, scale)$clip(clip)$neq(0)
    #   # im <- image$reproject(proj, NULL, scale)$updateMask(im_m)$neq(0)$unmask(-9999)

    #   #im_m <- image$reproject(proj, NULL, scale)$clip(clip)$eq(0.0L)
    #   #im <- image$reproject(proj, NULL, scale)$clip(clip)$updateMask(im_m)
    #   # im <- im$where(im$eq(as.numeric(0.0L)), -9999)

    #   im <- image$reproject(proj, NULL, scale)$clip(clip)
    # } else {
    #   im <- image$reproject(proj, NULL, scale)
    # }

    via <- "drive" # "drive" "getInfo"
    if (use_google_drive == FALSE) {
      via <- "getInfo"
    }

    image <- image$reproject(proj, NULL, scale)

    # přetypovat za účelem optimalizace velikosti?
    if (retype == TRUE) {
      # pokud jde o NDVI, *NDWI, *SAVI nebo EVI nutná úprava před přetypováním na INT16
      if (grepl("nd.i", tolower(dsn), fixed = FALSE) == TRUE |
        grepl("evi", tolower(dsn), fixed = TRUE) == TRUE |
        grepl("savi", tolower(dsn), fixed = TRUE) == TRUE) {
        image <- image$multiply(10000)$toInt16()
      } else {
        image <- image$toInt16()
      }
    }

    result_raster <- ee_as_raster(
      image = image,
      region = region,
      scale = scale,
      via = via,
      dsn = dsn,
      timePrefix = FALSE
      # maxPixels = 1e10
    )

    # úprava extentu
    if (!is.null(set_extent)) {
      # doplnit i parametry keepres=TRUE,  snap=FALSE ?
      rr_e <- setExtent(result_raster[[bands[1]]], set_extent)
    } else {
      rr_e <- result_raster[[bands[1]]]
    }

    # úprava resolution (velikosti pixelu)
    if (!is.null(set_res)) {
      res(rr_e) <- set_res
    }

    # uložení rasteru
    if (export_raster == TRUE) {
      writeRaster(rr_e, dsn, ext_wr, overwrite = TRUE)
    }

    return(rr_e)
  }

# vytvoří všechny kombinace bez opakování z vloženého vektoru (včetně možných jednotlivých tříd)
comb_all <- function(vector) {
  comb_list <- list()
  total <- length(vector)
  for (i in 1:total) {
    comb_list <- append(comb_list, combn(vector, i, simplify = FALSE))
  }
  return(comb_list)
}

# načte do RasterStack-u všechny rastry ze zadaného adresáře a dané přípony
rasters_dir_stack <- function(path_dir, raster_extension) {
  rasters_list <-
    list.files(
      path = path_dir,
      pattern = paste0("\\.", raster_extension, "$"),
      ignore.case = TRUE,
      full.names = TRUE
    )
  raster_stack <- stack(lapply(rasters_list, raster::raster))
  return(raster_stack)
}


# https://gist.github.com/johnbaums/3ff4c9aa01032ce21a84672c477a6dfb
maxtss2 <- function(x, t = "Test") {

  # t = "Test" / "Training"
  # x: a directory containing the cross-validated Maxent output
  ## Notes:
  ## tss  = sens + spec - 1
  ## sens = 1 - omission
  ## spec = 1 - FPA
  ## tss  = 1 - omission + 1 - fpa - 1
  ##      = 1 - (omission + fpa)

  # rekurzivně prohledat i podadresáře???  - když si budu dělat vlastní cykly s testy recursive = TRUE

  scol <- paste0(t, ".omission")
  ff <- list.files(x, "omission\\.csv$", full.names = TRUE)
  max_tss <- sapply(ff, function(f) {
    d <- read.csv(f)
    # d$Training.omission
    i <- which.min(d$scol + d$Fractional.area)
    type <- gsub("Corresponding\\.|\\.value", "", colnames(d)[3])
    if (!tolower(type) %in% c("logistic", "cloglog")) {
      stop('Expected name of third column to contain either "logistic" or "Cloglog".')
    }

    # d$Training.omission
    c(max_tss = 1 - min(d$tscol + d$Fractional.area), thr = d[, 3][i])
  })
  out <- t(max_tss)
  rownames(out) <- basename(rownames(out))
  list(
    max_tss = out, max_tss_mean = mean(out[, "max_tss"]),
    max_tss_sd = sd(out[, "max_tss"])
  )
}


performance <- function(confusion) {
  # https://onlinelibrary-wiley-com.infozdroje.czu.cz/doi/10.1111/jbi.13402
  tp <- confusion[1]
  fp <- confusion[2]
  fn <- confusion[3]
  tn <- confusion[4]
  TPR <- tp / (tp + fn)
  TNR <- tn / (tn + fp)
  FPR <- fp / (fp + tn)
  FNR <- fn / (fn + tp)
  Sensitivity <- TPR
  Specificity <- TNR
  TSS <- Sensitivity + Specificity - 1
  Jaccard <- tp / (fn + tp + fp)
  Sorensen <- 2 * tp / (fn + 2 * tp + fp)
  F_measure <- 2 * Jaccard
  OPR <- fp / (tp + fp)
  UPR <- 1 - Sensitivity

  return(data.frame(
    TPR = TPR, TNR = TNR, FPR = FPR, FNR = FNR, Sensitivity = Sensitivity, Specificity = Specificity,
    TSS = TSS, Jaccard = Jaccard, Sorensen = Sorensen, F_measure = F_measure, OPR = OPR, UPR = UPR
  ))
}

performance_models_list <- function(m) {
  cm <- lapply(m, function(x) performance(x$conf))
  cm.matrix <- abind(cm, along = 3)
  cm.perf <- apply(cm.matrix, c(1, 2), median)
  cm.perf.t <- as_tibble(cm.perf)

  return(cm.perf.t)
}

get_freq_by_cat <- function(freq.df, cat.id) {
  if (exists("freq.df") && !is.na(freq.df)) {
    # získání hodnot dle kategorie početnosti z freq()
    if (which(freq.df[, 1] == cat.id) == 0) {
      return(0)
    } else {
      return(unname(freq.df[which(freq.df[, 1] == cat.id), 2]))
    }
  } else {
    return(NA)
  }
}

rasters_confusion <- function(reality, prediction) {
  if (class(reality) == "RasterLayer" && class(prediction) == "RasterLayer") {
    # předpoklad je vstup binárních rasterů!!!
    overlap <- reality + (prediction * 2)
    classes <- freq(overlap)
    confusion <- c()
    confusion[1] <- get_freq_by_cat(classes, 3) # tp
    confusion[2] <- get_freq_by_cat(classes, 2) # fp
    confusion[3] <- get_freq_by_cat(classes, 1) # fn
    confusion[4] <- get_freq_by_cat(classes, 0) # tn

    return(performance(confusion))
  } else {
    return(NA)
  }
}



stack_NA_repair <- function(raster_stack) {
  # nutné odstranit pseudo NA hodnoty aby se nepočítyly do rozsahů prediktorů ve výpočtech - zásadní!!!
  # + propíše NA hodnoty napříč layery a znovu určí minMax hodnoty

  raster_stack_Bx <- subset(raster_stack, grep("_B", names(raster_stack)))
  # XXX Jelikož jsou zřejmě z export_raster.R špatně vygenerované _EVI, _SAVI a _MSAVI (mají nemožné přeškálované (*10000) rozsahy ±32768, normální je pro indexy ±1 rozsah ±9999),
  # tak je nutné je nevybírat do výsledných stacků, aby nekazily analýzy. NDVI, (M)NDWI by měly být v pořádku.
  raster_stack_I <- subset(raster_stack, grep("ND", names(raster_stack))) # po opravě možno grep("I",...)
  raster_stack_biox <- subset(raster_stack, grep("_bio", names(raster_stack)))
  raster_stack_srtm <- subset(raster_stack, grep("srtm", names(raster_stack)))

  raster_stack_Bx[raster_stack_Bx == -9999] <- NA
  raster_stack_I[raster_stack_I == -32768] <- NA
  raster_stack_srtm[raster_stack_srtm == -9999] <- NA
  raster_stack_srtm[raster_stack_srtm == -32768] <- NA
  raster_stack <- stack(raster_stack_Bx, raster_stack_I, raster_stack_biox, raster_stack_srtm)

  # + propíše NA hodnoty napříč layery
  raster_stack <- raster::mask(raster_stack, sum(raster_stack))

  #  znovu určí minMax hodnoty
  raster_stack <- raster::setMinMax(raster_stack)

  return(raster_stack)
}


round_df <- function(x, digits) {
  # pro optimalizaci tvorby bufferů, použít?
  numcols <- sapply(x, mode) == "numeric"
  x[numcols] <- round(x[numcols], digits)
  return(unique(x))
}

floor_df <- function(x, digits) {
  # pro optimalizaci tvorby bufferů, použít?
  numcols <- sapply(x, mode) == "numeric"
  x[numcols] <- x[numcols] / (10^abs(digits))
  x[numcols] <- floor(x[numcols])
  x[numcols] <- x[numcols] * (10^abs(digits))
  return(unique(x))
}

per_pixel_df <- function(x, divide_by) {
  # jeden nález per pixel (divide_by), zadané v dataframe
  numcols <- sapply(x, mode) == "numeric"
  x[numcols] <- x[numcols] %/% divide_by
  x[numcols] <- x[numcols] * divide_by
  x[numcols] <- x[numcols] + (divide_by / 2)
  return(unique(x))
}

normalize <- function(x) {
  min <- raster::minValue(x)
  max <- raster::maxValue(x)
  return((x - min) / (max - min))
}



nepuvodni_problematicke <- function() {
  nepuvodni <- c(
    # C
    "Phasianus colchicus",
    "Syrmaticus reevesi",
    "Branta canadensis",
    "Columba livia",
    "Alopochen aegyptiacus",
    "Alopochen aegyptiaca",
    "Threskiornis aethiopicus",
    "Aix galericulata",
    "Oxyura jamaicensis",
    # D
    "Bucephala albeola",
    "Bucephala islandica",
    "Lophodytes cucullatus",
    "Histrionicus histrionicus",
    "Gypaetus barbatus",
    # E
    "Phylloscopus sibilatrix",
    "Aix sponsa",
    "Platalea leucorodia"
  )

  problematicke <- c(
    "Turdus merula", # lesní vs. městské populace
    "Luscinia svecica", # dva poddruhy s odlišnými nároky, nejsem schopný je jednoduše odlišit...
    "Luscinia luscinia" # problematické nálezy zejména z Červenohorského sedla (>1000mnm, ikdyž jsou vícekrát a dlouhodobě nezávisle potvrzené, možná jde jen o oblíbenou zastávku při průtahu (kam?) nebo záměny s L. mega.? Raději vyloučit.
  )

  # https://www.ochranaprirody.cz/res/archive/372/058764.pdf - Nevhodný pro hodnocení – Not Applicable (NA)
  nevhodne <- c(
    "Anas penelope",
    "Podiceps grisegena",
    "Aquila chrysaetos",
    "Larus cachinnans",
    "Larus michahellis",
    "Sternula albifrons",
    "Chlidonias hybrida",
    "Otus scops",
    "Asio flammeus",
    "Luscinia luscinia",
    "Turdus iliacus",
    "Motacilla citreola"
  )

  return(list(nepuvodni = nepuvodni, problematicke = problematicke, nevhodne = nevhodne))
}

synonyms <- function() {
  synonyms <- list(
    "Spatula clypeata" = "Anas clypeata",
    "Phylloscopus sibilatrix" = "Phylloscopus sibillatrix",
    "Spatula querquedula" = "Anas querquedula",
    "Mareca penelope" = "Anas penelope",
    "Calidris pugnax" = "Philomachus pugnax",
    "Dryobates minor" = "Dendrocopos minor",
    # nové oproti traits
    "Acanthis cabaret" = "Acanthis flammea",
    "Mareca strepera" = "Anas strepera",
    "Clanga pomarina" = "Aquila pomarina",
    "Tetrastes bonasia" = "Bonasa bonasia",
    "Linaria cannabina" = "Carduelis cannabina",
    "Acanthis flammea" = "Carduelis flammea",
    "Dendrocoptes medius" = "Dendrocopos medius",
    "Dryobates minor" = "Dendrocopos minor",
    "Ardea alba" = "Egretta alba",
    "Ichthyaetus melanocephalus" = "Larus melanocephalus",
    "Poecile montanus" = "Parus montanus",
    "Saxicola rubicola" = "Saxicola torquata",
    "Lyrurus tetrix" = "Tetrao tetrix",
    "Chlidonias hybrida" = "Chlidonias hybridus",
    # nové k fkcso
    "Lophophanes cristatus" = "Parus cristatus",
    "Chloris chloris" = "Carduelis chloris",
    "Chroicocephalus ridibundus" = "Larus ridibundus",
    "Spinus spinus" = "Carduelis spinus",
    "Linaria cannabina" = "Carduelis cannabina",
    "Regulus ignicapilla" = "Regulus ignicapillus",
    "Delichon urbicum" = "Delichon urbica",
    "Periparus ater" = "Parus ater",
    "Poecile palustris" = "Parus palustris"
  )
  return(synonyms)
}


synonyms_unite <- function(tbl) {
  # sjednocení synonym, druh pojmenovaný species
  syns <- synonyms()

  # nahrazení názvů traits druhů novějšími názvy z NDOPu
  for (s in names(syns)) {
    matched <- tbl %>% filter(species == syns[[s]])
    if (nrow(matched) == 1) {
      tbl[tbl$species == syns[[s]], "species"] <- s
    }
  }
  return(tbl)
}

join_outputs_rds <- function(export_path, prefix) {
  rds_list <-
    list.files(
      path = paste0(export_path, "/outputs/rds"), # vse-v-jednom/outputs/rds/
      pattern = paste0("^enmsr_", prefix, ".+\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
      ignore.case = TRUE,
      full.names = TRUE
    )


  rds_append <- readRDS(rds_list[[1]])
  rds_list <- rds_list[-1]
  for (i in seq_along(rds_list)) {
    rds_append <- append(rds_append, readRDS(rds_list[[i]]))
  }


  # https://cs.wikipedia.org/wiki/Seznam_pt%C3%A1k%C5%AF_%C4%8Ceska
  nepuvodni <- nepuvodni_problematicke()$nepuvodni
  problematicke <- nepuvodni_problematicke()$problematicke


  "%notin%" <- Negate("%in%")

  for (i in seq_along(names(rds_append))) {
    tibble <- rds_append[[i]] %>% # ttemp
      map_depth(1, na.omit) %>%
      map(as_tibble) %>%
      bind_rows(.id = "species") %>%
      # group_by(species) %>% dplyr::select(-r)
      distinct(species, .keep_all = TRUE) %>%
      filter(species %notin% nepuvodni)
    # %>% filter(species %notin% problematicke)

    tibble %<>% tibble %>% group_by(species, px_size_item) # sp = paste(species, px_size_item)

    tibble_gbif <- tibble %>%
      summarize(vip1.gbif, .groups = "keep") %>%
      rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
      rename_all(paste0, ".gbif")

    tibble_ndop <- tibble %>%
      summarize(vip1.ndop, .groups = "keep") %>%
      rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
      rename_all(paste0, ".ndop")


    tibble_gbif_p <- tibble %>%
      summarize(perf.gbif, .groups = "keep") %>%
      rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
      rename_all(paste0, ".gbif")
    tibble_ndop_p <- tibble %>%
      summarize(perf.ndop, .groups = "keep") %>%
      rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
      rename_all(paste0, ".ndop")


    tibble.gbif_ndop.pa <- tibble %>%
      summarize(gbif_ndop.pa, .groups = "keep") %>%
      rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
      rename_all(paste0, ".gbif_ndop")
    tibble.ndop_gbif.pa <- tibble %>%
      summarize(ndop_gbif.pa, .groups = "keep") %>%
      rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
      rename_all(paste0, ".ndop_gbif")


    tibble_result <- tibble %>%
      left_join(tibble_gbif, by = c("species" = "species.gbif")) %>%
      left_join(tibble_ndop, by = c("species" = "species.ndop")) %>%
      left_join(tibble_gbif_p, by = c("species" = "species.gbif")) %>%
      left_join(tibble_ndop_p, by = c("species" = "species.ndop")) %>%
      left_join(tibble.gbif_ndop.pa, by = c("species" = "species.gbif_ndop")) %>%
      left_join(tibble.ndop_gbif.pa, by = c("species" = "species.ndop_gbif")) %>%
      ungroup() %>%
      dplyr::select(-contains("vip")) %>%
      dplyr::select(-contains("perf.")) %>%
      dplyr::select(-contains(".pa"))

    # spojím to až na konci!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!§§
    if (i == 1) {
      # založím novou tibble na základě vyprázdněné první
      tibble_grains <- tibble_result[NULL, ]
    }

    tibble_grains %<>% add_row(tibble_result)
  }

  return(tibble_grains)
}



fit_models <- function(alg, replicates, eval, test.prop, enm_mxt_gbif.s, enm_mxt_ndop.s, enm_mxt_all.s, raster_stack_b, raster_stack_mask_czechia_b, bias_gbif, bias_ndop, bias_all, nback_all, nback_ndop, breadth_only = FALSE) {



  ###
  ### gbifM
  ###
  print("GBIFM")
  enm_mxt_gbif.breadth.B1 <- NA
  enm_mxt_gbif.breadth.B2 <- NA
  enm_mxt_gbif <- list()
  if (!is.na(enm_mxt_gbif.s) && length(enm_mxt_gbif.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.glm(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_gbif,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.gam(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_gbif,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.maxent(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_gbif,
          args = c("removeDuplicates=FALSE", "outputFormat=raw"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }
    ### GBIF .breadth
    enm_mxt_gbif.breadth <- lapply(enm_mxt_gbif, raster.breadth)
    enm_mxt_gbif.breadth.B1 <- mean(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
    enm_mxt_gbif.breadth.B2 <- mean(sapply(enm_mxt_gbif.breadth, function(x) x$B2))
    # enm_mxt_gbif.breadth.B1.md <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
    # enm_mxt_gbif.breadth.B2.md <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B2))

    # enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
    # enm_mxt_gbif.r.m <- cellStats(enm_mxt_gbif.r, stat = "mean")
    # enm_mxt_gbif.r.md <- cellStats(enm_mxt_gbif.r, stat = "median")
    # enm_mxt_gbif.r.sd <- cellStats(enm_mxt_gbif.r, stat = "sd")
  }

  ###
  ### ndopM
  ###

  print("NDOPM")


  enm_mxt_ndop.breadth.B1 <- NA
  enm_mxt_ndop.breadth.B2 <- NA
  enm_mxt_ndop <- list()
  if (!is.na(enm_mxt_ndop.s) && length(enm_mxt_ndop.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_ndop[[r]] <- enmtools.glm(
          enm_mxt_ndop.s,
          eval = eval,
          raster_stack_mask_czechia_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_ndop,
          nback = nback_ndop,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_ndop[[r]] <- enmtools.gam(
          enm_mxt_ndop.s,
          eval = eval,
          raster_stack_mask_czechia_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_ndop,
          nback = nback_ndop,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_ndop[[r]] <- enmtools.maxent(
          enm_mxt_ndop.s,
          eval = eval,
          raster_stack_mask_czechia_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_ndop,
          args = c("removeDuplicates=FALSE", "outputFormat=raw"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          nback = nback_ndop,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }
    ### NDOP .breadth
    enm_mxt_ndop.breadth <- lapply(enm_mxt_ndop, raster.breadth)
    enm_mxt_ndop.breadth.B1 <- mean(sapply(enm_mxt_ndop.breadth, function(x) x$B1))
    enm_mxt_ndop.breadth.B2 <- mean(sapply(enm_mxt_ndop.breadth, function(x) x$B2))
    # enm_mxt_ndop.breadth.B1.md <- median(sapply(enm_mxt_ndop.breadth, function(x) x$B1))
    # enm_mxt_ndop.breadth.B2.md <- median(sapply(enm_mxt_ndop.breadth, function(x) x$B2))
  }

  ###
  ### allM (ndpop+gbif)
  ###
  print("ALLM")

  enm_mxt_all.breadth.B1 <- NA
  enm_mxt_all.breadth.B2 <- NA
  enm_mxt_all <- list()
  if (!is.na(enm_mxt_all.s) && length(enm_mxt_all.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.glm(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.gam(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.maxent(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          args = c("removeDuplicates=FALSE", "outputFormat=raw"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }

    ### ALL .breadth
    enm_mxt_all.breadth <- lapply(enm_mxt_all, raster.breadth)
    enm_mxt_all.breadth.B1 <- mean(sapply(enm_mxt_all.breadth, function(x) x$B1))
    enm_mxt_all.breadth.B2 <- mean(sapply(enm_mxt_all.breadth, function(x) x$B2))
    # enm_mxt_all.breadth.B1.md <- median(sapply(enm_mxt_all.breadth, function(x) x$B1))
    # enm_mxt_all.breadth.B2.md <- median(sapply(enm_mxt_all.breadth, function(x) x$B2))
  }

  if (breadth_only) {
    return(list(
      enm_mxt_gbif.breadth.B1 = enm_mxt_gbif.breadth.B1, enm_mxt_gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
      enm_mxt_ndop.breadth.B1 = enm_mxt_ndop.breadth.B1, enm_mxt_ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
      enm_mxt_all.breadth.B1 = enm_mxt_all.breadth.B1, enm_mxt_all.breadth.B2 = enm_mxt_all.breadth.B2

      # enm_mxt_gbif.breadth.B1.md = enm_mxt_gbif.breadth.B1.md, enm_mxt_gbif.breadth.B2.md = enm_mxt_gbif.breadth.B2.md,
      # enm_mxt_ndop.breadth.B1.md = enm_mxt_ndop.breadth.B1.md, enm_mxt_ndop.breadth.B2.md = enm_mxt_ndop.breadth.B2.md,
      # enm_mxt_all.breadth.B1.md = enm_mxt_all.breadth.B1.md, enm_mxt_all.breadth.B2.md = enm_mxt_all.breadth.B2.md
    ))
  } else {
    return(list(
      enm_mxt_gbif = enm_mxt_gbif, enm_mxt_ndop = enm_mxt_ndop, enm_mxt_all = enm_mxt_all,

      enm_mxt_gbif.breadth.B1 = enm_mxt_gbif.breadth.B1, enm_mxt_gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
      enm_mxt_ndop.breadth.B1 = enm_mxt_ndop.breadth.B1, enm_mxt_ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
      enm_mxt_all.breadth.B1 = enm_mxt_all.breadth.B1, enm_mxt_all.breadth.B2 = enm_mxt_all.breadth.B2

      # enm_mxt_gbif.r.m = enm_mxt_gbif.r.m,
      # enm_mxt_gbif.r.md = enm_mxt_gbif.r.md,
      # enm_mxt_gbif.r.sd = enm_mxt_gbif.r.sd

      # enm_mxt_gbif.breadth.B1.md = enm_mxt_gbif.breadth.B1.md, enm_mxt_gbif.breadth.B2.md = enm_mxt_gbif.breadth.B2.md,
      # enm_mxt_ndop.breadth.B1.md = enm_mxt_ndop.breadth.B1.md, enm_mxt_ndop.breadth.B2.md = enm_mxt_ndop.breadth.B2.md,
      # enm_mxt_all.breadth.B1.md = enm_mxt_all.breadth.B1.md, enm_mxt_all.breadth.B2.md = enm_mxt_all.breadth.B2.md
    ))
  }
}


fit_modelsN <- function(alg, replicates, eval, test.prop, enm_mxt_gbif.s, enm_mxt_ndop.s, enm_mxt_all.s, raster_stack_b, raster_stack_mask_czechia_b, bias_gbif, bias_ndop, bias_all, nback_all, nback_ndop, breadth_only = TRUE, fit_gbif_crop = TRUE, czechia_3035 = NA, bg.source.ndop = "range", bg.source.gbif = "range") {



  ###
  ### gbifM
  ###
  print("GBIFM")
  enm_mxt_gbif.breadth.B1 <- NA
  enm_mxt_gbif.breadth.B2 <- NA
  enm_mxt_gbif <- list()
  if (!is.na(enm_mxt_gbif.s) && length(enm_mxt_gbif.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.glm(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = bg.source.gbif, # "range" !!!
          verbose = TRUE,
          bias = bias_gbif,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.gam(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = bg.source, # "range" !!!
          verbose = TRUE,
          bias = bg.source.gbif, # bias_gbif !!! - můžu ale vybírat background i ze všech presenčních bodů přímo, nejen přes vygenerovaný raster z density.ppp!!!
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.maxent(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = bg.source.gbif, # "range" !!!
          verbose = TRUE,
          bias = bias_gbif,
          args = c("removeDuplicates=FALSE", "threads=4"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          removeDuplicates = FALSE,
          threads = 4,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }

    ### GBIF .breadth
    enm_mxt_gbif.breadth <- lapply(enm_mxt_gbif, raster.breadth)
    if (fit_gbif_crop == FALSE) {
      enm_mxt_gbif.breadth.B1 <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
      enm_mxt_gbif.breadth.B2 <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B2))
    } else {
      print("GBIFM crop by Czechia")
      sr <- sapply(enm_mxt_gbif, function(x) x$suitability)
      src <- sapply(sr, crop, y = extent(czechia_3035))
      srm <- sapply(src, mask, mask = czechia_3035)
      enm_mxt_gbif.breadth.B1 <- median(unlist(sapply(srm, raster.breadth)[1, ]))
      enm_mxt_gbif.breadth.B2 <- median(unlist(sapply(srm, raster.breadth)[2, ]))
    }

    # enm_mxt_gbif.breadth.B1.md <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
    # enm_mxt_gbif.breadth.B2.md <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B2))

    # enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
    # enm_mxt_gbif.r.m <- cellStats(enm_mxt_gbif.r, stat = "mean")
    # enm_mxt_gbif.r.md <- cellStats(enm_mxt_gbif.r, stat = "median")
    # enm_mxt_gbif.r.sd <- cellStats(enm_mxt_gbif.r, stat = "sd")
  }

  ###
  ### ndopM
  ###

  print("NDOPM")


  enm_mxt_ndop.breadth.B1 <- NA
  enm_mxt_ndop.breadth.B2 <- NA
  enm_mxt_ndop <- list()
  if (!is.na(enm_mxt_ndop.s) && length(enm_mxt_ndop.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_ndop[[r]] <- enmtools.glm(
          enm_mxt_ndop.s,
          eval = eval,
          raster_stack_mask_czechia_b,
          test.prop = test.prop,
          bg.source = bg.source.ndop, # "range" !!!
          verbose = TRUE,
          bias = NA,
          nback = nback_ndop,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_ndop[[r]] <- enmtools.gam(
          enm_mxt_ndop.s,
          eval = eval,
          raster_stack_mask_czechia_b,
          test.prop = test.prop,
          bg.source = bg.source.ndop, # "range" !!!
          verbose = TRUE,
          bias = NA,
          nback = nback_ndop,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_ndop[[r]] <- enmtools.maxent(
          enm_mxt_ndop.s,
          eval = eval,
          raster_stack_mask_czechia_b,
          test.prop = test.prop,
          bg.source = bg.source.ndop, # "range" !!!
          verbose = TRUE,
          bias = NA,
          args = c("removeDuplicates=FALSE", "threads=4"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          removeDuplicates = FALSE,
          threads = 4,
          nback = nback_ndop,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }
    ### NDOP .breadth
    enm_mxt_ndop.breadth <- lapply(enm_mxt_ndop, raster.breadth)
    enm_mxt_ndop.breadth.B1 <- median(sapply(enm_mxt_ndop.breadth, function(x) x$B1))
    enm_mxt_ndop.breadth.B2 <- median(sapply(enm_mxt_ndop.breadth, function(x) x$B2))
    # enm_mxt_ndop.breadth.B1.md <- median(sapply(enm_mxt_ndop.breadth, function(x) x$B1))
    # enm_mxt_ndop.breadth.B2.md <- median(sapply(enm_mxt_ndop.breadth, function(x) x$B2))
  }

  ###
  ### allM (ndpop+gbif)
  ###
  print("ALLM")

  enm_mxt_all.breadth.B1 <- NA
  enm_mxt_all.breadth.B2 <- NA
  enm_mxt_all <- list()
  if (!is.na(enm_mxt_all.s) && length(enm_mxt_all.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.glm(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.gam(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.maxent(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          args = c("removeDuplicates=FALSE", "threads=4"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          removeDuplicates = FALSE,
          threads = 4,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }

    ### ALL .breadth
    enm_mxt_all.breadth <- lapply(enm_mxt_all, raster.breadth)
    enm_mxt_all.breadth.B1 <- median(sapply(enm_mxt_all.breadth, function(x) x$B1))
    enm_mxt_all.breadth.B2 <- median(sapply(enm_mxt_all.breadth, function(x) x$B2))
    # enm_mxt_all.breadth.B1.md <- median(sapply(enm_mxt_all.breadth, function(x) x$B1))
    # enm_mxt_all.breadth.B2.md <- median(sapply(enm_mxt_all.breadth, function(x) x$B2))
  }

  if (breadth_only) {
    return(list(
      enm_mxt_gbif.breadth.B1 = enm_mxt_gbif.breadth.B1, enm_mxt_gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
      enm_mxt_ndop.breadth.B1 = enm_mxt_ndop.breadth.B1, enm_mxt_ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
      enm_mxt_all.breadth.B1 = enm_mxt_all.breadth.B1, enm_mxt_all.breadth.B2 = enm_mxt_all.breadth.B2

      # enm_mxt_gbif.breadth.B1.md = enm_mxt_gbif.breadth.B1.md, enm_mxt_gbif.breadth.B2.md = enm_mxt_gbif.breadth.B2.md,
      # enm_mxt_ndop.breadth.B1.md = enm_mxt_ndop.breadth.B1.md, enm_mxt_ndop.breadth.B2.md = enm_mxt_ndop.breadth.B2.md,
      # enm_mxt_all.breadth.B1.md = enm_mxt_all.breadth.B1.md, enm_mxt_all.breadth.B2.md = enm_mxt_all.breadth.B2.md
    ))
  } else {
    return(list(
      enm_mxt_gbif = enm_mxt_gbif, enm_mxt_ndop = enm_mxt_ndop, enm_mxt_all = enm_mxt_all,

      enm_mxt_gbif.breadth.B1 = enm_mxt_gbif.breadth.B1, enm_mxt_gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
      enm_mxt_ndop.breadth.B1 = enm_mxt_ndop.breadth.B1, enm_mxt_ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
      enm_mxt_all.breadth.B1 = enm_mxt_all.breadth.B1, enm_mxt_all.breadth.B2 = enm_mxt_all.breadth.B2

      # enm_mxt_gbif.r.m = enm_mxt_gbif.r.m,
      # enm_mxt_gbif.r.md = enm_mxt_gbif.r.md,
      # enm_mxt_gbif.r.sd = enm_mxt_gbif.r.sd

      # enm_mxt_gbif.breadth.B1.md = enm_mxt_gbif.breadth.B1.md, enm_mxt_gbif.breadth.B2.md = enm_mxt_gbif.breadth.B2.md,
      # enm_mxt_ndop.breadth.B1.md = enm_mxt_ndop.breadth.B1.md, enm_mxt_ndop.breadth.B2.md = enm_mxt_ndop.breadth.B2.md,
      # enm_mxt_all.breadth.B1.md = enm_mxt_all.breadth.B1.md, enm_mxt_all.breadth.B2.md = enm_mxt_all.breadth.B2.md
    ))
  }
}


fit_modelsN2 <- function(alg, replicates, eval, test.prop, enm_mxt_gbif.s, enm_mxt_ndop.s, enm_mxt_all.s, raster_stack_b, raster_stack_mask_czechia_b, bias_gbif, bias_ndop, bias_all, nback_all, nback_ndop, breadth_only = TRUE, fit_gbif_crop = TRUE, czechia_3035 = NA, bg.source.ndop = "range", bg.source.gbif = "range") {
  # varianta N2 počítá s pozměněným enmtool (zatím lokálně, nutnost nainstalovat), který dělá evaluation fittingu GBIF nad lokálními daty z ČR a nad územím ČR - dostávám tak AUC z testu nad ČR a NDOP daty

  eval <- TRUE
  ###
  ### gbifM
  ###
  print("GBIFM")
  enm_mxt_gbif.breadth.B1 <- NA
  enm_mxt_gbif.breadth.B2 <- NA
  enm_mxt_gbif <- list()
  if (!is.na(enm_mxt_gbif.s) && length(enm_mxt_gbif.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.glm(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = bg.source.gbif, # "range" !!!
          verbose = TRUE,
          bias = bias_gbif,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA),
          speciesInd = enm_mxt_ndop.s,
          nbackInd = nback_ndop,
          envInd = raster_stack_mask_czechia_b
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.gam(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = bg.source.gbif, # "range" !!!
          verbose = TRUE,
          bias = bias_gbif,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_gbif[[r]] <- enmtools.maxent(
          enm_mxt_gbif.s,
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = bg.source.gbif, # "range" !!!
          verbose = TRUE,
          bias = bias_gbif,
          args = c("removeDuplicates=FALSE", "threads=4"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          removeDuplicates = FALSE,
          threads = 4,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }

    ### GBIF .breadth
    enm_mxt_gbif.breadth <- lapply(enm_mxt_gbif, raster.breadth)
    if (fit_gbif_crop == FALSE) {
      enm_mxt_gbif.breadth.B1 <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
      enm_mxt_gbif.breadth.B2 <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B2))
    } else {
      print("GBIFM crop by Czechia")
      sr <- sapply(enm_mxt_gbif, function(x) x$suitability)
      src <- sapply(sr, crop, y = extent(czechia_3035))
      srm <- sapply(src, mask, mask = czechia_3035)
      enm_mxt_gbif.breadth.B1 <- median(unlist(sapply(srm, raster.breadth)[1, ]))
      enm_mxt_gbif.breadth.B2 <- median(unlist(sapply(srm, raster.breadth)[2, ]))
    }

    # enm_mxt_gbif.breadth.B1.md <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
    # enm_mxt_gbif.breadth.B2.md <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B2))

    # enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
    # enm_mxt_gbif.r.m <- cellStats(enm_mxt_gbif.r, stat = "mean")
    # enm_mxt_gbif.r.md <- cellStats(enm_mxt_gbif.r, stat = "median")
    # enm_mxt_gbif.r.sd <- cellStats(enm_mxt_gbif.r, stat = "sd")
  }

  ###
  ### ndopM
  ###

  print("NDOPM")


  enm_mxt_ndop.breadth.B1 <- NA
  enm_mxt_ndop.breadth.B2 <- NA
  enm_mxt_ndop <- list()


  ###
  ### allM (ndpop+gbif)
  ###
  print("ALLM")

  enm_mxt_all.breadth.B1 <- NA
  enm_mxt_all.breadth.B2 <- NA
  enm_mxt_all <- list()
  if (!is.na(enm_mxt_all.s) && length(enm_mxt_all.s) != 0) {
    if (alg == "glm") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.glm(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "gam") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.gam(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
        )
      }
    }
    if (alg == "maxent") {
      for (r in 1:replicates) {
        enm_mxt_all[[r]] <- enmtools.maxent(
          enm_mxt_all.s[[r]],
          eval = eval,
          raster_stack_b,
          test.prop = test.prop,
          bg.source = "range",
          verbose = TRUE,
          bias = bias_all,
          args = c("removeDuplicates=FALSE", "threads=4"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
          removeDuplicates = FALSE,
          threads = 4,
          nback = nback_all,
          corner = ifelse(r == 4, r, NA)
          # args = c("threads=4")
        )
      }
    }

    ### ALL .breadth
    enm_mxt_all.breadth <- lapply(enm_mxt_all, raster.breadth)
    enm_mxt_all.breadth.B1 <- median(sapply(enm_mxt_all.breadth, function(x) x$B1))
    enm_mxt_all.breadth.B2 <- median(sapply(enm_mxt_all.breadth, function(x) x$B2))
    # enm_mxt_all.breadth.B1.md <- median(sapply(enm_mxt_all.breadth, function(x) x$B1))
    # enm_mxt_all.breadth.B2.md <- median(sapply(enm_mxt_all.breadth, function(x) x$B2))
  }

  if (breadth_only) {
    return(list(
      enm_mxt_gbif.breadth.B1 = enm_mxt_gbif.breadth.B1, enm_mxt_gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
      enm_mxt_ndop.breadth.B1 = enm_mxt_ndop.breadth.B1, enm_mxt_ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
      enm_mxt_all.breadth.B1 = enm_mxt_all.breadth.B1, enm_mxt_all.breadth.B2 = enm_mxt_all.breadth.B2

      # enm_mxt_gbif.breadth.B1.md = enm_mxt_gbif.breadth.B1.md, enm_mxt_gbif.breadth.B2.md = enm_mxt_gbif.breadth.B2.md,
      # enm_mxt_ndop.breadth.B1.md = enm_mxt_ndop.breadth.B1.md, enm_mxt_ndop.breadth.B2.md = enm_mxt_ndop.breadth.B2.md,
      # enm_mxt_all.breadth.B1.md = enm_mxt_all.breadth.B1.md, enm_mxt_all.breadth.B2.md = enm_mxt_all.breadth.B2.md
    ))
  } else {
    return(list(
      enm_mxt_gbif = enm_mxt_gbif, enm_mxt_ndop = enm_mxt_ndop, enm_mxt_all = enm_mxt_all,

      enm_mxt_gbif.breadth.B1 = enm_mxt_gbif.breadth.B1, enm_mxt_gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
      enm_mxt_ndop.breadth.B1 = enm_mxt_ndop.breadth.B1, enm_mxt_ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
      enm_mxt_all.breadth.B1 = enm_mxt_all.breadth.B1, enm_mxt_all.breadth.B2 = enm_mxt_all.breadth.B2

      # enm_mxt_gbif.r.m = enm_mxt_gbif.r.m,
      # enm_mxt_gbif.r.md = enm_mxt_gbif.r.md,
      # enm_mxt_gbif.r.sd = enm_mxt_gbif.r.sd

      # enm_mxt_gbif.breadth.B1.md = enm_mxt_gbif.breadth.B1.md, enm_mxt_gbif.breadth.B2.md = enm_mxt_gbif.breadth.B2.md,
      # enm_mxt_ndop.breadth.B1.md = enm_mxt_ndop.breadth.B1.md, enm_mxt_ndop.breadth.B2.md = enm_mxt_ndop.breadth.B2.md,
      # enm_mxt_all.breadth.B1.md = enm_mxt_all.breadth.B1.md, enm_mxt_all.breadth.B2.md = enm_mxt_all.breadth.B2.md
    ))
  }
}

rRMSE <- function(x, y) {
  return(
    sqrt(
      mean(((as.vector(x) - as.vector(y))^2), na.rm = TRUE)
    )
  )
}


rEPS <- function(x, y) {
  # https://rdrr.io/github/adamlilith/enmSdm/src/R/compareNiches.r
  # Godsoe's Expected fraction of Shared Presences (ESP)
  return(
    2 * sum(as.vector(x) * as.vector(y), na.rm = TRUE) / (sum(as.vector(x) + as.vector(y), na.rm = TRUE))
  )
}


select_raster_by_kernel <- function(gbif_top_adj, fm, czechia_3035) {
  nejAdj <- format(as.numeric(gbif_top_adj), nsmall = 2)
  gbif_res <- stack(sapply(fm[[nejAdj]]$enm_mxt_gbif, function(x) x$suitability))
  gbif_res <- calc(gbif_res, fun = median)
  gbif_res.crop <- crop(gbif_res, extent(czechia_3035))
  gbif_res.crop.czechia <- mask(gbif_res.crop, czechia_3035)
  return(gbif_res.crop.czechia)
}



ecospat.boyce2 <- function(fit, obs, nclass = 0, window.w = "default", res = 100,
                           PEplot = TRUE, rm.duplicate = TRUE, method = "spearman") {
  # https://github.com/cran/ecospat/blob/3.2.1/R/ecospat.boyce.R

  #### internal function calculating predicted-to-expected ratio for each class-interval
  boycei <- function(interval, obs, fit) {
    pi <- sum(as.numeric(obs >= interval[1] & obs <= interval[2])) / length(obs)
    ei <- sum(as.numeric(fit >= interval[1] & fit <= interval[2])) / length(fit)
    return(round(pi / ei, 10))
  }

  if (class(fit) == "RasterLayer") {
    if (class(obs) == "data.frame" || class(obs) == "matrix") {
      obs <- extract(fit, obs)
    }
    fit <- getValues(fit)
    fit <- fit[!is.na(fit)]
  }

  mini <- min(fit, obs)
  maxi <- max(fit, obs)

  if (length(nclass) == 1) {
    if (nclass == 0) { # moving window
      if (window.w == "default") {
        window.w <- (max(fit) - min(fit)) / 10
      }
      vec.mov <- seq(from = mini, to = maxi - window.w, by = (maxi - mini - window.w) / res)
      vec.mov[res + 1] <- vec.mov[res + 1] + 1 # Trick to avoid error with closed interval in R
      interval <- cbind(vec.mov, vec.mov + window.w)
    } else { # window based on nb of class
      vec.mov <- seq(from = mini, to = maxi, by = (maxi - mini) / nclass)
      interval <- cbind(vec.mov, c(vec.mov[-1], maxi))
    }
  } else { # user defined window
    vec.mov <- c(mini, sort(nclass[!nclass > maxi | nclass < mini]))
    interval <- cbind(vec.mov, c(vec.mov[-1], maxi))
  }

  f <- apply(interval, 1, boycei, obs, fit)
  to.keep <- which(f != "NaN") # index to keep no NaN data
  f <- f[to.keep]
  if (length(f) < 2) {
    b <- NA # at least two points are necessary to draw a correlation
  } else {
    r <- 1:length(f)
    if (rm.duplicate == TRUE) {
      r <- c(1:length(f))[f != c(f[-1], TRUE)] # index to remove successive duplicates
    }
    b <- cor(f[r], vec.mov[to.keep][r], method = method) # calculation of the correlation (i.e. Boyce index) after removing successive duplicated values
  }
  HS <- apply(interval, 1, sum) / 2 # mean habitat suitability in the moving window
  if (length(nclass) == 1 & nclass == 0) {
    HS[length(HS)] <- HS[length(HS)] - 1 # Correction of the 'trick' to deal with closed interval
  }
  HS <- HS[to.keep] # exclude the NaN
  if (PEplot == TRUE) {
    plot(HS, f, xlab = "Habitat suitability", ylab = "Predicted/Expected ratio", col = "grey", cex = 0.75)
    points(HS[r], f[r], pch = 19, cex = 0.75)
  }
  return(list(F.ratio = f, cor = round(b, 5), HS = HS))
}


# rekurzivní variable permutation importance s postupným odebíráním nejméně přispívajícího prediktoru, jen záloha, není to vhodná metoda...  + bias fitting
permImp_remove_last <- function(species.selected, species.selected.ndop, env.sentinel_bio, path.igaD, replicates, data = NA) {
  if (!is.list(data)) {
    data <- list()
  }
  n_layers <- nlayers(env.sentinel_bio)
  print(paste0("Počet použitých prediktorů/layerů:", n_layers))
  print(names(env.sentinel_bio))
  if (n_layers > 1) {
    n_layers <- as.character(n_layers)
    # m <- list()
    # for (r in 1:replicates) {
    #   m[[r]] <- ENMToolsPB::enmtools.glm(
    #     species = species.selected, env = env.sentinel_bio,
    #     test.prop = "checkerboard2",
    #     # nback = 10000,
    #     # bias = bias_czechia,
    #     bg.source = "points",
    #     corner = r,
    #     verbose = TRUE
    #   )
    # }

    # hledám bias raster s nejlepším AUC [start]
    ba <- list()
    ba.m <- list()
    for (adj in 0:9) {
      bias_czechia <- raster(paste0(path.igaD, "bias-ptaci-adj-0.", as.character(adj), "-kfme16.tif"))
      m <- list()
      for (r in 1:replicates) {
        m[[r]] <- ENMToolsPB::enmtools.glm(
          species.selected.ndop,
          eval = TRUE,
          env.sentinel_bio,
          test.prop = "checkerboard2",
          bg.source = "range",
          verbose = TRUE,
          bias = bias_czechia,
          nback = 10000,
          corner = ifelse(r == 4, r, NA),
          speciesInd = species.selected,
          nbackInd = species.selected,
          envInd = env.sentinel_bio
        )
      }


      ba[[as.character(adj)]] <- m.auc.sentinel_bio <- median(sapply(m, function(x) x$test.evaluation@auc))
      ba.m[[as.character(adj)]] <- m
    }

    data[[n_layers]]$adj.selected <- adj.selected <- names(base::which.max(unlist(ba)))

    m <- ba.m[[adj.selected]]

    # hledám bias raster s nejlepším AUC [end]



    data[[n_layers]]$auc <- median(sapply(m, function(x) x$test.evaluation@auc))



    cm <- lapply(m, function(x) performance(x$conf))
    cm.matrix <- abind(cm, along = 3)
    cm.perf <- apply(cm.matrix, c(1, 2), median)
    cm.perf.t <- as_tibble(cm.perf)

    data[[n_layers]] <- append(data[[n_layers]], as.list(cm.perf.t))


    enm_mxt_gbif.vip <- sapply(
      m, enmtools.vip # , nsim = 100
    )
    enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
      as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
    })

    if (replicates == 1) {
      enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
    } else {
      b_g <- enm_mxt_gbif.vip.t[[1]]

      for (n in 1:replicates) {
        if (n > 1) {
          b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
        }
      }
      enm_mxt_gbif.vip.s <- b_g %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)
    }

    data[[n_layers]]$permImp <- enm_mxt_gbif.vip.s


    # odstraním nejhorší prediktor
    predictors.all <- as_tibble(cbind(nms = names(enm_mxt_gbif.vip.s), t(enm_mxt_gbif.vip.s))) %>%
      mutate(across(V2, as.numeric)) %>%
      arrange(V2)

    predictor.bad <- predictors.all %>% slice_min(V2, n = 1)
    rs_names <- names(env.sentinel_bio)

    rs_ex <- match(substr(predictor.bad$nms, 1, nchar(predictor.bad$nms) - 11), rs_names) # odstraním příponu .Importance
    env.sentinel_bio.minus1 <- dropLayer(env.sentinel_bio, rs_ex)

    data[[n_layers]]$bad.name <- predictor.bad$nms
    data[[n_layers]]$bad.value <- predictor.bad$V2
    if (n_layers == 2) {
      print("end")
      return(data)
    } else {
      print("continue")
      return(permImp_remove_last(species.selected, species.selected.ndop, env.sentinel_bio.minus1, path.igaD, replicates, data))
    }
  } else {
    return(NA)
  }
}



# všechny kombinace prediktorů + bias fitting
permImp_comb <- function(species.selected, species.selected.ndop, env.sentinel_bio, path.igaD, replicates) {
  data <- list()


  ###  # všechny kombinace (prediktorů) bez opakování (10prediktorů 1023 kombinací)
  all.p <- names(env.sentinel_bio)
  all <- c()
  all <- comb_all(all.p) # všechny kombinace bez opakování
  all <- all[-c(1:10)] #  odstraním prvních 10 samostatných prediktorů, nefungoval s mimi model, proč? *** při dočasných 8 prediktorech jsem si tím odstranil 2 dvojice...


  for (pc in all) {
    print("**********")
    print(pc)
    pc.v <- unlist(pc)
    pc.name <- paste(pc.v, collapse = "__")

    data[[pc.name]]$predictors.c <- length(pc.v)
    data[[pc.name]]$predictors.v <- pc.v
    data[[pc.name]]$species <- species.selected$species.name
    data[[pc.name]]$species.ndop.p <- nrow(species.selected.ndop$presence.points)
    data[[pc.name]]$species.p <- nrow(species.selected$presence.points)
    data[[pc.name]]$species.a <- nrow(species.selected$background.points)

    tr <- env.sentinel_bio[[pc.v]]


    # hledám bias raster s nejlepším AUC/Sorensen
    ba <- list()
    ba.m <- list()
    bs <- list()
    metrics.bias <- list()
    aic <- list()
    conf <- list()
    thr <- list()
    for (adj in c(0, 1, 3, 5, 7, 9)) { # ideálně 0:9 / c(0, 1, 3, 5, 7, 9)
      bias_czechia <- raster(paste0(path.igaD, "bias-ptaci-adj-0.", as.character(adj), "-kfme16.tif"))
      m <- list()
      for (r in 1:replicates) {
        m[[r]] <- ENMToolsPB::enmtools.glm(
          species.selected.ndop,
          eval = TRUE,
          tr,
          test.prop = "checkerboard2",
          bg.source = "range",
          verbose = TRUE,
          bias = bias_czechia,
          nback = 10000,
          corner = ifelse(r == 4, r, NA),
          speciesInd = species.selected,
          nbackInd = species.selected,
          envInd = tr
        )
      }
      conf[[as.character(adj)]] <- sapply(m, function(x) x$conf)
      thr[[as.character(adj)]] <- sapply(m, function(x) x$thr)

      aic[[as.character(adj)]] <- median(sapply(m, function(x) x$model$aic))
      # aic[[as.character(adj)]] <- median(sapply(m, function(x) x$model$aic))
      ba[[as.character(adj)]] <- m.auc.sentinel_bio <- median(sapply(m, function(x) x$test.evaluation@auc))
      ba.m[[as.character(adj)]] <- m


      # bb.r <- calc(stack(sapply(m, function(x) x$suitability)), fun = median)
      # pro výběr NDOP použít Boyce? - netřeba, ověřuju lsdHod
      # bb <- ecospat.boyce2(bb.r, species.selected.ndop$presence.points, nclass = 0, PEplot = FALSE, method = "spearman")$cor
      metrics.bias[[as.character(adj)]] <- as.list(performance_models_list(m))
      bs[[as.character(adj)]] <- metrics.bias[[as.character(adj)]]$Sorensen
    }
    data[[pc.name]] <- append(data[[pc.name]], list(bias.aic = aic))
    data[[pc.name]] <- append(data[[pc.name]], list(bias.conf = conf))
    data[[pc.name]] <- append(data[[pc.name]], list(bias.thr = thr))
    data[[pc.name]] <- append(data[[pc.name]], list(bias.auc = ba))
    data[[pc.name]] <- append(data[[pc.name]], list(bias.metrics = metrics.bias))

    # 1) klasické AUC
    data[[pc.name]]$adj.selected <- adj.selected <- names(base::which.max(unlist(ba)))

    m <- ba.m[[adj.selected]]

    # vytvoří adresář pro export rasterů prediktorů, pokud neexistuje
    dir.create(paste0(path.igaD, "predictions"), showWarnings = FALSE)

    rr <- stack(sapply(m, function(x) x$suitability))
    rr <- calc(rr, fun = median)
    # bacha, může toho být hodně v rámci fittování všech kombinací prediktorů a druhů!
    writeRaster(rr, paste0(path.igaD, "predictions/WS1-", data[[pc.name]]$species, "-auc", round(ba[[adj.selected]], 2), "---", pc.name, "---3rep-6bias-10pred--ndop-train_lsdHod-test_GLM-biasFitting-permImpFitting.tif"), format = "GTiff", overwrite = TRUE)



    data[[pc.name]]$auc <- ba[[adj.selected]]
    #  data[[pc.name]]$auc.totez <- median(sapply(m, function(x) x$test.evaluation@auc))
    #  data[[pc.name]]$aic <- median(sapply(m, function(x) x$model$aic))

    metrics <- as.list(performance_models_list(m))
    data[[pc.name]] <- append(data[[pc.name]], list(auc.metrics = metrics))


    # 1) Sorensen

    data[[pc.name]]$adj.selected.sorensen <- adj.selected.sorensen <- names(base::which.max(unlist(bs)))

    m2 <- ba.m[[adj.selected.sorensen]]

    data[[pc.name]]$sorensen <- bs[[adj.selected.sorensen]]

    metrics2 <- as.list(performance_models_list(m2))
    data[[pc.name]] <- append(data[[pc.name]], list(sorensen.metrics = metrics2))





    # # teď už nepotřebuju, zdržuje
    #     enm_mxt_gbif.vip <- sapply(
    #       m, enmtools.vip # , nsim = 100
    #     )
    #     enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
    #       as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
    #     })

    #     if (replicates == 1) {
    #       enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
    #     } else {
    #       b_g <- enm_mxt_gbif.vip.t[[1]]

    #       for (n in 1:replicates) {
    #         if (n > 1) {
    #           b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
    #         }
    #       }
    #       enm_mxt_gbif.vip.s <- b_g %>%
    #         summarise_if(is.numeric, mean, na.rm = TRUE)
    #     }

    #     data[[pc.name]]$permImp <- enm_mxt_gbif.vip.s
  }


  return(data)
}


# is.defined <- function(sym) {
#   # https://stackoverflow.com/a/43446356
#   sym <- deparse(substitute(sym))
#   env <- parent.frame()
#   exists(sym, env)
# }



# # # # #  manuální kontrola použitých koeficientů pro adjust pro jednotlivé druhy
# required_packages <-
#     c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "dismo", "ENMToolsPB", "spatstat", "purrr", "abind") # "rmaxent", "blockCV", "ggplot2", "MASS", "data.table", "virtualspecies" (convertToPA - problematické definovat parametry v reálném světě...)
# install.packages(setdiff(required_packages, rownames(installed.packages())))
# # načte všechny požadované knihovny jako dělá jednotlivě library()
# lapply(required_packages, require, character.only = TRUE)



# px_size_item <- 5000
# cmd_arg_str  <- 4
# export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom"
#         fm_gbif_f_i_c <- readRDS(paste0(export_path, "/inputs/occurrences/fm_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
#         fm_ndop_f_i_c <- readRDS(paste0(export_path, "/inputs/occurrences/fm_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
#         fm_all_f_i_c <- readRDS(paste0(export_path, "/inputs/occurrences/fm_all_", px_size_item, "-", cmd_arg_str, ".rds"))

#       ndop_df <- as_tibble(fm_ndop_f_i_c$`5000`)
#        print(as_tibble(cbind(nms = names(ndop_df ), t(ndop_df ))) %>% mutate(across(V2, as.numeric)) %>% arrange(V2), n = 100)


#       all_df <- as_tibble(fm_all_f_i_c$`5000`)
#         print(as_tibble(cbind(nms = names(all_df ), t(all_df )))  %>% mutate(across(V2, as.numeric)) %>% arrange(V2), n = 100)


#       gbif_df <- as_tibble(fm_gbif_f_i_c$`5000`)
#         print(as_tibble(cbind(nms = names(gbif_df ), t(gbif_df)))  %>% mutate(across(V2, as.numeric)) %>% arrange(V2), n = 100)