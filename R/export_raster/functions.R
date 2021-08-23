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


# odstranění stínů a oblačnosti
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
  Jaccard <- TPR / (FNR + TPR + FPR)
  Sorensen <- 2 * TPR / (FNR + 2 * TPR + FPR)
  F_measure <- 2 * Jaccard
  OPR <- fp / (tp + fp)
  UPR <- 1 - Sensitivity
  data.frame(
    TPR = TPR, TNR = TNR, FPR = FPR, FNR = FNR, Sensitivity = Sensitivity, Specificity = Specificity,
    TSS = TSS, Jaccard = Jaccard, Sorensen = Sorensen, F_measure = F_measure, OPR = OPR, UPR = UPR
  )
}


get_freq_by_cat <- function(freq.df, cat.id) {
  if (!is.na(freq.df)) {
    # získání hodnot dle kategorie početnosti z freq()
    if (is.empty(which(freq.df[, 1] == cat.id))) {
      return(0)
    } else {
      return(unname(freq.df[which(freq.df[, 1] == cat.id), 2]))
    }
  } else {
    return(NA)
  }
}

rasters_confusion <- function(reality, prediction) {
  if (!is.na(reality) && !is.na(prediction)) {
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

  raster_stack_Bx[raster_stack_Bx == -9999] <- NA
  raster_stack_I[raster_stack_I == -32768] <- NA

  raster_stack <- stack(raster_stack_Bx, raster_stack_I, raster_stack_biox)

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
  return(x)
}

floor_df <- function(x, digits) {
  # pro optimalizaci tvorby bufferů, použít?
  numcols <- sapply(x, mode) == "numeric"
  x[numcols] <- x[numcols] / (10^abs(digits))
  x[numcols] <- floor(x[numcols])
  x[numcols] <- x[numcols] * (10^abs(digits))
  return(x)
}


normalize <- function(x) {
  min <- raster::minValue(x)
  max <- raster::maxValue(x)
  return((x - min) / (max - min))
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


fit_modelsN <- function(alg, replicates, eval, test.prop, enm_mxt_gbif.s, enm_mxt_ndop.s, enm_mxt_all.s, raster_stack_b, raster_stack_mask_czechia_b, bias_gbif, bias_ndop, bias_all, nback_all, nback_ndop, breadth_only = FALSE) {



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
    enm_mxt_gbif.breadth.B1 <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
    enm_mxt_gbif.breadth.B2 <- median(sapply(enm_mxt_gbif.breadth, function(x) x$B2))
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
          bg.source = "range",
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
          bg.source = "range",
          verbose = TRUE,
          bias = NA,
          args = c("removeDuplicates=FALSE", "outputFormat=raw"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
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
          args = c("removeDuplicates=FALSE", "outputFormat=raw"), # většinově nemají předávajné parametry žádný efekt, používá se jen pro model, ne pro predikci/projekci, tu si enmtools dělá samo přes predict()
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
      mean((as.vector(x) - as.vector(y))^2, na.rm = TRUE)
    )
  )
}


rEPS <- function(x, y) {
  # https://rdrr.io/github/adamlilith/enmSdm/src/R/compareNiches.r
  # Godsoe's Expected fraction of Shared Presences (ESP)
  return(
    2 * sum(x * y, na.rm = TRUE) / (sum(x + y, na.rm = TRUE))
  )
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