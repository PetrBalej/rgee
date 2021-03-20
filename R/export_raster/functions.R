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
      # pokud jde o NDVI, nutná úprava před přetypováním na INT16
      if (grepl("ndvi", tolower(dsn), fixed = TRUE) == TRUE) {
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
  raster_stack <- stack(lapply(rasters_list, raster))
  return(raster_stack)
}