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
    sublist <- list(short = toString(gee_datasets[i, "short"]), geeSnippet = toString(gee_datasets[i, "geeSnippet"]), type = toString(gee_datasets[i, "type"]))
    gdl[toString(gee_datasets[i, "short"])] <- list(sublist)
  }
  return(gdl)
}


# odstranění stínů a oblačnosti
mask_L8_sr <- function(image) {
  # Get the pixel QA band.
  qa <- image$select('pixel_qa')

  # https://www.usgs.gov/media/files/landsat-8-collection-1-land-surface-reflectance-code-product-guide
  # Landsat 8 Collection 1 (C1) Land Surface Reflectance Code (LaSRC) Product Guide
  # LSDS-1368 Version 3.0
  # Table 6-3. Landsat 8 Pixel Quality Assessment (pixel_qa) Values
  # 322: Clear terrain, low-confidence cloud, low-confidence cirrus
  # 324: Water, low-confidence cloud, low-confidence cirrus
  mask <- qa$eq(322)$bitwiseOr(qa$eq(324));

  return(image$updateMask(mask))
}

# export rasterů z GEE image
export_gee_image <- function(image, region, scale, dsn, format, bands = c()) {

  bands_count <- length(bands)

  multiband_support <- list(grd = "raster", nc = "CDF", tif = "GTiff", envi = "ENVI", bil = "EHdr", img = "HFA")
  multiband_not_support <- list(asc = "ascii", sdat = "SAGA", rst = "IDRISI")

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

  ee_as_raster_support <- c("tif", "envi", "img")

  if (is.element(ext, multiband_not_support_ext) && bands_count > 1) {
    stop(paste0("Output file format ", format, " have not multiband support!"))
  }

  dsn_format <- paste0(dsn, ".", ext)
  if (!is.element(ext, ee_as_raster_support)) {
    # pokud nejde o formát podporovaný ee_as_raster tak nechám vytvořit jen v temp dočasný tif
    dsn_format <- NULL
    # průběžně promazávat temp tiffy s jedním bandem?
  }

  result_raster <- ee_as_raster(
    image = image,
    region = region,
    scale = scale,
    via = "getInfo",
    dsn = dsn_format
  # maxPixels = 1e10
  )

  if (!is.element(ext, ee_as_raster_support)) {
    # musím použít writeRaster
    writeRaster(result_raster[[bands[1]]], dsn, ext_wr, overwrite = TRUE)
  }

}
