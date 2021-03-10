# wd <- paste0(path.expand("~"), "/git-projekty/rgee-RB")
# setwd(wd)

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("sp",
    "rgdal",
    "raster",
    "stars",
    "httpuv")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

### pomocné funkce
toInt_NDVI <-
  function(x) {
    return (as.integer(round(x, 4) * 10000))
  }
toInt <- function(x) {
  return (as.integer(x))
}
doCalc_NDVI <- function(r) {
  return(calc(r, fun = toInt_NDVI))
}
doCalc <- function(r) {
  return(calc(r, fun = toInt))
}

doWR <- function(r, p) {
  crs(r) <- CRS("+init=EPSG:3035")
  # NAvalue(r) <- -9999
  # paste0(gsub(".tif", "_xxx3.tif", p))
  return(
    writeRaster(
      r,
      p,
      "GTiff",
      datatype = "INT2S",
      overwrite = TRUE,
      options = "COMPRESS=DEFLATE"
    )
  )
}

### přetypuje rastery na INT2S (=Int16) za účelem zmenšené datové velikosti, u NDVI je nutno posunout des. čárku
retype_raster <- function(path_dir, raster_extension = "tif") {
  # zpracování NDVI
  rasters_list_NDVI <-
    list.files(
      path = path_dir,
      pattern = paste0("^l8__NDVI\\.", raster_extension, "$"),
      ignore.case = TRUE,
      full.names = TRUE,
      recursive = TRUE
    )
  
  mapply(doWR, sapply(sapply(rasters_list_NDVI, raster), doCalc_NDVI), p = rasters_list_NDVI)
  
  # zpracování ostatních rasterů už je totožné
  rasters_list_all <-
    list.files(
      path = path_dir,
      pattern = paste0("\\.", raster_extension, "$"),
      ignore.case = TRUE,
      full.names = TRUE,
      recursive = TRUE
    )
  rasters_list_all <-
    rasters_list_all[!rasters_list_all %in% rasters_list_NDVI]
  
  mapply(doWR, sapply(sapply(rasters_list_all, raster), doCalc), p = rasters_list_all)
}

# path_dir <- "C:/Users/balej/Documents/git-projekty/rgee-RB/export/30"
# retype_raster(path_dir)
