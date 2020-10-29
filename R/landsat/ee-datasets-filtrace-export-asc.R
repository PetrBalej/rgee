start_time <- Sys.time()
library(rgee)
library(raster)

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv") # , "googledrive", "httpuv"
install.packages(setdiff(required_packages, rownames(installed.packages())))

ee_Initialize(drive = FALSE, gcs = FALSE)
# ee_user_info()

# při odpojení nebo zneplatnění původního přihlašovacího tokenu
# ee_clean_credentials()

# tempdir() # dočasný adresář pro aktuální R session

# definice obálek (bounding box) různě velkých území pro testování
sz_cechy <- list(xmin = 13.0, xmax = 13.5, ymin = 50.0, ymax = 50.5)
cesko <- list(xmin = 12.0, xmax = 19.0, ymin = 48.5, ymax = 51.5)
str_evropa <- list(xmin = 8.5, xmax = 22.0, ymin = 46.0, ymax = 53.5)

####################################################################################

# nastavení základních parametrů

# výběr území
bb <- sz_cechy

# výsledná velikost pixelu v m
scale <- 10000 

# rozsah snímků od/do
years_range <- list(from = '2017-01-01', to = '2019-12-31') 

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
season_months_range <- list(from = 4, to = 9) 

# domovský adresář
home_path <- path.expand("~")

# adresář pro exportované soubory v rámci home_path
export_path <- paste0(home_path, "/Downloads/rgee2/export")

# adresář pro dočasné soubory v rámci export_path
temp_path <- paste0(export_path, "/temp")

# zobrazit mapové okno s polygonem oblasti a RGB kompozitem?
vis_map <- FALSE

# parametry použitých datasetů z GEE
gee_datasets <- read.csv(file = 'gee_datasets/gee-pouzite-datasety.csv')
# cell <- subset(gee_datasets, short == "landsat", select = "geeSnippet")

time_name <- Sys.time()


# v cyklu průběžně promazávat temp tiffy s jedním bandem

####################################################################################

# průběžné mazání temp (hlavně zbytečných tiffů jako meziproduktů k .asc)
clear_temp <- function() {
unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
}

# gc() # Force R to release memory it is no longer using


# převod gee_datasets do přístuupnějšího listu
gdl <- list()
for(i in 1:nrow(gee_datasets)) 
{
sublist <- list(geeSnippet = toString(gee_datasets[i, "geeSnippet"]), type = toString(gee_datasets[i, "type"]))
gdl[toString(gee_datasets[i, "short"])] <- list(sublist)
}

xmin <- bb$xmin
xmax <- bb$xmax
ymin <- bb$ymin
ymax <- bb$ymax

bb_geometry_rectangle <- ee$Geometry$Rectangle(
  coords = c(xmin, ymin, xmax, ymax),
  proj = "EPSG:4326",
  geodesic = FALSE
)



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

  return (image$updateMask(mask))
}

################################
# L8 _SR 'LANDSAT/LC08/C01/T1_SR' - raw bandy
################################

l8_sr_collection <- ee$ImageCollection(gdl$landsat$geeSnippet)$
  filterBounds(bb_geometry_rectangle)$
  filterDate(years_range$from, years_range$to)$
  filter(ee$Filter$calendarRange(season_months_range$from, season_months_range$to, "month"))$
  map(mask_L8_sr)

# medián pro výslednou hodnotu pixelu - export všech bandů
# l8_sr_collection_reduce <- l8_sr_collection$select(bands_all)$reduce(ee$Reducer$median())$rename(bands_all) #$reproject("EPSG:32633")


# bands_all <-  l8_sr_collection$first()$bandNames()$getInfo()

# https://landsat.gsfc.nasa.gov/data/how-to-use-landsat-data/
# B11 - nedoporučeno používat
# B10 jen s atm. korekcemi z atmcorr.gsfc.nasa.gov - jsou součástí }spo4tenz] GEE L8 _SR datasetů?!
# u B10 by navíc bylo vhodné odfiltrovat pryč vodní toky a plochy, pokud se na nich něco nemůže vyskytovat (tváří se jako lesy a jiná chladná místa)
bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10") 

# mělo by být funkční pro kteroukoliv kolekci z GEE (dodělat)
# - u každé kolekce/datasetu ale stejně PŘEDEM musím vědět které bandy (případně i s jakými analýzami nad nimi) použít


for (band in bands_all) {
print(band)
l8_sr_collection_reduce_1_band <- l8_sr_collection$select(band)$reduce(ee$Reducer$median())$rename(band) #$reproject("EPSG:32633")

# export do meziproduktu v podobě tiffu s jedním bandem
result_raster <- ee_as_raster(
  image = l8_sr_collection_reduce_1_band, #$reproject("EPSG:32633")
  region = bb_geometry_rectangle,
  scale = scale,
  via = "getInfo", # na Ubuntu nebylo nutné vůbec uvádět tento parametr, na Win10 si to jinak vynucovalo přihlášení a následné ukládání na Google disk
  # maxPixels = 1e10
  dsn = paste0(temp_path, "/l8_", time_name, "_", band, ".tif") # Output filename. If missing, will create a temporary file.
)

# 'ascii' je přímo ESRI Ascii formát 
#  jak tam dostat nodata value???
writeRaster(result_raster[[band]], paste0(temp_path, "/l8_", time_name, "_", band, ".asc"), 'ascii', overwrite = TRUE)

}



################################
# L8 _SR 'LANDSAT/LC08/C01/T1_SR' - NDVI
################################

bands_all <- c("B5", "B4") 

# ndvi <- l8_sr_collection_reduce$normalizedDifference(c("B5", "B4"))
ndvi <- l8_sr_collection$select(bands_all)$reduce(ee$Reducer$median())$rename(bands_all)$normalizedDifference(bands_all)$rename("NDVI")

result_raster <- ee_as_raster(
  image = ndvi$select("NDVI"), #$reproject("EPSG:32633")
  region = bb_geometry_rectangle,
  scale = scale,
  via = "getInfo", # na Ubuntu nebylo nutné vůbec uvádět tento parametr, na Win10 si to jinak vynucovalo přihlášení a následné ukládání na Google disk
  # maxPixels = 1e10
  dsn = paste0(temp_path, "/ndvi_", time_name, ".tif") # Output filename. If missing, will create a temporary file.
)

writeRaster(result_raster[["NDVI"]], paste0(temp_path, "/ndvi_", time_name, ".asc"), 'ascii', overwrite = TRUE)


################################
# Worldclim/Bioclim 'WORLDCLIM/V1/BIO'
################################

wc <- ee$Image(gdl$worldclim$geeSnippet)

bands_all <- wc$bandNames()$getInfo()

for (band in bands_all) {
print(band)
wc_1_band <- wc$select(band)$rename(band) #$reproject("EPSG:32633")

# export do meziproduktu v podobě tiffu s jedním bandem
result_raster <- ee_as_raster(
  image = wc_1_band, #$reproject("EPSG:32633")
  region = bb_geometry_rectangle,
  scale = scale,
  via = "getInfo", # na Ubuntu nebylo nutné vůbec uvádět tento parametr, na Win10 si to jinak vynucovalo přihlášení a následné ukládání na Google disk
  # maxPixels = 1e10
  dsn = paste0(temp_path, "/wc_", time_name, "_", band, ".tif") # Output filename. If missing, will create a temporary file.
)

writeRaster(result_raster[[band]], paste0(temp_path, "/wc_", time_name, "_", band, ".asc"), 'ascii', overwrite = TRUE)

}




################################
# SRTM 'USGS/SRTMGL1_003'
################################

srtm <- ee$Image(gdl$srtm$geeSnippet)$select(c("elevation"))

# export do meziproduktu v podobě tiffu s jedním bandem
result_raster <- ee_as_raster(
  image = srtm, #$reproject("EPSG:32633")
  region = bb_geometry_rectangle,
  scale = scale,
  via = "getInfo", # na Ubuntu nebylo nutné vůbec uvádět tento parametr, na Win10 si to jinak vynucovalo přihlášení a následné ukládání na Google disk
  # maxPixels = 1e10
  dsn = paste0(temp_path, "/srtm_", time_name, "_", band, ".tif") # Output filename. If missing, will create a temporary file.
)

writeRaster(result_raster[["elevation"]], paste0(temp_path, "/srtm_", time_name, ".asc"), 'ascii', overwrite = TRUE)


################################
# SRTM 'USGS/SRTMGL1_003' + slope, aspect...
################################





################################
# corine 'COPERNICUS/CORINE/V20/100m/2018'
################################

# corine <- ee$Image(gdl$corine$geeSnippet)$select(c("landcover"))

# export do meziproduktu v podobě tiffu s jedním bandem
# result_raster <- ee_as_raster(
#   image = corine, #$reproject("EPSG:32633")
#   region = bb_geometry_rectangle,
#   scale = scale,
#   via = "getInfo", # na Ubuntu nebylo nutné vůbec uvádět tento parametr, na Win10 si to jinak vynucovalo přihlášení a následné ukládání na Google disk
#   # maxPixels = 1e10
#   dsn = paste0(temp_path, "/corine_", time_name, ".tif") # Output filename. If missing, will create a temporary file.
# )

# writeRaster(result_raster[["elevation"]], paste0(temp_path, "/srtm_", time_name, ".asc"), 'ascii', overwrite = TRUE)




# pro zjištění vygenerovaného názvu tiff-u v /temp
# str(result_raster)

# přístup přes: result_raster$B1 (RasterLayer)
# result_raster$B1[2,3] # hodnota pixelu B1 bandu na 2. řádku a 3. sloupci
# result_raster$B1[1:3,1:2] # hodnota pixelu B1 bandu na 1-3. řádku a 1-2. sloupci


# NDVI z kolekce
# ndvi <- l8_sr_collection_reduce$normalizedDifference(c("B5", "B4"))

# export jednoho bandu
# l8_sr_collection_reduce_B2 <- l8_sr_collection_reduce$select("B2")



if(vis_map){

bands_vis = c("B4", "B3", "B2")

# vizualizace v mapovém okně
visparams <- list(
  bands = bands_vis,
  min = 0,
  max = 3000,
  gamma = 1.4
)

# Map$setCenter(13.0, 50.0, 10)
Map$centerObject(bb_geometry_rectangle, zoom = 9)
l1 <- Map$addLayer(bb_geometry_rectangle, visParams = list(color = "FF0000"), opacity = 0.3, name = "vybraná obálka (bounding box)")
l2 <- Map$addLayer(l8_sr_collection_reduce, visparams, name = "LANDSAT/LC08/C01/T1_SR filtered median") 
l2 + l1
}

end_time <- Sys.time()
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)


# library(rgdal)
# library(raster)
# currentEnv=getData("worldclim", var="bio", res=5) # stáhne 35 MB .zip
# str(currentEnv)
# # Formal class 'RasterStack' [package "raster"] with 11 slots - totožné jako result_raster - použitelné do dismo(maxent) jako vstup