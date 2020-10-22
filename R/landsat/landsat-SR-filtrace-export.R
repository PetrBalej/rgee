library(rgee)

ee_Initialize()
# ee_user_info()

# odefinice obálek (bounding box) různě velkých území pro testování
sz_cechy <- list(xmin = 13.0, xmax = 13.5, ymin = 50.0, ymax = 50.5)
cesko <- list(xmin = 12.0, xmax = 19.0, ymin = 48.5, ymax = 51.5)
str_evropa <- list(xmin = 8.5, xmax = 22.0, ymin = 46.0, ymax = 53.5)

####################################################################################

# nastavení základních parametrů

# výběr území
bb <- sz_cechy

# výsledná velikost pixelu v km
scale = 10000 

# rozsah snímků od/do
years_range <- list(from = '2017-01-01', to = '2019-12-31') 

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
season_months_range <- list(from = 4, to = 9) 

####################################################################################


xmin <- bb$xmin
xmax <- bb$xmax
ymin <- bb$ymin
ymax <- bb$ymax


bb_polygon <- ee$Geometry$Polygon(
  coords = list(
    c(xmin, ymin),
    c(xmin, ymax),
    c(xmax, ymax),
    c(xmax, ymin),
    c(xmin, ymin)
  ),
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


l8_sr_collection <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$
  filterBounds(bb_polygon)$
  filterDate(years_range$from, years_range$to)$
  filter(ee$Filter$calendarRange(season_months_range$from, season_months_range$to, "month"))$
  map(mask_L8_sr)

bands_vis = c("B4", "B3", "B2")
bands_all = c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11")
# l8_sr_collection_reduce = l8_sr_collection$select(bands_prep)$reduce(ee$Reducer$median())$rename(bands_prep) #$reproject("EPSG:32633")
l8_sr_collection_reduce = l8_sr_collection$select(bands_all)$reduce(ee$Reducer$median())$rename(bands_all) #$reproject("EPSG:32633")

# vizualizace v mapovém okně
visparams <- list(
  bands = bands_vis,
  min = 0,
  max = 3000,
  gamma = 1.4
)

geometry <- ee$Geometry$Rectangle(
  coords = c(xmin, ymin, xmax, ymax),
  proj = "EPSG:4326",
  geodesic = FALSE
)

#Map$setCenter(13.0, 50.0, 10)
Map$centerObject(geometry, zoom = 9)
l1 <- Map$addLayer(geometry, visParams = list(color = "FF0000"), opacity = 0.3, name = "vybraná obálka (bounding box)")
l2 <- Map$addLayer(l8_sr_collection_reduce, visparams, name = "LANDSAT/LC08/C01/T1_SR filtered median") 
l2 + l1


# export do rasteru se všemi bandy (RasterStack object)
result_raster <- ee_as_raster(
  image = l8_sr_collection_reduce, #$reproject("EPSG:32633")
  region = geometry,
  scale = scale 
  #maxPixels = 1e10
)

# pro zjištění vygenerovaného názvu tiff-u v /temp
# str(result_raster)

# přístup přes: result_raster$B1 (RasterLayer)
# result_raster$B1[2,3] # hodnota pixelu B1 bandu na 2. řádku a 3. sloupci
# result_raster$B1[1:3,1:2] # hodnota pixelu B1 bandu na 1-3. řádku a 1-2. sloupci



library(raster) # pro použití writeRaster

home_path <- path.expand("~") # domovský adresář

# 'ascii' je přímo ESRI Ascii formát 
writeRaster(result_raster$B3, paste0(home_path, "/export_band.asc"), 'ascii', overwrite = TRUE)

