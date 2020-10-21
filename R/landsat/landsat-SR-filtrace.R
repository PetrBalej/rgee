library(rgee)
library(reticulate)
library(sf)
library(stars) # download imageCollection (req. abind)
ee_Initialize()


#
# a) vlastní bounding box
#


# bounding box manuálně (nikoliv ze shp)
# - česko a okolí
# xmin <- 12.0
# xmax <- 19.0
# ymin <- 48.0
# ymax <- 51.0

# - testovací malý výsek SZ Čechy: Chomutov, Ohře, Nechranice, Doupovské hory
xmin <- 13.0
xmax <- 13.5
ymin <- 50.0
ymax <- 50.5


boundingBox <- cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin))
boundingBox_st <- st_polygon(list(boundingBox))
boundingBox_ee <- boundingBox_st  %>% sf_as_ee()

region <- boundingBox_ee

# pokud nemám obdélník, ale jiný tvar ze kterého musím získat obdélník/hranice:
region <- boundingBox_ee$bounds()

#
# konec a)
#


maskL8sr <- function(image) {
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

#date <- c('2016-04-05', '2016-04-06') 
date <- c('2016-04-01', '2016-09-30') 

dataset_SR <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterDate(date[1], date[2])$select('B3') %>% ee$ImageCollection$median()


#$map(maskL8sr)
#$map(function(x) x$reproject("EPSG:4326"))

#$select('B[1-12]')

tmp <- tempdir()

# ee_crs <- dataset_SR$first()$projection()$getInfo()$crs
# proj <- ee$Projection();

# Export do rastru???
ic_drive_files <- ee_imagecollection_to_local(
  ic = dataset_SR,
  region = region,
  scale = 100,
  dsn = file.path(tmp, "test_export5_")
)



l8sr <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')

noClipNeeded <- l8sr$
  select('B[1-12]')$                          # Good.
  filterBounds(region)$          # Good.
  filterDate('2019-01-01', '2019-12-31')$ # Good.
  median()$
  reduceRegion(
    reducer = ee$Reducer$median(),
    geometry = region, # Geometry is specified here.
    scale = 30,
    maxPixels = 1e10
  )

#
# -----------------------------------------------------------------------------------------------
#

library(rgee)
library(raster)
ee_Initialize()

# USDA example
#loc <- ee$Geometry$Point(13.0, 50.0)

loc <- ee$Geometry$Polygon(
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


collection <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$
  filterBounds(loc)$
  #filterDate('2018-01-01', '2020-01-01')$
  filterDate(date[1], date[2])$
  map(maskL8sr)$
  select('B3')$
  median()
#  reduceRegion(
#    reducer = ee$Reducer$median(),
#    geometry = region, # Geometry is specified here.
#    scale = 100,
#    maxPixels = 1e10
#  )

# From ImageCollection to local directory
ee_crs <- collection$first()$projection()$getInfo()$crs
geometry <- collection$first()$geometry(proj = ee_crs)$bounds()
tmp <- tempdir()

## Using drive
ic_drive_files <- ee_imagecollection_to_local(
  ic = collection,
  region = geometry,
  scale = 100,
  dsn = file.path(tmp, "driveqqq4_")
)



