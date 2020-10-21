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


date <- c('2016-04-05', '2016-04-06') 

dataset_SR <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterDate(date[1], date[2])$map(maskL8sr);

tmp <- tempdir()

## Export do rastru???
ic_drive_files <- ee_imagecollection_to_local(
  ic = dataset_SR,
  region = region,
  scale = 100,
  dsn = file.path(tmp, "test_export_")
)


