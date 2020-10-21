# https://github.com/r-spatial/rgee#3-create-an-ndvi-animation-js-version

library(rgee)
library(reticulate)
library(sf)
ee_Initialize()


#
# a) vlastní bounding box
#


# bounding box manuálně (nikoliv ze shp) - česko a okolí
xmin <- 12.0
xmax <- 19.0
ymin <- 48.0
ymax <- 51.0

boundingBox <- cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin))
boundingBox_st <- st_polygon(list(boundingBox))
boundingBox_ee <- boundingBox_st  %>% sf_as_ee()

region <- boundingBox_ee

# pokud nemám obdélník, ale jiný tvar ze kterého musím získat obdélník/hranice:
region <- boundingBox_ee$bounds()

#
# konec a)
#


# Landsat 8 filtrace polygonem a časem
col <- ee$ImageCollection('LANDSAT/LC08/C01/T1_32DAY_NDVI')$filterBounds(region)$filterDate('2019-04-01', '2019-07-01')

# počet snímků ve výběru
col$size()$getInfo()


# vlastnosti (pásma) konkrétního snímku
img <- ee$Image('LANDSAT/LC08/C01/T1/LC08_044034_20140318')
img$bandNames()$getInfo()


# -----------------------------------------------------------------------

maskL8sr <- function(image) {
  # Bits 3 and 5 are cloud shadow and cloud, respectively.

  # Get the pixel QA band.
  qa <- image$select('pixel_qa')
  # Both flags should be set to zero, indicating clear conditions.
  mask <- qa$eq(322)$bitwiseOr(qa$eq(324));
  return (image$updateMask(mask))
}


date <- c('2016-04-05', '2016-04-06') 

dataset_SR <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')$filterDate(date[1], date[2])$map(maskL8sr);

