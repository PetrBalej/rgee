# https://github.com/r-spatial/rgee#3-create-an-ndvi-animation-js-version

library(rgee)
library(reticulate)
library(sf)
# library(sp)
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


#
# b) podle shapefile hranice
#

#shpPath <- "/media/petr/data/puv-20gb/petr/bordel-ke-smazani/phd/rgee/shp/ne_50m_admin_0_countries/czechia/cz_4326.shp"

#boundingBox_ee <- st_read(shpPath) %>% sf_as_ee()
#region <- boundingBox_ee$geometry()$bounds()

#
# konec b)
#



# modis
col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')


col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})

# rok 2019
distinctDOY <- col$filterDate('2019-01-01', '2020-01-01')

filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy');

join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))


comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})


visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
    )
)


rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>% 
    ee$Image$clip(boundingBox_ee)
})

gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:4326',
  framesPerSecond = 10
)


print(rgbVis$getVideoThumbURL(gifParams))
browseURL(rgbVis$getVideoThumbURL(gifParams))
