start_time <- Sys.time()

# předem nainstalováno...
library(rgee)

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv")
install.packages(setdiff(required_packages, rownames(installed.packages())))

library(raster)

ee_Initialize(drive = FALSE, gcs = FALSE)
# ee_user_info()

# při odpojení nebo zneplatnění původního přihlašovacího tokenu
# ee_clean_credentials()

# tempdir() # dočasný adresář pro aktuální R session


# # # # # # # # # # # # # # # # # # # # # #
# nastavení základních parametrů [start]  #
# # # # # # # # # # # # # # # # # # # # # #


## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
home_path <- path.expand("~")

# adresář pro exportované soubory (v rámci home_path)
export_path <- paste0(home_path, "/Downloads/rgee2/export")

# GIT project directory (kompletní repozitář rgee z github.com: po rozbalení zipu v rgee-master/rgee-master)
git_project_path <- paste0(home_path, "/Downloads/rgee2/rgee")


## výběr regionu

# definice obálek (bounding box) různě velkých území pro testování
sz_cechy <- list(xmin = 13.0, xmax = 13.5, ymin = 50.0, ymax = 50.5)
cesko <- list(xmin = 12.0, xmax = 19.0, ymin = 48.5, ymax = 51.5)
str_evropa <- list(xmin = 8.5, xmax = 22.0, ymin = 46.0, ymax = 53.5)

# výběr konkrétního území
bb <- sz_cechy


## časové rozsahy

# rozsah snímků od/do
years_range <- list(from = '2017-01-01', to = '2019-12-31')

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
season_months_range <- list(from = 4, to = 9)


## výsledná velikost pixelu v m
scale <- 10000

## výstupní fotmát exportovaných rasterů
output_raster_ext <- "asc" # tif, grd, envi, img

## zobrazit mapové okno s polygonem oblasti a RGB kompozitem?
vis_map <- FALSE

## jednotná "značka" přidaná ke všem output rasterům z jednoho běhu skriptu (stejné nastavení parametrů)
tag_name <- gsub('[^0-9-]', '-', Sys.time())

## NoDataValue
no_data_value <- -9999 # vede k -3.4e+38 

## minimum sat. snímků k použití pixelu pro analýzu, jinak no_data_value
# nechávat raději 0 a až dodatečně použít příslušný vygenerovaný raster k domaskování? Jinak tím poznamenám všechny uložené rastery (dořešit)
threshold_px_count <- 3

# # # # # # # # # # # # # # # # # # # # # #
# nastavení základních parametrů [konec]  #
# # # # # # # # # # # # # # # # # # # # # #




# parametry použitých datasetů z GEE - export z gee_datasets/gee-pouzite-datasety.xlsx
gee_datasets_path_csv <- paste0(git_project_path, "/gee_datasets/gee-pouzite-datasety.csv")

# načtení potřebných funkcí
source(paste0(git_project_path, "/R/export_raster/functions.R"))

# načtení csv s datasety z GEE
gdl <- gee_datasets_list(gee_datasets_path_csv)

xmin <- bb$xmin
xmax <- bb$xmax
ymin <- bb$ymin
ymax <- bb$ymax

bb_geometry_rectangle <- ee$Geometry$Rectangle(
  coords = c(xmin, ymin, xmax, ymax),
  proj = "EPSG:4326",
  geodesic = FALSE
)


################################################################
# L8 _SR 'LANDSAT/LC08/C01/T1_SR' - raw bandy
################################################################

# aplikace základních geogracických, časových a odmračňovacích/odstiňovacích filtrů
l8_sr_collection <- ee$ImageCollection(gdl$landsat$geeSnippet)$
  filterBounds(bb_geometry_rectangle)$
  filterDate(years_range$from, years_range$to)$
  filter(ee$Filter$calendarRange(season_months_range$from, season_months_range$to, "month"))$
  map(mask_L8_sr)

# příprava vrstvy s počtem snímků použitých na jeden pixel pro následné odmaskování (odstranění) pixelů s příliš nízkou hodnotou (threshold_px_count) snímků, které se na něm podílely
# zde na B1, nemělo by záležet o který band jde (raději ověřit?), i odmračnění probíhá hromadně skrz všechny bandy, počet použitých snímků pixelů bude u všech bandů stejný
l8_sr_collection_px_count <- l8_sr_collection$select("B1")$count()$rename("px_count")$gte(threshold_px_count)
file_name <- paste0(export_path, "/l8_", tag_name, "_", "px_count")
export_gee_image(l8_sr_collection_px_count, bb_geometry_rectangle, scale, file_name, output_raster_ext, "px_count")

# medián pro výslednou hodnotu pixelu - export všech bandů

# https://landsat.gsfc.nasa.gov/data/how-to-use-landsat-data/
# B11 - nedoporučeno používat
# B10 jen s atm. korekcemi z atmcorr.gsfc.nasa.gov - jsou součástí dopočtených GEE L8 _SR datasetů?!
# u B10 by navíc bylo vhodné odfiltrovat pryč vodní toky a plochy, pokud se na nich něco nemůže vyskytovat (tváří se jako lesy a jiná chladná místa)
bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10")

for (band in bands_all) {
  print(band)
  # medián pro výslednou hodnotu pixelu a aplikace vrstvy na odmaskování pixelů s nízkým podílem snímků
  l8_sr_collection_reduce_1_band <- l8_sr_collection$select(band)$median()$updateMask(l8_sr_collection_px_count)$unmask(no_data_value) # -3.4e+38

  file_name <- paste0(export_path, "/l8_", tag_name, "_", band)
  export_gee_image(l8_sr_collection_reduce_1_band, bb_geometry_rectangle, scale, file_name, output_raster_ext, band)

}



################################################################
# L8 _SR 'LANDSAT/LC08/C01/T1_SR' - NDVI
################################################################

bands_all <- c("B5", "B4")

# výpočet + odmaskování pixelů s nízkým podílem snímků
ndvi <- l8_sr_collection$select(bands_all)$median()$normalizedDifference(bands_all)$rename("NDVI")$select("NDVI")$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

file_name <- paste0(export_path, "/l8_", tag_name, "_", "NDVI")
export_gee_image(ndvi, bb_geometry_rectangle, scale, file_name, output_raster_ext, "NDVI")


################################################################
# Worldclim/Bioclim 'WORLDCLIM/V1/BIO'
################################################################

wc <- ee$Image(gdl$worldclim$geeSnippet)

bands_all <- wc$bandNames()$getInfo()

for (band in bands_all) {
  print(band)
  wc_1_band <- wc$select(band)

  file_name <- paste0(export_path, "/wc_", tag_name, "_", band)
  export_gee_image(wc_1_band, bb_geometry_rectangle, scale, file_name, output_raster_ext, band)

}




################################################################
# SRTM 'USGS/SRTMGL1_003'
################################################################

srtm <- ee$Image(gdl$srtm$geeSnippet)$select("elevation")

# elevation
file_name <- paste0(export_path, "/srtm_", tag_name, "_", "elevation")
export_gee_image(srtm, bb_geometry_rectangle, scale, file_name, output_raster_ext, "elevation")

# slope
slope <- ee$Terrain$slope(srtm)
file_name <- paste0(export_path, "/srtm_", tag_name, "_", "slope")
export_gee_image(slope, bb_geometry_rectangle, scale, file_name, output_raster_ext, "slope")

# aspect (ve stupních)
aspect <- ee$Terrain$aspect(srtm) # $divide(180)$multiply(pi)$sin() # převod na radiány
file_name <- paste0(export_path, "/srtm_", tag_name, "_", "aspect")
export_gee_image(aspect, bb_geometry_rectangle, scale, file_name, output_raster_ext, "aspect")



################################################################
# corine 'COPERNICUS/CORINE/V20/100m/2018'
################################################################

# corine <- ee$Image(gdl$corine$geeSnippet)$select(c("landcover"))

# export do meziproduktu v podobě tiffu s jedním bandem
# result_raster <- ee_as_raster(
#   image = corine, #$reproject("EPSG:32633")
#   region = bb_geometry_rectangle,
#   scale = scale,
#   via = "getInfo", # na Ubuntu nebylo nutné vůbec uvádět tento parametr, na Win10 si to jinak vynucovalo přihlášení a následné ukládání na Google disk
#   # maxPixels = 1e10
#   dsn = paste0(export_path, "/corine_", tag_name, ".tif") # Output filename. If missing, will create a temporary file.
# )

# writeRaster(result_raster[["elevation"]], paste0(export_path, "/srtm_", tag_name, ".asc"), 'ascii', overwrite = TRUE)




# pro zjištění vygenerovaného názvu tiff-u v /temp
# str(result_raster)

# přístup přes: result_raster$B1 (RasterLayer)
# result_raster$B1[2,3] # hodnota pixelu B1 bandu na 2. řádku a 3. sloupci
# result_raster$B1[1:3,1:2] # hodnota pixelu B1 bandu na 1-3. řádku a 1-2. sloupci


# NDVI z kolekce
# ndvi <- l8_sr_collection_reduce$normalizedDifference(c("B5", "B4"))

# export jednoho bandu
# l8_sr_collection_reduce_B2 <- l8_sr_collection_reduce$select("B2")



if (vis_map) {
  bands_vis <- c("B4", "B3", "B2")
  l8_sr_collection_reduce <- l8_sr_collection$select(bands_vis)$median() #$reproject("EPSG:32633")

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
