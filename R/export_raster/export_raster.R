start_time <- Sys.time()

# předem nainstalováno...
library(rgee)

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("sp",
    "rgdal",
    "mapview",
    "raster",
    "geojsonio",
    "stars",
    "httpuv")
# "googledrive" # kontrola při použití v ee_Initialize?
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

# použít Google Drive?
use_google_drive <- TRUE

ee_Initialize(email = "balej.petr@gmail.com", drive = use_google_drive)

# přetypovat výsledné rastery za účelem optimalizace velikosti? (udělá INT16 i z NDVI po vynásobení 10000, v ostatních rasterech jsou desetinná čísla zbytečná)
retype <- TRUE

# ee_user_info()

# při odpojení nebo zneplatnění původního přihlašovacího tokenu
# ee_clean_credentials()

# tempdir() # dočasný adresář pro aktuální R session


# # # # # # # # # # # # # # # # # # # # # #
# nastavení základních parametrů [start]  #
# # # # # # # # # # # # # # # # # # # # # #

## projekce výsledných rastrů, (většinou) není možné použít WGS84 (4326) - nesouhlasí pak rozlišení (odlišný počet rows/cols) pokud se používají různé zdroje (L8, WorldClim, SRTM, ...)
# 3035: ETRS89-LAEA	- Lambertovo azimutální stejnoploché zobrazení
# 32633: UTM zone 33N - použito Mercatorovo válcové konformní zobrazení (UTM zobrazení), základní poledník 15°
res_proj_epsg <- 3035

## výsledná velikost pixelu v m
scale <- 100

## jednotná "značka" přidaná ke všem output rasterům z jednoho běhu skriptu (stejné nastavení parametrů) a
tag_name <- scale # "" # gsub('[^0-9-]', '-', Sys.time())

# adresář pro exportované soubory (v rámci wd) + další tag_name
export_path <-
  paste0(getwd(), "/../export/raster/schuzka_rastery/", scale)


# GIT project directory (kompletní repozitář rgee z github.com: po rozbalení zipu v rgee-master/rgee-master)
git_project_path <- getwd()


## výběr regionu

# definice obálek (bounding box) různě velkých území pro testování
sz_cechy <- list(
  xmin = 13.0,
  xmax = 13.5,
  ymin = 50.0,
  ymax = 50.5
)
cesko <- list(
  xmin = 12.0,
  xmax = 19.0,
  ymin = 48.5,
  ymax = 51.2
)
str_evropa <-
  list(
    xmin = 8.5,
    xmax = 22.0,
    ymin = 46.0,
    ymax = 53.5
  )
str_evropa2 <-
  list(
    xmin = 8.6,
    xmax = 21.9,
    ymin = 46.4,
    ymax = 53.1
  )

# výběr konkrétního území
bb <- sz_cechy
# bb <- paste0(git_project_path, "/shp/ne_50m_admin_0_countries/czechia/cz_4326.shp")


## časové rozsahy

# rozsah snímků od/do
years_range <- list(from = '2016-01-01', to = '2020-12-31')

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
season_months_range <- list(from = 4, to = 6)


## výstupní fotmát exportovaných rasterů
# pokud zadám koncovku, budou rastry uloženy jako fyzické soubory na disk
# pokud NEzadám koncovku, tvoří se jen RasterLayer-y které se následně vloží do RasterStack-u do proměnné raster_stack
output_raster_ext <- "" # asc, tif, grd, envi, img

## zobrazit mapové okno s polygonem oblasti a RGB kompozitem?
vis_map <- FALSE

## NoDataValue
no_data_value <- -9999 # vede k -3.4e+38

## minimum sat. snímků k použití pixelu pro analýzu, jinak no_data_value
# nechávat raději 0 a až dodatečně použít příslušný vygenerovaný raster k domaskování? Jinak tím poznamenám všechny uložené rastery (dořešit)
threshold_px_count <- 3

# # # # # # # # # # # # # # # # # # # # # #
# nastavení základních parametrů [konec]  #
# # # # # # # # # # # # # # # # # # # # # #

raster_stack_list <- list()
file_name_list <- list()

# vytvoří adresář pro export, pokud neexistuje
dir.create(export_path, showWarnings = FALSE)

# parametry použitých datasetů z GEE - export z gee_datasets/gee-pouzite-datasety.xlsx
gee_datasets_path_csv <-
  paste0(git_project_path, "/gee_datasets/gee-pouzite-datasety.csv")

# načtení potřebných funkcí
source(paste0(git_project_path, "/R/export_raster/functions.R"))

# načtení csv s datasety z GEE
gdl <- gee_datasets_list(gee_datasets_path_csv)

if (!is.character(bb)) {
  xmin <- bb$xmin
  xmax <- bb$xmax
  ymin <- bb$ymin
  ymax <- bb$ymax
  
  bb_geometry <- NULL
  bb_geometry_rectangle <- ee$Geometry$Rectangle(
    coords = c(xmin, ymin, xmax, ymax),
    proj = "EPSG:4326",
    geodesic = FALSE
  )
  
} else {
  if (file.exists(bb)) {
    bb_geometry_ee <- st_read(bb) %>% sf_as_ee()
    bb_geometry <- bb_geometry_ee$geometry() #[[1]]$getInfo()
    bb_geometry_rectangle <- bb_geometry_ee$geometry()$bounds()
  } else {
    stop(paste0("Shapefile ", bb, " not exist!"))
  }
  
}


################################################################
# L8 _SR 'LANDSAT/LC08/C01/T1_SR' - raw bandy
################################################################

# aplikace základních geogracických, časových a odmračňovacích/odstiňovacích filtrů
l8_sr_collection <-
  ee$ImageCollection(gdl$landsat$geeSnippet)$filterBounds(bb_geometry_rectangle)$filterDate(years_range$from, years_range$to)$filter(
    ee$Filter$calendarRange(season_months_range$from, season_months_range$to, "month")
  )$map(mask_L8_sr)

# příprava vrstvy s počtem snímků použitých na jeden pixel pro následné odmaskování (odstranění) pixelů s příliš nízkou hodnotou (threshold_px_count) snímků, které se na něm podílely
# zde na B1, nemělo by záležet o který band jde (raději ověřit?), i odmračnění probíhá hromadně skrz všechny bandy, počet použitých snímků pixelů bude u všech bandů stejný
band <- "px_count"
l8_sr_collection_px_count <-
  l8_sr_collection$select("B1")$count()$rename(band)$gte(threshold_px_count)
file_name <- paste0(export_path, "/l8_", tag_name, "_", band)
file_name_list <- append(file_name_list, c(file_name))
raster_stack_list[[band]] <-
  export_gee_image(
    l8_sr_collection_px_count,
    bb_geometry_rectangle,
    scale,
    file_name,
    output_raster_ext,
    band,
    NULL,
    NULL,
    res_proj_epsg,
    use_google_drive,
    retype
  )

# výchozí extent a resolution převezmu z L8
default_extent <- extent(raster_stack_list[["px_count"]])
default_res <- res(raster_stack_list[["px_count"]])

# medián pro výslednou hodnotu pixelu - export všech bandů

# https://landsat.gsfc.nasa.gov/data/how-to-use-landsat-data/
# B11 - nedoporučeno používat
# B10 jen s atm. korekcemi z atmcorr.gsfc.nasa.gov - jsou součástí dopočtených GEE L8 _SR datasetů?!
# u B10 by navíc bylo vhodné odfiltrovat pryč vodní toky a plochy, pokud se na nich něco nemůže vyskytovat (tváří se jako lesy a jiná chladná místa)

bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10")
# bands_all <- c("B1", "B2")

for (band in bands_all) {
  print(band)
  # medián pro výslednou hodnotu pixelu a aplikace vrstvy na odmaskování pixelů s nízkým podílem snímků
  l8_sr_collection_reduce_1_band <-
    l8_sr_collection$select(band)$median()$updateMask(l8_sr_collection_px_count)$unmask(no_data_value) # -3.4e+38
  
  file_name <- paste0(export_path, "/l8_", tag_name, "_", band)
  file_name_list <- append(file_name_list, c(file_name))
  raster_stack_list[[band]] <-
    export_gee_image(
      l8_sr_collection_reduce_1_band,
      bb_geometry_rectangle,
      scale,
      file_name,
      output_raster_ext,
      band,
      default_extent,
      default_res,
      res_proj_epsg,
      use_google_drive,
      retype
    )
  
}


################################################################
# L8 _SR 'LANDSAT/LC08/C01/T1_SR' - NDVI
################################################################

bands_all <- c("B5", "B4")

band <- "NDVI"

# výpočet + odmaskování pixelů s nízkým podílem snímků
ndvi <-
  l8_sr_collection$select(bands_all)$median()$normalizedDifference(bands_all)$rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

file_name <- paste0(export_path, "/l8_", tag_name, "_", band)
file_name_list <- append(file_name_list, c(file_name))
raster_stack_list[[band]] <-
  export_gee_image(
    ndvi,
    bb_geometry_rectangle,
    scale,
    file_name,
    output_raster_ext,
    band,
    default_extent,
    default_res,
    res_proj_epsg,
    use_google_drive,
    retype
  )


################################################################
# Worldclim/Bioclim 'WORLDCLIM/V1/BIO'
################################################################

wc <- ee$Image(gdl$worldclim$geeSnippet)

bands_all <- wc$bandNames()$getInfo()
# bands_all <- c("bio01", "bio02")

for (band in bands_all) {
  print(band)
  wc_1_band <- wc$select(band)
  
  file_name <- paste0(export_path, "/wc_", tag_name, "_", band)
  file_name_list <- append(file_name_list, c(file_name))
  raster_stack_list[[band]] <-
    export_gee_image(
      wc_1_band,
      bb_geometry_rectangle,
      scale,
      file_name,
      output_raster_ext,
      band,
      default_extent,
      default_res,
      res_proj_epsg,
      use_google_drive,
      retype
    )
  
}




################################################################
# SRTM 'USGS/SRTMGL1_003'
################################################################

# elevation
band <- "elevation"
srtm <- ee$Image(gdl$srtm$geeSnippet)$select(band)

file_name <- paste0(export_path, "/srtm_", tag_name, "_", band)
file_name_list <- append(file_name_list, c(file_name))
raster_stack_list[[band]] <-
  export_gee_image(
    srtm,
    bb_geometry_rectangle,
    scale,
    file_name,
    output_raster_ext,
    band,
    default_extent,
    default_res,
    res_proj_epsg,
    use_google_drive,
    retype
  )

# slope
band <- "slope"
slope <- ee$Terrain$slope(srtm)
file_name <- paste0(export_path, "/srtm_", tag_name, "_", band)
file_name_list <- append(file_name_list, c(file_name))
raster_stack_list[[band]] <-
  export_gee_image(
    slope,
    bb_geometry_rectangle,
    scale,
    file_name,
    output_raster_ext,
    band,
    default_extent,
    default_res,
    res_proj_epsg,
    use_google_drive,
    retype
  )

# aspect (ve stupních)
band <- "aspect"
aspect <-
  ee$Terrain$aspect(srtm) # $divide(180)$multiply(pi)$sin() # převod na radiány
file_name <- paste0(export_path, "/srtm_", tag_name, "_", band)
file_name_list <- append(file_name_list, c(file_name))
raster_stack_list[[band]] <-
  export_gee_image(
    aspect,
    bb_geometry_rectangle,
    scale,
    file_name,
    output_raster_ext,
    band,
    default_extent,
    default_res,
    res_proj_epsg,
    use_google_drive,
    retype
  )



################################################################
# corine 'COPERNICUS/CORINE/V20/100m/2018'
################################################################

band <- "landcover"
corine <- ee$Image(gdl$corine$geeSnippet)$select(c(band))

file_name <- paste0(export_path, "/clc_", tag_name, "_", band)
file_name_list <- append(file_name_list, c(file_name))

raster_stack_list[[band]] <-
  export_gee_image(
    corine,
    bb_geometry_rectangle,
    scale,
    file_name,
    output_raster_ext,
    band,
    default_extent,
    default_res,
    res_proj_epsg,
    use_google_drive,
    retype
  )




# # pro zjištění vygenerovaného názvu tiff-u v /temp
# str(result_raster)

# # přístup přes: result_raster$B1 (RasterLayer)
# result_raster$B1[2,3] # hodnota pixelu B1 bandu na 2. řádku a 3. sloupci
# result_raster$B1[1:3,1:2] # hodnota pixelu B1 bandu na 1-3. řádku a 1-2. sloupci


# # NDVI z kolekce
# ndvi <- l8_sr_collection_reduce$normalizedDifference(c("B5", "B4"))

# # export jednoho bandu
# l8_sr_collection_reduce_B2 <- l8_sr_collection_reduce$select("B2")



if (vis_map) {
  bands_vis <- c("B4", "B3", "B2")
  l8_sr_collection_reduce <-
    l8_sr_collection$select(bands_vis)$median() #$reproject("EPSG:32633")
  
  # vizualizace v mapovém okně
  visparams <- list(
    bands = bands_vis,
    min = 0,
    max = 3000,
    gamma = 1.4
  )
  
  # Map$setCenter(13.0, 50.0, 10)
  Map$centerObject(bb_geometry_rectangle, zoom = 9)
  l1 <-
    Map$addLayer(
      bb_geometry_rectangle,
      visParams = list(color = "FF0000"),
      opacity = 0.3,
      name = "vybraná obálka (bounding box)"
    )
  l2 <-
    Map$addLayer(l8_sr_collection_reduce, visparams, name = "LANDSAT/LC08/C01/T1_SR filtered median")
  l2 + l1
}


# uložení všech RasterLayer-ů do RasterStack
raster_stack <- stack(raster_stack_list)


# names(raster_stack)
# nlayers(raster_stack)

# # výběr smysluplných bandů do VIFu pro předvýběr do SDM
# raster_stack_selected <- dropLayer(raster_stack, c(1,31,32))

# #  export RasterStack-u do fyzického multiband souboru na disk
# file_name <- paste0(export_path, "/multiband_", tag_name, ".tif")
# writeRaster(raster_stack_selected, file_name, format = "GTiff", overwrite = TRUE)


end_time <- Sys.time()

# časové rozmezí a celkový čas generování
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)


# zápis a uložení protokolu
df <- "%Y-%m-%d %H:%M:%S"
text <- c(
  toString(strptime(start_time, format = df)),
  toString(strptime(end_time, format = df)),
  toString(end_time - start_time),
  export_path,
  git_project_path,
  deparse(bb),
  deparse(years_range),
  deparse(season_months_range),
  scale,
  output_raster_ext,
  vis_map,
  tag_name,
  no_data_value,
  threshold_px_count,
  deparse(names(raster_stack_list)),
  deparse(file_name_list)
)
file_name <- paste0(export_path, "/protocol_", tag_name, ".txt")
writeLines(text, file_name)

# library(rgdal)
# library(raster)
# currentEnv=getData("worldclim", var="bio", res=5) # stáhne 35 MB .zip
# str(currentEnv)
# # Formal class 'RasterStack' [package "raster"] with 11 slots - totožné jako result_raster - použitelné do dismo(maxent) jako vstup
