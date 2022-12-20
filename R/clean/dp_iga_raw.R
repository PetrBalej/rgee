# předem nainstalováno...
library(rgee)

# nastavit working directory
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

path.igaD <- "/home/petr/Documents/igaD/"

# závisí na některých funkcích z:
source(paste0(getwd(), "/R/export_raster/functions.R"))

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "readxl", "abind", "stringr")
# "googledrive" # kontrola při použití v ee_Initialize?
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

ee_Initialize(email = "balej.petr@gmail.com", gcs = TRUE)

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
res_proj_epsg <- 4326

## výsledná velikost pixelu v m
scale <- 30

## jednotná "značka" přidaná ke všem output rasterům z jednoho běhu skriptu (stejné nastavení parametrů) a
tag_name <- scale # "" # gsub('[^0-9-]', '-', Sys.time())

# adresář pro exportované soubory (v rámci wd) + další tag_name
export_path <- paste0(path.igaD, "predsDP/")
export_fileName <- "kfme0rad-czechia_wc_l8_2014-2017_4-8"


# GIT project directory (kompletní repozitář rgee z github.com: po rozbalení zipu v rgee-master/rgee-master)
git_project_path <- getwd()


## výběr regionu

SitMap.selected <- unname(unlist(read_csv(paste0(wd, "/R/clean/SitMap_0Rad-selected.csv"))))
SitMap <- st_transform(st_read(paste0(path.igaD, "sitmap_0rad/sitmap_0rad.shp")), crs = 4326) %>% dplyr::select(POLE)
SitMap.POLE.selected <- SitMap %>% dplyr::filter(POLE %in% SitMap.selected)

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
bb <- cesko
# bb <- paste0(git_project_path, "/shp/ne_50m_admin_0_countries/czechia/cz_4326.shp")

# bb <- list(
#   xmin = 12.7,
#   xmax = 13.2,
#   ymin = 47.1,
#   ymax = 47.5
# )
## časové rozsahy

# 2014 – 2017 pro každý měsíc 4, 5, 6, 7, 8.

# rozsah snímků od/do
years_range <- list(from = "2014-01-01", to = "2017-12-31")

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše), možno i více sezón
season_months_range <- list(c(4, 4), c(5, 5), c(6, 6), c(7, 7), c(8, 8))

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
threshold_px_count <- 1

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


POLE <- SitMap.POLE.selected %>% sf_as_ee()
envelope <- st_as_sfc(st_bbox(SitMap.POLE.selected)) %>% sf_as_ee()

start_time <- Sys.time()

for (season in season_months_range) {


    ################################################################
    # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - raw bandy
    ################################################################

    # aplikace základních geografických, časových a odmračňovacích/odstiňovacích/aerosol/radio filtrů
    l8_sr_collection <-
        ee$ImageCollection(gdl$landsat2$geeSnippet)$
            # filterBounds(SitMap.POLE.selected %>% sf_as_ee())$ # nevhodné dělat předem?
            filterDate(years_range$from, years_range$to)$
            filter(
            ee$Filter$calendarRange(season[1], season[2], "month")
        )$map(mask_L8_sr2_qa)$map(mask_L8_sr2_radsat)$map(mask_L8_sr2_aerosol)$map(applyScaleFactors)


    bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7")
    bn <- l8_sr_collection$first()$bandNames()$getInfo()
    bn.renamed <- gsub("SR_|ST_", "", bn)

    l8_sr_collection <- l8_sr_collection$select(bn, bn.renamed)

    #####
    # raw
    #####
    task <- ee_image_to_gcs(
        image = l8_sr_collection$select(bands_all)$mean()$clip(POLE),
        region = envelope, # bb_geometry_rectangle,
        scale = scale,
        description = paste0("czechia-0rad-l8_", scale, "_", season[1], "_raw_mean"),
        bucket = "kfme33",
        maxPixels = 1e10,
        timePrefix = FALSE
    )

    task$start()
    ee_monitoring(task) # optional
}
# Map$addLayer()

end_time <- Sys.time()

# časové rozmezí a celkový čas generování
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)

stop()
#
# spojení tiles, při 30m to GEE pro ČR rozdělí do více tiffů...
#
library(terra)
tiles <- list.files("/mnt/6C2B3F36770071FA/presuny-u20/igaD0/30m/0rad/mean/", "tiff$", full.names = TRUE)
tiles.c <- sprc(lapply(tiles, rast))
r <- mosaic(tiles.c)
rr <- writeRaster(r, "/mnt/6C2B3F36770071FA/presuny-u20/igaD0/30m/0rad_mean.grd", format = "raster", overwrite = TRUE)
hdr(rr, format = "ENVI")