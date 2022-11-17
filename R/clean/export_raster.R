start_time <- Sys.time()

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
    c(
        "sp",
        "rgdal",
        "mapview",
        "raster",
        "geojsonio",
        "stars",
        "httpuv"
    )
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
res_proj_epsg <- 4326

## výsledná velikost pixelu v m
scale <- 30

## jednotná "značka" přidaná ke všem output rasterům z jednoho běhu skriptu (stejné nastavení parametrů) a
tag_name <- scale # "" # gsub('[^0-9-]', '-', Sys.time())

# adresář pro exportované soubory (v rámci wd) + další tag_name
export_path <-
    paste0(getwd(), "/../export/test-reduceResolution/", scale)


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
bb <- cesko
# bb <- paste0(git_project_path, "/shp/ne_50m_admin_0_countries/czechia/cz_4326.shp")

# bb <- list(
#   xmin = 12.7,
#   xmax = 13.2,
#   ymin = 47.1,
#   ymax = 47.5
# )
## časové rozsahy

# rozsah snímků od/do
years_range <- list(from = "2018-01-01", to = "2021-12-31")

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše), možno i více sezón
season_months_range <- list(c(4, 4), c(5, 5), c(6, 6))

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
        bb_geometry <- bb_geometry_ee$geometry() # [[1]]$getInfo()
        bb_geometry_rectangle <- bb_geometry_ee$geometry()$bounds()
    } else {
        stop(paste0("Shapefile ", bb, " not exist!"))
    }
}


sitmap_2rad <- list()
# Define a geometry
sitmap_2rad$N <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326_czechia_N.shp"))
sitmap_2rad$S <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326_czechia_S.shp"))
output_df <- list()

for (half in names(sitmap_2rad)) {
    for (season in season_months_range) {


        ################################################################
        # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - raw bandy
        ################################################################

        # aplikace základních geografických, časových a odmračňovacích/odstiňovacích/aerosol/radio filtrů
        l8_sr_collection <-
            ee$ImageCollection(gdl$landsat2$geeSnippet)$
                filterBounds(bb_geometry_rectangle)$
                filterDate(years_range$from, years_range$to)$
                filter(
                ee$Filter$calendarRange(season[1], season[2], "month")
            )$map(mask_L8_sr2_qa)$map(mask_L8_sr2_radsat)$map(mask_L8_sr2_aerosol)$map(applyScaleFactors)





        bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10")
        bn <- l8_sr_collection$first()$bandNames()$getInfo()
        bn.renamed <- gsub("SR_|ST_", "", bn)

        l8_sr_collection <- l8_sr_collection$select(bn, bn.renamed)

        #####
        # raw
        #####
        output_df[[paste0("l8_", scale, "_", season[1], "_raw_stdev")]] <- ee_extract(
            x = l8_sr_collection$select(bands_all)$median(),
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$stdDev(),
            sf = FALSE
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_raw_mean")]] <- ee_extract(
            x = l8_sr_collection$select(bands_all)$median(),
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$median(),
            sf = FALSE
        )

        #####
        # ndvi
        #####
        output_df[[paste0("l8_", scale, "_", season[1], "_ndvi_stdev")]] <- ee_extract(
            x = l8_sr_collection$select(c("B5", "B4"))$median()$normalizedDifference(c("B5", "B4")),
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$stdDev(),
            sf = FALSE
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_ndvi_mean")]] <- ee_extract(
            x = l8_sr_collection$select(c("B5", "B4"))$median()$normalizedDifference(c("B5", "B4")),
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$median(),
            sf = FALSE
        )


        #####
        # mndwi
        #####
        # https://www.linkedin.com/pulse/ndvi-ndbi-ndwi-calculation-using-landsat-7-8-tek-bahadur-kshetri
        output_df[[paste0("l8_", scale, "_", season[1], "_mndwi_stdev")]] <- ee_extract(
            x = l8_sr_collection$select(c("B3", "B6"))$median()$normalizedDifference(c("B3", "B6")),
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$stdDev(),
            sf = FALSE
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_mndwi_mean")]] <- ee_extract(
            x = l8_sr_collection$select(c("B3", "B6"))$median()$normalizedDifference(c("B3", "B6")),
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$median(),
            sf = FALSE
        )


        #####
        # evi
        #####
        #   # https://www.usgs.gov/core-science-systems/nli/landsat/landsat-enhanced-vegetation-index?qt-science_support_page_related_con=0
        #   # In Landsat 8, EVI = 2.5 * ((Band 5 – Band 4) / (Band 5 + 6 * Band 4 – 7.5 * Band 2 + 1)).

        evi_prep <- l8_sr_collection$select(c("B5", "B4", "B2"))$median()

        evi_res <- evi_prep$expression(
            "2.5 * ((B5 - B4) / (B5 + 6 * B4 - 7.5 * B2 + 1))",
            list(
                B5 = evi_prep$select("B5"),
                B4 = evi_prep$select("B4"),
                B2 = evi_prep$select("B2")
            )
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_evi_stdev")]] <- ee_extract(
            x = evi_res,
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$stdDev(),
            sf = FALSE
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_evi_mean")]] <- ee_extract(
            x = evi_res,
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$median(),
            sf = FALSE
        )

        #####
        # msavi
        #####
        #   # https://www.usgs.gov/core-science-systems/nli/landsat/landsat-modified-soil-adjusted-vegetation-index
        #   # In Landsat 8, MSAVI = (2 * Band 5 + 1 – sqrt ((2 * Band 5 + 1)2 – 8 * (Band 5 – Band 4))) / 2.

        msavi_prep <- l8_sr_collection$select(c("B5", "B4"))$median()
        msavi_res <- msavi_prep$expression(
            "(2 * B5 + 1 - sqrt(pow((2 * B5 + 1), 2) - 8 * (B5 - B4))) / 2",
            list(
                B5 = msavi_prep$select("B5"),
                B4 = msavi_prep$select("B4")
            )
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_msavi_stdev")]] <- ee_extract(
            x = msavi_res,
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$stdDev(),
            sf = FALSE
        )

        output_df[[paste0("l8_", scale, "_", season[1], "_msavi_mean")]] <- ee_extract(
            x = msavi_res,
            y = sitmap_2rad[[half]],
            scale = scale,
            fun = ee$Reducer$median(),
            sf = FALSE
        )
    }


    # ################################################################
    # # Worldclim/Bioclim 'WORLDCLIM/V1/BIO'
    # ################################################################

    wc <- ee$Image(gdl$worldclim$geeSnippet)

    bands_all <- wc$bandNames()$getInfo()
    # bands_all <- c("bio01", "bio02")



    # Extract values
    output_df[[paste0("wc_", scale, "_", season[1], "_stdev")]] <- ee_extract(
        x = wc,
        y = sitmap_2rad[[half]],
        scale = scale,
        fun = ee$Reducer$stdDev(),
        sf = FALSE
    )

    output_df[[paste0("wc_", scale, "_", season[1], "_mean")]] <- ee_extract(
        x = wc,
        y = sitmap_2rad[[half]],
        scale = scale,
        fun = ee$Reducer$median(),
        sf = FALSE
    )



    # saveRDS(output_df, file = paste0(path.igaD, "test-kfme16-N_czechia_wc_l8_2018-2021_4-6.rds"))
    saveRDS(output_df, file = paste0(path.igaD, "clean/predictors/kfme16-", half, "_czechia_wc_l8_2018-2021_4-6.rds"))
}

end_time <- Sys.time()

# časové rozmezí a celkový čas generování
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)



