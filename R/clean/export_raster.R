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
        # # Sentinel test

        #   s2_sr_collection <-
        #     ee$ImageCollection(gdl$sentinel$geeSnippet)$
        #       filterBounds(bb_geometry_rectangle)$
        #       filterDate(years_range$from, years_range$to)$
        #       filter(
        #       ee$Filter$calendarRange(2, 2, "month")
        #     )$ #$map(maskS2clouds)
        #     filter(ee$Filter$lt("CLOUD_COVERAGE_ASSESSMENT", 10))$
        # # filterMetadata("IMAGE_QUALITY", "equals", 9)$
        # # filter(ee$Filter$lt("CLOUDY_SHADOW_PERCENTAGE", 10))$
        # filter(ee$Filter$lt("DARK_FEATURES_PERCENTAGE", 50))

        #   bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11")
        #   for (band in bands_all) {
        #     print(band)
        #     # medián pro výslednou hodnotu pixelu a aplikace vrstvy na odmaskování pixelů s nízkým podílem snímků
        #     s2_sr_collection_reduce_1_band <-
        #       s2_sr_collection$select(band)$median() # -3.4e+38



        #     file_name <- paste0(export_path, "/s2_", 2, "-", 2, "_", tag_name, "_", band)
        #     file_name_list <- append(file_name_list, c(file_name))
        #     raster_stack_list[[band]] <-
        #       export_gee_image(
        #         s2_sr_collection_reduce_1_band,
        #         bb_geometry_rectangle,
        #         scale,
        #         file_name,
        #         output_raster_ext,
        #         band,
        #         default_extent,
        #         default_res,
        #         res_proj_epsg,
        #         use_google_drive,
        #         retype
        #       )
        #   }

        ################################################################
        # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - raw bandy
        ################################################################

        # aplikace základních geogracických, časových a odmračňovacích/odstiňovacích filtrů
        l8_sr_collection <-
            ee$ImageCollection(gdl$landsat$geeSnippet)$
                filterBounds(bb_geometry_rectangle)$
                filterDate(years_range$from, years_range$to)$
                filter(
                ee$Filter$calendarRange(season[1], season[2], "month")
            )$map(mask_L8_sr)

        # $reduce(ee$Reducer$stdDev())


        # # příprava vrstvy s počtem snímků použitých na jeden pixel pro následné odmaskování (odstranění) pixelů s příliš nízkou hodnotou (threshold_px_count) snímků, které se na něm podílely
        # # zde na B1, nemělo by záležet o který band jde (raději ověřit?), i odmračnění probíhá hromadně skrz všechny bandy, počet použitých snímků pixelů bude u všech bandů stejný
        # band <- "px_count"
        # l8_sr_collection_px_count <-
        #   l8_sr_collection$select("B1")$count()$rename(band)$gte(threshold_px_count)
        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     l8_sr_collection_px_count,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     NULL,
        #     NULL,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )

        # # výchozí extent a resolution převezmu z L8
        # default_extent <- extent(raster_stack_list[["px_count"]])
        # default_res <- res(raster_stack_list[["px_count"]])

        # medián pro výslednou hodnotu pixelu - export všech bandů

        # https://landsat.gsfc.nasa.gov/data/how-to-use-landsat-data/
        # B11 - nedoporučeno používat
        # B10 jen s atm. korekcemi z atmcorr.gsfc.nasa.gov - jsou součástí dopočtených GEE L8 _SR datasetů?!
        # u B10 by navíc bylo vhodné odfiltrovat pryč vodní toky a plochy, pokud se na nich něco nemůže vyskytovat (tváří se jako lesy a jiná chladná místa)

        # bands_all <- c("B1")
        # # bands_all <- c("B1", "B2")

        # for (band in bands_all) {
        #   print(band)
        #   # medián pro výslednou hodnotu pixelu a aplikace vrstvy na odmaskování pixelů s nízkým podílem snímků
        #   l8_sr_collection_reduce_1_band <-
        #     l8_sr_collection$select(band)$median()$updateMask(l8_sr_collection_px_count)$unmask(no_data_value) # -3.4e+38

        #   file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        #   file_name_list <- append(file_name_list, c(file_name))
        #   raster_stack_list[[band]] <-
        #     export_gee_image(
        #       l8_sr_collection_reduce_1_band,
        #       bb_geometry_rectangle,
        #       scale,
        #       file_name,
        #       output_raster_ext,
        #       band,
        #       default_extent,
        #       default_res,
        #       res_proj_epsg,
        #       use_google_drive,
        #       retype
        #     )
        # }


        bands_all <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10")
        # bands_all <- c("B2", "B5")
        # Extract values


        
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



        # t(apply(simplify2array(stdDev), 1:2, median))

        # stdDev$getInfo()
        # write.csv(stdDev, paste0(path.igaD, "stdev-test.csv"))
        # stop()

        # ################################################################
        # # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - NDVI
        # ################################################################

        # bands_all <- c("B5", "B4")

        # band <- "NDVI"
        # print(band)
        # # výpočet + odmaskování pixelů s nízkým podílem snímků
        # ndvi <-
        #   l8_sr_collection$select(bands_all)$median()$normalizedDifference(bands_all)$
        #     rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     ndvi,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     default_extent,
        #     default_res,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )



        # ################################################################
        # # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - NDWI/NDMI
        # # https://www.linkedin.com/pulse/ndvi-ndbi-ndwi-calculation-using-landsat-7-8-tek-bahadur-kshetri
        # ################################################################

        # bands_all <- c("B5", "B6")

        # band <- "NDWI"
        # print(band)
        # # výpočet + odmaskování pixelů s nízkým podílem snímků
        # ndwi <-
        #   l8_sr_collection$select(bands_all)$median()$normalizedDifference(bands_all)$
        #     rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     ndwi,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     default_extent,
        #     default_res,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )



        # ################################################################
        # # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - MNDWI
        # # https://www.linkedin.com/pulse/ndvi-ndbi-ndwi-calculation-using-landsat-7-8-tek-bahadur-kshetri
        # ################################################################

        # bands_all <- c("B3", "B6")

        # band <- "MNDWI"
        # print(band)
        # # výpočet + odmaskování pixelů s nízkým podílem snímků
        # mndwi <-
        #   l8_sr_collection$select(bands_all)$median()$normalizedDifference(bands_all)$
        #     rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     mndwi,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     default_extent,
        #     default_res,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )


        # ################################################################
        # # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - EVI
        # # https://www.usgs.gov/core-science-systems/nli/landsat/landsat-enhanced-vegetation-index?qt-science_support_page_related_con=0
        # # In Landsat 8, EVI = 2.5 * ((Band 5 – Band 4) / (Band 5 + 6 * Band 4 – 7.5 * Band 2 + 1)).
        # ################################################################

        # bands_all <- c("B5", "B4", "B2")
        # band <- "EVI"
        # print(band)

        # # výpočet + odmaskování pixelů s nízkým podílem snímků
        # evi_prep <- l8_sr_collection$select(bands_all)$median()
        # evi <- evi_prep$expression(
        #   "2.5 * ((B5 - B4) / (B5 + 6 * B4 - 7.5 * B2 + 1))",
        #   list(
        #     B5 = evi_prep$select("B5"),
        #     B4 = evi_prep$select("B4"),
        #     B2 = evi_prep$select("B2")
        #   )
        # )$rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     evi,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     default_extent,
        #     default_res,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )



        # ################################################################
        # # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - SAVI
        # # https://www.usgs.gov/core-science-systems/nli/landsat/landsat-soil-adjusted-vegetation-index
        # # In Landsat 8, SAVI = ((Band 5 – Band 4) / (Band 5 + Band 4 + 0.5)) * (1.5).
        # ################################################################

        # bands_all <- c("B5", "B4")
        # band <- "SAVI"
        # print(band)

        # # výpočet + odmaskování pixelů s nízkým podílem snímků
        # savi_prep <- l8_sr_collection$select(bands_all)$median()
        # savi <- savi_prep$expression(
        #   "((B5 - B4) / (B5 + B4 + 0.5)) * (1.5)",
        #   list(
        #     B5 = savi_prep$select("B5"),
        #     B4 = savi_prep$select("B4")
        #   )
        # )$rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     savi,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     default_extent,
        #     default_res,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )


        # ################################################################
        # # L8 _SR 'LANDSAT/LC08/C01/T1_SR' - MSAVI
        # # https://www.usgs.gov/core-science-systems/nli/landsat/landsat-modified-soil-adjusted-vegetation-index
        # # In Landsat 8, MSAVI = (2 * Band 5 + 1 – sqrt ((2 * Band 5 + 1)2 – 8 * (Band 5 – Band 4))) / 2.
        # ################################################################

        # bands_all <- c("B5", "B4")
        # band <- "MSAVI"
        # print(band)

        # # výpočet + odmaskování pixelů s nízkým podílem snímků
        # msavi_prep <- l8_sr_collection$select(bands_all)$median()
        # msavi <- msavi_prep$expression(
        #   "(2 * B5 + 1 - sqrt(pow((2 * B5 + 1), 2) - 8 * (B5 - B4))) / 2",
        #   list(
        #     B5 = msavi_prep$select("B5"),
        #     B4 = msavi_prep$select("B4")
        #   )
        # )$rename(band)$select(band)$updateMask(l8_sr_collection_px_count)$unmask(no_data_value)

        # file_name <- paste0(export_path, "/l8_", season[1], "-", season[2], "_", tag_name, "_", band)
        # file_name_list <- append(file_name_list, c(file_name))
        # raster_stack_list[[band]] <-
        #   export_gee_image(
        #     msavi,
        #     bb_geometry_rectangle,
        #     scale,
        #     file_name,
        #     output_raster_ext,
        #     band,
        #     default_extent,
        #     default_res,
        #     res_proj_epsg,
        #     use_google_drive,
        #     retype
        #   )
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
    saveRDS(output_df, file = paste0(path.igaD, "test2-kfme16_wc-", half, "_czechia_wc_l8_2018-2021_4-6.rds"))
}

end_time <- Sys.time()

# časové rozmezí a celkový čas generování
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)

# wc <- ee$Image(gdl$worldclim$geeSnippet)

# bands_all <- wc$bandNames()$getInfo()
# # bands_all <- c("bio01", "bio02")

# for (band in bands_all) {
#   print(band)
#   wc_1_band <- wc$select(band)

#   file_name <- paste0(export_path, "/wc_", tag_name, "_", band)
#   file_name_list <- append(file_name_list, c(file_name))
#   raster_stack_list[[band]] <-
#     export_gee_image(
#       wc_1_band,
#       bb_geometry_rectangle,
#       scale,
#       file_name,
#       output_raster_ext,
#       band,
#       default_extent,
#       default_res,
#       res_proj_epsg,
#       use_google_drive,
#       retype
#     )
# }




# ################################################################
# # SRTM 'USGS/SRTMGL1_003'
# ################################################################

# # elevation
# band <- "elevation"
# print(band)
# srtm <- ee$Image(gdl$srtm$geeSnippet)$select(band)

# file_name <- paste0(export_path, "/srtm_", tag_name, "_", band)
# file_name_list <- append(file_name_list, c(file_name))
# raster_stack_list[[band]] <-
#   export_gee_image(
#     srtm,
#     bb_geometry_rectangle,
#     scale,
#     file_name,
#     output_raster_ext,
#     band,
#     default_extent,
#     default_res,
#     res_proj_epsg,
#     use_google_drive,
#     retype
#   )

# # slope
# band <- "slope"
# print(band)
# slope <- ee$Terrain$slope(srtm)
# file_name <- paste0(export_path, "/srtm_", tag_name, "_", band)
# file_name_list <- append(file_name_list, c(file_name))
# raster_stack_list[[band]] <-
#   export_gee_image(
#     slope,
#     bb_geometry_rectangle,
#     scale,
#     file_name,
#     output_raster_ext,
#     band,
#     default_extent,
#     default_res,
#     res_proj_epsg,
#     use_google_drive,
#     retype
#   )

# # aspect (ve stupních)
# band <- "aspect"
# print(band)
# aspect <-
#   ee$Terrain$aspect(srtm) # $divide(180)$multiply(pi)$sin() # převod na radiány
# file_name <- paste0(export_path, "/srtm_", tag_name, "_", band)
# file_name_list <- append(file_name_list, c(file_name))
# raster_stack_list[[band]] <-
#   export_gee_image(
#     aspect,
#     bb_geometry_rectangle,
#     scale,
#     file_name,
#     output_raster_ext,
#     band,
#     default_extent,
#     default_res,
#     res_proj_epsg,
#     use_google_drive,
#     retype
#   )



# ################################################################
# # corine 'COPERNICUS/CORINE/V20/100m/2018'
# ################################################################

# band <- "landcover"
# print(band)
# corine <- ee$Image(gdl$corine$geeSnippet)$select(c(band))

# file_name <- paste0(export_path, "/clc_", tag_name, "_", band)
# file_name_list <- append(file_name_list, c(file_name))

# raster_stack_list[[band]] <-
#   export_gee_image(
#     corine,
#     bb_geometry_rectangle,
#     scale,
#     file_name,
#     output_raster_ext,
#     band,
#     default_extent,
#     default_res,
#     res_proj_epsg,
#     use_google_drive,
#     retype
#   )




# # pro zjištění vygenerovaného názvu tiff-u v /temp
# str(result_raster)

# # přístup přes: result_raster$B1 (RasterLayer)
# result_raster$B1[2,3] # hodnota pixelu B1 bandu na 2. řádku a 3. sloupci
# result_raster$B1[1:3,1:2] # hodnota pixelu B1 bandu na 1-3. řádku a 1-2. sloupci


# # NDVI z kolekce
# ndvi <- l8_sr_collection_reduce$normalizedDifference(c("B5", "B4"))

# # export jednoho bandu
# l8_sr_collection_reduce_B2 <- l8_sr_collection_reduce$select("B2")


stop()
if (vis_map) {
  bands_vis <- c("B4", "B3", "B2")
  l8_sr_collection_reduce <-
    l8_sr_collection$select(bands_vis)$median() # $reproject("EPSG:32633")

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