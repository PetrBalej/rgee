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
    c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "readxl", "abind", "stringr")
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



#########
# spojení obou "polovin" ČR a vepsání vypočtených variant hodnot pixelů do rasterů
#########

####### nové načítání L8 a WC
kfme16.N <- readRDS(paste0(path.igaD, "clean/predictors/kfme16-N_czechia_wc_l8_2018-2021_4-6.rds"))
kfme16.S <- readRDS(paste0(path.igaD, "clean/predictors/kfme16-S_czechia_wc_l8_2018-2021_4-6.rds"))

# sloučit N a S
for (l1 in names(kfme16.N)) {
    kfme16.N[[l1]] %<>% add_row(kfme16.S[[l1]])
}

kfme16 <- kfme16.N

sf.grid <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326_czechia.shp"))


# vzorový KFME raster pro 16 subkvadrátů
r <- raster(
    xmn = 12.0834157383896326, # set minimum x coordinate
    xmx = 18.8750299183168408, # set maximum x coordinate
    ymn = 48.5500817521775048, # set minimum y coordinate
    ymx = 51.0750755551042701, # set maximum y coordinate
    res = c((1 / 6) / 4, 0.1 / 4), # resolution in c(x,y) direction
    vals = runif(16463, 0, 1)
)
# writeRaster(r, paste0(path.igaD, "kfme16-test.tif"))




for (l1 in names(kfme16)) {
    kfme16.t <- as_tibble(kfme16[[l1]])

    print(l1)

    names(kfme16[[l1]])[-1]

    for (band in names(kfme16[[l1]])[-1]) {
        print(band)
        kfme16.t.select <- kfme16.t %>% dplyr::select(all_of(c("POLE", band)))

        m <- merge(sf.grid, kfme16.t.select, by = "POLE")


        rr <- rasterize(m, r, field = band)
        crs(rr) <- 4326

        writeRaster(rr, paste0(path.igaD, "clean/predictors/kfme16-", l1, "-", band, ".tif"), format = "GTiff", overwrite = TRUE)

        # dodělávka CV
        if (grepl("stdev", l1, fixed = TRUE)) {
            l1.mean <- gsub("stdev", "mean", l1)
            print(l1.mean)
            kfme16.t.mean <- as_tibble(kfme16[[l1.mean]])
            kfme16.t.mean.select <- kfme16.t.mean %>% dplyr::select(all_of(c("POLE", band)))
            m.mean <- merge(sf.grid, kfme16.t.mean.select, by = "POLE")

            rr.mean <- rasterize(m.mean, r, field = band)
            crs(rr.mean) <- 4326


            writeRaster(rr / rr.mean, paste0(path.igaD, "clean/predictors/kfme16-", gsub("stdev", "cv", l1), "-", band, ".tif"), format = "GTiff", overwrite = TRUE)
        }
    }
}

### ### ### ### ### ### ### ### ###
# ### VIFy (bio, (mean, cv))
### ### ### ### ### ### ### ### ###

library(usdmPB) # /mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/usdmPB
raster_stack <-
    rasters_dir_stack(
        paste0(
            "/home/petr/Documents/igaD/clean/predictors/"
        ),
        "tif"
    )
# 165

raster_stack <- dropLayer(raster_stack, grep("srtm|clc|count|bio03|bio09|b10|stdev", names(raster_stack), ignore.case = TRUE)) # 100

names(raster_stack) <- str_replace_all(names(raster_stack), c("kfme16." = "", "6_mean.bio" = "mean.bio", "6_cv.bio" = "cv.bio", "_30" = "", "\\.nd" = "", "\\.constant" = "", "\\." = "_"))

# ### uložení
# rr <- writeRaster(raster_stack, paste0(path.igaD, "clean/predictors/kfme16-100.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(raster_stack, file = paste0(path.igaD, "clean/predictors/kfme16-100.rds"))





# ### jen vizualizace všech rasterů do PDF
# rStandardize <- function(x){
#     # https://github.com/danlwarren/ENMTools/blob/master/R/raster.standardize.R
#    return(x/cellStats(x, stat=sum))
# }
# pdf(paste0(path.igaD, "bioclim-landsat-100-standardized.pdf"), width = 12, height = 9)
# for (l in sort(names(raster_stack))) {
# # plot(normalize(raster_stack[[l]]), main=l)
# plot(rStandardize(raster_stack[[l]]), main=l)
# # plot(raster_stack[[l]], axes=FALSE, frame.plot=FALSE, legend=FALSE)
# }
# dev.off()




predictor.groups <- c("wc_mean", "wc_cv", "l8.*_mean", "l8.*_cv")
raster_stack_groups <- list()
for (group in predictor.groups) {
    group.name <- str_replace(group, "\\.\\*", "")
    print(group.name)
    layer.indexes <- grep(group, names(raster_stack), ignore.case = TRUE)

    raster_stack_groups[[group.name]] <- raster_stack[[layer.indexes]]
    rr <- writeRaster(raster_stack_groups[[group.name]], paste0(path.igaD, "clean/predictors/kfme16-100---", group.name, ".grd"), format = "raster", overwrite = TRUE)
    hdr(rr, format = "ENVI")
    saveRDS(raster_stack_groups[[group.name]], file = paste0(path.igaD, "clean/predictors/kfme16-100---", group.name, ".rds"))
}


# VIFy - 1st round (4 skupiny)

raster_stack_groups_vif <- list()
for (group in names(raster_stack_groups)) {
    print("***********************************************")
    print(group)

    # vifcor
    vif.vifcor <- usdmPB::vifcor(raster_stack_groups[[group]], th = 0.7, maxobservations = 10000)
    saveRDS(vif.vifcor, file = paste0(path.igaD, "clean/vif/vifcor-1stLevel---", group, ".rds"))

    # vifstep
    vif.vifstep <- usdmPB::vifstep(raster_stack_groups[[group]][[vif.vifcor@results$Variables]], th = 5, maxobservations = 10000)

    vifcor07_vifstep5 <- raster_stack_groups[[group]][[vif.vifstep@results$Variables]]

    rr <- writeRaster(vifcor07_vifstep5, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, ".grd"), format = "raster", overwrite = TRUE)
    hdr(rr, format = "ENVI")
    saveRDS(vifcor07_vifstep5, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, ".rds"))
    raster_stack_groups_vif[[group]] <- vifcor07_vifstep5
}



# VIFy - 2nd round (2 skupiny bioXl8)
# reálně nic nezredukuje, zůstane stejný počet prediktorů
raster_stack_groups2 <- list()
raster_stack_groups2$wc <- stack(raster_stack_groups_vif[["wc_mean"]], raster_stack_groups_vif[["wc_cv"]])
raster_stack_groups2$l8 <- stack(raster_stack_groups_vif[["l8_mean"]], raster_stack_groups_vif[["l8_cv"]])

raster_stack_groups2_vif <- list()
for (group in names(raster_stack_groups2)) {
    print("***********************************************")
    print(group)

    # vifcor
    vif.vifcor <- usdmPB::vifcor(raster_stack_groups2[[group]], th = 0.7, maxobservations = 10000)
    saveRDS(vif.vifcor, file = paste0(path.igaD, "clean/vif/vifcor-2ndLevel---", group, ".rds"))

    # vifstep
    vif.vifstep <- usdmPB::vifstep(raster_stack_groups2[[group]][[vif.vifcor@results$Variables]], th = 5, maxobservations = 10000)

    vifcor07_vifstep5 <- raster_stack_groups2[[group]][[vif.vifstep@results$Variables]]

    rr <- writeRaster(vifcor07_vifstep5, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, ".grd"), format = "raster", overwrite = TRUE)
    hdr(rr, format = "ENVI")
    saveRDS(vifcor07_vifstep5, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, ".rds"))
    raster_stack_groups2_vif[[group]] <- vifcor07_vifstep5
}






# VIFy - 3rd round (all)

raster_stack_groups3 <- stack(raster_stack_groups2$wc, raster_stack_groups2$l8)
group <- "all"

# vifcor
vif.vifcor <- usdmPB::vifcor(raster_stack_groups3, th = 0.7, maxobservations = 10000)
saveRDS(vif.vifcor, file = paste0(path.igaD, "clean/vif/vifcor-3rdLevel---", group, ".rds"))

# vifstep
vif.vifstep <- usdmPB::vifstep(raster_stack_groups3[[vif.vifcor@results$Variables]], th = 5, maxobservations = 10000)

vifcor07_vifstep5 <- raster_stack_groups3[[vif.vifstep@results$Variables]]

rr <- writeRaster(vifcor07_vifstep5, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, ".grd"), format = "raster", overwrite = TRUE)
hdr(rr, format = "ENVI")
saveRDS(vifcor07_vifstep5, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, ".rds"))
raster_stack_groups3_vif <- vifcor07_vifstep5




# můžu počítat s finálními 21, cv na závěr spolklo sama sebe... (proč ne už v prvním kole je asi jen náhoda?)
# > `vifcor-3rdLevel---all`@excludedPairs
# $l8_5_ndvi_cv
# [1] "l8_5_evi_cv"
# $l8_4_ndvi_cv
# [1] "l8_4_raw_cv_B5"



# příprava NDOP
res.ndop <- ndop_ugc(list(from = "2018-01-01", to = "2021-12-31"), list(from = 4, to = 6), "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/zal", 5514, 1000)
res.ndop.ptaci <- res.ndop %>% filter(cat == "Ptáci") # 447052 nálezů
res.ndop.ptaci$species %<>% as.character
res.ndop.ptaci$species %<>% as.factor
res.ndop.ptaci %<>% dplyr::select(-cat)
res.ndop.ptaci %<>% st_as_sf(coords = c("X", "Y"), crs = 5514)

sf.grid <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326_czechia.shp")) # 9845
sf.grid.czechia <- st_union(sf.grid)
# st_write(sf.grid.czechia, paste0(path.igaD,"clean/border/czechia-borderGrid-4326.shp"))
# saveRDS(sf.grid.czechia, paste0(path.igaD,"clean/border/czechia-borderGrid-4326.rds"))
sf.grid.czechia5514 <- st_transform(sf.grid.czechia, 5514)
# st_write(sf.grid.czechia5514, paste0(path.igaD,"clean/border/czechia-borderGrid-5514.shp"))
# saveRDS(sf.grid.czechia5514, paste0(path.igaD,"clean/border/czechia-borderGrid-5514.rds"))

res.ndop.ptaci.czechia <- st_intersection(res.ndop.ptaci, sf.grid.czechia5514) # 446661 NDOP nálezů jen v ČR
# saveRDS(res.ndop.ptaci.czechia, paste0(path.igaD,"clean/occurrences/NDOP-5514_2018-2021_4-6_lt1000.rds"))
# st_write(res.ndop.ptaci.czechia, paste0(path.igaD,"clean/occurrences/NDOP-5514_2018-2021_4-6_lt1000.shp"))
# saveRDS(synonyms_unite(res.ndop.ptaci.czechia), paste0(path.igaD,"clean/occurrences/NDOP-5514_2018-2021_4-6_lt1000_synonymUnite.rds"))
# st_write(synonyms_unite(res.ndop.ptaci.czechia), paste0(path.igaD,"clean/occurrences/NDOP-5514_2018-2021_4-6_lt1000_synonymUnite.shp"))

res.ndop.ptaci.czechia4326 <- st_transform(res.ndop.ptaci.czechia, 4326)

#  saveRDS(res.ndop.ptaci.czechia4326, paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000.rds"))
#  st_write(res.ndop.ptaci.czechia4326, paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000.shp"))
#  saveRDS(synonyms_unite(res.ndop.ptaci.czechia4326), paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000_synonymUnite.rds"))
#  st_write(synonyms_unite(res.ndop.ptaci.czechia4326), paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000_synonymUnite.shp"))

# unikátní centroid kvadrátu pro každý výskyt druhu
sf.grid.ndop <- st_intersection(sf.grid, res.ndop.ptaci.czechia4326)
sf.grid.ndop.groups <- sf.grid.ndop %>%
    group_by(species, POLE) %>%
    dplyr::select(species, POLE) # 136427 obsazených kvadrátů různými druhy
sf.grid.ndop.groups.distinct <- sf.grid.ndop.groups %>% distinct(across(c(species, POLE)), .keep_all = TRUE)
sf.grid.ndop.groups.distinct.df <- as.data.frame(sf.grid.ndop.groups.distinct) %>% dplyr::select(species, POLE)
sf.grid.ndop.sq <- merge(sf.grid, sf.grid.ndop.groups.distinct.df, by = "POLE")
sf.grid.ndop.sq.centroid <- st_centroid(sf.grid.ndop.sq)

# saveRDS(sf.grid.ndop.sq.centroid, paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000_kfmeUniqueCentroids.rds"))
# st_write(sf.grid.ndop.sq.centroid, paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000_kfmeUniqueCentroids.shp"))
# saveRDS(synonyms_unite(sf.grid.ndop.sq.centroid), paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000_kfmeUniqueCentroids_synonymUnite.rds"))
# st_write(synonyms_unite(sf.grid.ndop.sq.centroid), paste0(path.igaD,"clean/occurrences/NDOP-4326_2018-2021_4-6_lt1000_kfmeUniqueCentroids_synonymUnite.shp"))

# length(unique(sf.grid.ndop.sq.centroid$POLE)) # 7818 z 9845 pokrývá NDOP (~80%); 160 LSD (<2%)


# LSD (raději bez hodinovek, ať to nekomplikuju)
res.lsd <- readRDS(paste0(path.igaD, "export-avif-lsd-2018-2022_utf8_3-6.rds"))
res.lsd %<>% filter(Year %in% 2018:2021) %>%
    filter(Month %in% 4:6) %>%
    filter(!str_detect(species, "/|sp\\.")) %>%
    dplyr::select(POLE, species, ObsListsID) # 131918 kvadrátů
res.lsd <- st_transform(res.lsd, 4326)
res.lsd <- distinct(res.lsd) # trvá dlouho, ale nutné udělat, nekteré druhy zaznamenány vícekrát na jedné návčtěve, 45264 kvadrátů
###### není distinct!!!!!!!!!!!!!!!!!!!!!!!!!!! musím dát res.lsd distinct(across(c(species, POLE)), .keep_all = TRUE)


# 164 lokalit, alespoň 4 návštěvy
ObsListsIDperSq <- res.lsd %>%
    group_by(POLE) %>%
    summarise(count = n_distinct(ObsListsID)) %>%
    arrange(desc(count)) %>%
    filter(count >= 4) %>% # 4 je hranice 1/2 kvartilu / summary(ObsListsIDperSq)
    ungroup()

POLE.selected <- as.vector(ObsListsIDperSq$POLE)

# st_write(ObsListsIDperSq, paste0(path.igaD, "clean/occurrences/ObsListsIDperSq-4326.shp"))
# saveRDS(ObsListsIDperSq, paste0(path.igaD, "clean/occurrences/ObsListsIDperSq-4326.rds"))

# dofiltr jen kvadráty s alespoň 4 návštěvami
res.lsd %<>% filter(POLE %in% POLE.selected) # 44839 kvadrátů
res.lsd$species %<>% as.character # factor dělá problémy v synonyms_unite()

# st_write(res.lsd, paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6.shp"))
# saveRDS(res.lsd, paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6.rds"))
# st_write(synonyms_unite(res.lsd), paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6_synonymUnite.shp"))
# saveRDS(synonyms_unite(res.lsd), paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6_synonymUnite.rds"))



#### znovu distinct až tady!!! 
res.lsd.distinct <- res.lsd %>% distinct(across(c(species, POLE)), .keep_all = TRUE) # 9307 kvadrátů
# st_write(res.lsd.distinct, paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6_distinct.shp"))
# saveRDS(res.lsd.distinct, paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6_distinct.rds"))
# st_write(synonyms_unite(res.lsd.distinct), paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6_synonymUnite_distinct.shp"))
# saveRDS(synonyms_unite(res.lsd.distinct), paste0(path.igaD,"clean/occurrences/LSD-164-4326_2018-2021_4-6_synonymUnite_distinct.rds"))


#
#### kontrola na NA napříč
#
# + propíše NA hodnoty napříč layery
raster_stack_groups3_vif_na <- raster::mask(raster_stack_groups3_vif, sum(raster_stack_groups3_vif))
#  znovu určí minMax hodnoty
raster_stack_groups3_vif_na <- raster::setMinMax(raster_stack_groups3_vif_na)

# rr <- writeRaster(raster_stack_groups3_vif_na, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, "-na.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(raster_stack_groups3_vif_na, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, "-na.rds"))


#################################################################################################################
# konečný dataset 21 rasterů s propsanými NA -- nutno ještě dodatečně vyřezat LSD pro NDOP (odstranit) a pro predikci (získat jen 162)!!!
# vif/vifcor07_vifstep5---all-na.grd
# vif/vifcor07_vifstep5---all-na.rds

# konečné datasety LSD+NDOP
# occurrences/f.*
# _noLSD - odstraněno LSD

#################################################################################################################



#
##### nutná zpětná kontrola NDOP+LSD jestli leží v NA rasteru
##### 2 LSD v Praze spadnou do NA... Nutno odstranit (jen 162)
#
rCheck <- raster_stack_groups3_vif_na[[1]]


#
# LSD
#
ObsListsIDperSqNA <- which(is.na(extract(rCheck, as_Spatial(ObsListsIDperSq))))
f.ObsListsIDperSq <- ObsListsIDperSq[-ObsListsIDperSqNA, ]
# st_write(f.ObsListsIDperSq, paste0(path.igaD,"clean/occurrences/f.ObsListsIDperSq.shp"))
# saveRDS(f.ObsListsIDperSq, paste0(path.igaD,"clean/occurrences/f.ObsListsIDperSq.rds"))


res.lsdNA <- which(is.na(extract(rCheck, as_Spatial(res.lsd))))
f.res.lsd <- res.lsd[-res.lsdNA, ] #  44839 na 44568
# st_write(f.res.lsd, paste0(path.igaD,"clean/occurrences/f.res.lsd.shp"))
# saveRDS(f.res.lsd, paste0(path.igaD,"clean/occurrences/f.res.lsd.rds"))
# st_write(synonyms_unite(f.res.lsd), paste0(path.igaD,"clean/occurrences/f.res.lsd_syn.shp"))
# saveRDS(synonyms_unite(f.res.lsd), paste0(path.igaD,"clean/occurrences/f.res.lsd_syn.rds"))

res.lsd.distinctNA <- which(is.na(extract(rCheck, as_Spatial(res.lsd.distinct))))
f.res.lsd.distinct <- res.lsd.distinct[-res.lsd.distinctNA, ] #  9307 na 9227
# st_write(f.res.lsd.distinct, paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.shp"))
# saveRDS(f.res.lsd.distinct, paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.rds"))
# st_write(synonyms_unite(f.res.lsd.distinct), paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct_syn.shp"))
# saveRDS(synonyms_unite(f.res.lsd.distinct), paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct_syn.rds"))


POLE.selected <- as.vector(f.ObsListsIDperSq$POLE)

# odvodit pro LSD absence
"%notin%" <- Negate("%in%")

f.res.lsd.distinct.temp <- f.res.lsd.distinct
f.res.lsd.distinct.temp$presence <- 1
f.res.lsd.distinct.temp %<>% dplyr::select(POLE, species, presence)
f.ObsListsIDperSq.temp <- f.ObsListsIDperSq %>% dplyr::select(POLE)


for (sp in as.vector(unique(f.res.lsd.distinct.temp$species))) {
    sp.presences <- f.res.lsd.distinct.temp %>% filter(species == sp)
    sp.absences <- f.ObsListsIDperSq.temp %>% filter(POLE %notin% sp.presences$POLE)

    sp.absences$species <- sp
    sp.absences$presence <- 0

    f.res.lsd.distinct.temp %<>% add_row(sp.absences)
}

f.res.lsd.distinct.pa <- f.res.lsd.distinct.temp # 9227 na 32076

# st_write(f.res.lsd.distinct.pa, paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.pa.shp"))
# saveRDS(f.res.lsd.distinct.pa, paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.pa.rds"))
# st_write(synonyms_unite(f.res.lsd.distinct.pa), paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.pa_syn.shp"))
# saveRDS(synonyms_unite(f.res.lsd.distinct.pa), paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.pa_syn.rds"))


#
# NDOP
#

sf.grid.ndop.sq.centroidNA <- which(is.na(extract(rCheck, as_Spatial(sf.grid.ndop.sq.centroid))))
f.sf.grid.ndop.sq.centroid <- sf.grid.ndop.sq.centroid[-sf.grid.ndop.sq.centroidNA, ] #  136427 na 135868
# st_write(f.sf.grid.ndop.sq.centroid, paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid.shp"))
# saveRDS(f.sf.grid.ndop.sq.centroid, paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid.rds"))
# st_write(synonyms_unite(f.sf.grid.ndop.sq.centroid), paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_syn.shp"))
# saveRDS(synonyms_unite(f.sf.grid.ndop.sq.centroid), paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_syn.rds"))




res.ndop.ptaci.czechia4326NA <- which(is.na(extract(rCheck, as_Spatial(res.ndop.ptaci.czechia4326))))
f.res.ndop.ptaci.czechia4326 <- res.ndop.ptaci.czechia4326[-res.ndop.ptaci.czechia4326NA, ] #  446661 na 442018
# st_write(f.res.ndop.ptaci.czechia4326, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326.shp"))
# saveRDS(f.res.ndop.ptaci.czechia4326, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326.rds"))
# st_write(synonyms_unite(f.res.ndop.ptaci.czechia4326), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_syn.shp"))
# saveRDS(synonyms_unite(f.res.ndop.ptaci.czechia4326), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_syn.rds"))




# použito předchozí NA, protože nemůžu vylučovat rasterem v jiném CRS
f.res.ndop.ptaci.czechia <- res.ndop.ptaci.czechia[-res.ndop.ptaci.czechia4326NA, ] #  446661 na 442018
# st_write(f.res.ndop.ptaci.czechia, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia.shp"))
# saveRDS(f.res.ndop.ptaci.czechia, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia.rds"))
# st_write(synonyms_unite(f.res.ndop.ptaci.czechia), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia_syn.shp"))
# saveRDS(synonyms_unite(f.res.ndop.ptaci.czechia), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia_syn.rds"))



# nutno ještě dodatečně vyřezat LSD pro NDOP (odstranit) a pro predikci (získat jen 162)!!!

# toto asi reálně netřeba? jen pro bias raster jeden layer jako podklad? Ale preventivně můžu používat k extrakci NDOP a generování a extrakci backgroundu
raster_stack_groups3_vif_na_noLSD <- raster::mask(raster_stack_groups3_vif_na, f.ObsListsIDperSq, inverse = TRUE)
raster_stack_groups3_vif_na_noLSD <- raster::setMinMax(raster_stack_groups3_vif_na_noLSD)
# rr <- writeRaster(raster_stack_groups3_vif_na_noLSD, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, "-na_noLSD.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(raster_stack_groups3_vif_na_noLSD, paste0(path.igaD, "clean/vif/vifcor07_vifstep5---", group, "-na_noLSD.rds"))



#
# NDOP2 - odstranění kvadrátů z LSD (úplná nezávislost)
#
rCheck2 <- raster_stack_groups3_vif_na_noLSD[[1]]


sf.grid.ndop.sq.centroidNA <- which(is.na(extract(rCheck2, as_Spatial(sf.grid.ndop.sq.centroid))))
f.sf.grid.ndop.sq.centroid_noLSD <- sf.grid.ndop.sq.centroid[-sf.grid.ndop.sq.centroidNA, ] #  136427 na 135868 na 130654
# st_write(f.sf.grid.ndop.sq.centroid_noLSD, paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_noLSD.shp"))
# saveRDS(f.sf.grid.ndop.sq.centroid_noLSD, paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_noLSD.rds"))
# st_write(synonyms_unite(f.sf.grid.ndop.sq.centroid_noLSD), paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_syn_noLSD.shp"))
# saveRDS(synonyms_unite(f.sf.grid.ndop.sq.centroid_noLSD), paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_syn_noLSD.rds"))


res.ndop.ptaci.czechia4326NA <- which(is.na(extract(rCheck2, as_Spatial(res.ndop.ptaci.czechia4326))))
f.res.ndop.ptaci.czechia4326_noLSD <- res.ndop.ptaci.czechia4326[-res.ndop.ptaci.czechia4326NA, ] #  446661 na 442018 na 412824
# st_write(f.res.ndop.ptaci.czechia4326_noLSD, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_noLSD.shp"))
# saveRDS(f.res.ndop.ptaci.czechia4326_noLSD, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_noLSD.rds"))
# st_write(synonyms_unite(f.res.ndop.ptaci.czechia4326_noLSD), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_syn_noLSD.shp"))
# saveRDS(synonyms_unite(f.res.ndop.ptaci.czechia4326_noLSD), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_syn_noLSD.rds"))


# použito předchozí NA, protože nemůžu vylučovat rasterem v jiném CRS
f.res.ndop.ptaci.czechia_noLSD <- res.ndop.ptaci.czechia[-res.ndop.ptaci.czechia4326NA, ] #  446661 na 442018 na 412824
# st_write(f.res.ndop.ptaci.czechia_noLSD, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia_noLSD.shp"))
# saveRDS(f.res.ndop.ptaci.czechia_noLSD, paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia_noLSD.rds"))
# st_write(synonyms_unite(f.res.ndop.ptaci.czechia_noLSD), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia_syn_noLSD.shp"))
# saveRDS(synonyms_unite(f.res.ndop.ptaci.czechia_noLSD), paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia_syn_noLSD.rds"))





# # doplnění prázdného prediktoru pro maxent s jedním prediktorem
# if (length(names(env)) == 1) {
#     # https://github.com/danlwarren/ENMTools/blob/master/R/enmtools.maxent.R
#     oldname <- names(env)
#     env <- stack(env, env)
#     env[[2]][!is.na(env[[2]])] <- 0
#     names(env) <- c(oldname, "dummyvar")
# }
# # occ$lon + occ$lat
# bg <- randomPoints(predictors, 1000)
# me <- dismo::maxent(predictors, occtrain, a=bg, removeDuplicates = FALSE, args = c("hinge=false", "threshold=false"))
# me.r <- dismo::predict(me, predictors, args = c("outputformat=cloglog"))

#
# bias rastery
#
library(spatstat)

# res.ndop.ptaci.czechia4326
# rCheck
# rCheck2

# ppp
ow <- as.owin(as.im(rCheck2)) # rCheck + dále níže
ndop_coords <- st_coordinates(res.ndop.ptaci.czechia4326)
ndop_ppp <- ppp(ndop_coords[, 1], ndop_coords[, 2], window = ow)

# ndop_ppp.bw <- bw.scott.iso(ndop_ppp) # 0.09940521 ~ 7.10 km
# sigma = 0.001 se téměř rovná použití per_pixel, nemusím ho používat...

sigmas <- c(0.001, seq(0.01, 0.2, by = 0.01))

for (sigma in sigmas) {
    ndop_ppp.d <- density.ppp(ndop_ppp, sigma = sigma, positive = TRUE)
    ndop_ppp.r <- normalize(raster(ndop_ppp.d, crs = crs(rCheck2)))

    # rr <- writeRaster(ndop_ppp.r, paste0(path.igaD, "clean/bias/ppp_na_noLSD-", sigma * 1000, ".tif"), format = "GTiff", overwrite = TRUE)
    # hdr(rr, format = "ENVI")
    # saveRDS(ndop_ppp.r, paste0(path.igaD, "clean/bias/ppp_na_noLSD-", sigma * 1000, ".rds"))
}

#
# occ/per_pixel + normalizace k vytvoření jednoduchého bias rasteru
#

p.temp <- as_Spatial(res.ndop.ptaci.czechia4326 %>% dplyr::select(!everything()))

ndop_per_pixel <- rasterize(p.temp, rCheck, update = TRUE, fun = "count")


# rr <- writeRaster(ndop_per_pixel, paste0(path.igaD, "clean/bias/ndop_per_pixel.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(ndop_per_pixel, file = paste0(path.igaD, "clean/bias/ndop_per_pixel.rds"))
# rr <- writeRaster(normalize(ndop_per_pixel), paste0(path.igaD, "clean/bias/ndop_per_pixel_normalized.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(normalize(ndop_per_pixel), file = paste0(path.igaD, "clean/bias/ndop_per_pixel_normalized.rds"))

# # NA
# ndop_per_pixel_na <- raster::mask(ndop_per_pixel, rCheck)
# ndop_per_pixel_na <- raster::setMinMax(ndop_per_pixel_na)
# rr <- writeRaster(ndop_per_pixel_na, paste0(path.igaD, "clean/bias/ndop_per_pixel_na.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(ndop_per_pixel_na, file = paste0(path.igaD, "clean/bias/ndop_per_pixel_na.rds"))
# rr <- writeRaster(normalize(ndop_per_pixel_na), paste0(path.igaD, "clean/bias/ndop_per_pixel_normalized_na.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(normalize(ndop_per_pixel_na), file = paste0(path.igaD, "clean/bias/ndop_per_pixel_normalized_na.rds"))

# # noLSD
# ndop_per_pixel_na_noLSD <- raster::mask(ndop_per_pixel, rCheck2)
# ndop_per_pixel_na_noLSD <- raster::setMinMax(ndop_per_pixel_na_noLSD)
# rr <- writeRaster(ndop_per_pixel_na_noLSD, paste0(path.igaD, "clean/bias/ndop_per_pixel_na_noLSD.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(ndop_per_pixel_na_noLSD, file = paste0(path.igaD, "clean/bias/ndop_per_pixel_na_noLSD.rds"))
# rr <- writeRaster(normalize(ndop_per_pixel_na_noLSD), paste0(path.igaD, "clean/bias/ndop_per_pixel_normalized_na_noLSD.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(normalize(ndop_per_pixel_na_noLSD), file = paste0(path.igaD, "clean/bias/ndop_per_pixel_normalized_na_noLSD.rds"))


# "wc_cv_bio11" má nodata v Praze
# raster_stack_groups3_vif[["wc_cv_bio11"]]@file@nodatavalue
# NAvalue(raster_stack_groups3_vif[["wc_cv_bio11"]])


# vyřezat z NDOP LSD? Až po TGB? Zkreslím tím TGB... Ne, pokud TGB bez, tak i train bez!



# jsou v literatuře známé velikosti home range (okrsky) které každý druh využívá? Jde odněkud zjistit? V nových traits? Dát do vazby s TGB adjustem a pokud je tam jasný vztah, tak je vyhráno!
# pravděpodobnější vztah velikosti home range, než prevalence (tu ale také otestovat)

# sdm - možnost predikce jen do LSD míst (ne do celé ČR)? Rychlejší
# jsou v maxentu přidat váhy (presencím nebo random bg?)???



#
# generování backgroundu
#

# biasRasters <-
#     list.files(
#         path = paste0(path.igaD, "clean/bias"),
#         pattern = paste0("^ppp_na_noLSD.+\\.tif$"),
#         ignore.case = TRUE,
#         full.names = TRUE
#     )
# for (biasRaster in biasRasters) {
#     br <- raster(biasRaster)
#     bgBias[[]] <- generate_bg(br, 5000, prob = TRUE)
# }

bgBias <- list()

for (r in 1:10) {
    r <- as.character(r)
    for (sigma in sigmas) {
        sigma <- as.character(sigma * 1000)
        br.na <- raster(paste0(path.igaD, "clean/bias/ppp_na-", sigma, ".tif"))
        br.na_noLSD <- raster(paste0(path.igaD, "clean/bias/ppp_na_noLSD-", sigma, ".tif"))

        bgBias[["bias_na"]][[as.character(5000)]][[r]][[sigma]] <- generate_bg(br.na, 5000, prob = TRUE)
        bgBias[["bias_na_noLSD"]][[as.character(5000)]][[r]][[sigma]] <- generate_bg(br.na_noLSD, 5000, prob = TRUE)

        bgBias[["bias_na"]][[as.character(10000)]][[r]][[sigma]] <- generate_bg(br.na, 10000, prob = TRUE)
        bgBias[["bias_na_noLSD"]][[as.character(10000)]][[r]][[sigma]] <- generate_bg(br.na_noLSD, 10000, prob = TRUE)
    }
    bgBias[["random_na"]][[as.character(5000)]][[r]] <- generate_bg(br.na, 5000, prob = FALSE)
    bgBias[["random_na_noLSD"]][[as.character(5000)]][[r]] <- generate_bg(br.na_noLSD, 5000, prob = FALSE)

    bgBias[["random_na"]][[as.character(10000)]][[r]] <- generate_bg(br.na, 10000, prob = FALSE)
    bgBias[["random_na_noLSD"]][[as.character(10000)]][[r]] <- generate_bg(br.na_noLSD, 10000, prob = FALSE)
}

# saveRDS(bgBias, file = paste0(path.igaD, "clean/bias/bg-variants.rds"))

# # plot(st_as_sf(bgBias[["random_na_noLSD"]][[as.character(5000)]][[r]], coords = c("x", "y"), crs = crs(br.na_noLSD)))



# předem nasamplovat a uložit pro všechny typy backgroundů pro rastery prediktorů (ať se zbytečně pak neopakuje při modelování)
# stejně tak bych mohl nasamplovat i všechny druhy (nálezy) pro presence NDOP (k nim pak napárovávat bg výše - to až při modelování?) i PA bodů LSD
# ověřit formát pro sdm a možnost vložení parametrů pro sdm::maxent včetně zpracování 1 prediktoru

# plot(st_as_sf(bgBias[["bias_na_noLSD"]][[as.character(10000)]][["1"]][["1"]], coords = c("x", "y"), crs = crs(br.na_noLSD)))

# sdm "test.indep"



#
# nasamplovat bg
#

bgBiasSampled <- list()
for (r in 1:10) {
    r <- as.character(r)
    print(r)
    for (sigma in sigmas) {
        print(sigma)
        sigma <- as.character(sigma * 1000)
        bgBiasSampled[["bias_na"]][[as.character(5000)]][[r]][[sigma]] <- extract(raster_stack_groups3_vif_na, as_Spatial(st_as_sf(bgBias[["bias_na"]][[as.character(5000)]][[r]][[sigma]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na))))
        bgBiasSampled[["bias_na_noLSD"]][[as.character(5000)]][[r]][[sigma]] <- extract(raster_stack_groups3_vif_na_noLSD, as_Spatial(st_as_sf(bgBias[["bias_na_noLSD"]][[as.character(5000)]][[r]][[sigma]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na_noLSD))))

        bgBiasSampled[["bias_na"]][[as.character(10000)]][[r]][[sigma]] <- extract(raster_stack_groups3_vif_na, as_Spatial(st_as_sf(bgBias[["bias_na"]][[as.character(10000)]][[r]][[sigma]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na))))
        bgBiasSampled[["bias_na_noLSD"]][[as.character(10000)]][[r]][[sigma]] <- extract(raster_stack_groups3_vif_na_noLSD, as_Spatial(st_as_sf(bgBias[["bias_na_noLSD"]][[as.character(10000)]][[r]][[sigma]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na_noLSD))))
    }
    bgBiasSampled[["random_na"]][[as.character(5000)]][[r]] <- extract(raster_stack_groups3_vif_na, as_Spatial(st_as_sf(bgBias[["random_na"]][[as.character(5000)]][[r]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na))))
    bgBiasSampled[["random_na_noLSD"]][[as.character(5000)]][[r]] <- extract(raster_stack_groups3_vif_na_noLSD, as_Spatial(st_as_sf(bgBias[["random_na_noLSD"]][[as.character(5000)]][[r]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na_noLSD))))

    bgBiasSampled[["random_na"]][[as.character(10000)]][[r]] <- extract(raster_stack_groups3_vif_na, as_Spatial(st_as_sf(bgBias[["random_na"]][[as.character(10000)]][[r]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na))))
    bgBiasSampled[["random_na_noLSD"]][[as.character(10000)]][[r]] <- extract(raster_stack_groups3_vif_na_noLSD, as_Spatial(st_as_sf(bgBias[["random_na_noLSD"]][[as.character(10000)]][[r]], coords = c("x", "y"), crs = crs(raster_stack_groups3_vif_na_noLSD))))
}
# saveRDS(bgBiasSampled, file = paste0(path.igaD, "clean/bias/bgSampled-variants.rds"))



#
# nasamplovat LSD
#

f.res.lsd.distinct.pa.sampled <- extract(raster_stack_groups3_vif_na, as_Spatial(f.res.lsd.distinct.pa))
f.res.lsd.distinct.pa.sampled.df <- cbind(f.res.lsd.distinct.pa, f.res.lsd.distinct.pa.sampled)
# saveRDS(f.res.lsd.distinct.pa.sampled.df, file = paste0(path.igaD, "clean/occurrences/lsdSampled.rds"))
# saveRDS(synonyms_unite(f.res.lsd.distinct.pa.sampled.df), file = paste0(path.igaD, "clean/occurrences/lsdSampled_syn.rds"))



#
# nasamplovat NDOP
#
# 130654 NDOP vs.  32076 LSD (PA) respektive 9227 (P); 14x více presencí v NDOP

f.sf.grid.ndop.sq.centroid_noLSD.sampled <- extract(raster_stack_groups3_vif_na_noLSD, as_Spatial(f.sf.grid.ndop.sq.centroid_noLSD))
f.sf.grid.ndop.sq.centroid_noLSD.sampled.df <- cbind(f.sf.grid.ndop.sq.centroid_noLSD, f.sf.grid.ndop.sq.centroid_noLSD.sampled)
# saveRDS(f.sf.grid.ndop.sq.centroid_noLSD.sampled.df, file = paste0(path.igaD, "clean/occurrences/ndopSampled.rds"))
# saveRDS(synonyms_unite(f.sf.grid.ndop.sq.centroid_noLSD.sampled.df), file = paste0(path.igaD, "clean/occurrences/ndopSampled_syn.rds"))




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# paste0(path.igaD, "clean/bias/bgSampled-variants.rds")
# paste0(path.igaD, "clean/occurrences/ndopSampled_syn.rds")
# paste0(path.igaD, "clean/occurrences/lsdSampled_syn.rds")
# 
# paste0(path.igaD,"clean/occurrences/f.res.lsd.distinct.pa_syn.rds")
# 
# paste0(path.igaD,"clean/occurrences/f.sf.grid.ndop.sq.centroid_syn_noLSD.rds")
# paste0(path.igaD,"clean/occurrences/f.res.ndop.ptaci.czechia4326_syn_noLSD.rds")
# 
# paste0(path.igaD, "clean/vif/vifcor07_vifstep5---all-na.rds")
# paste0(path.igaD, "clean/vif/vifcor07_vifstep5---all-na_noLSD.rds")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



