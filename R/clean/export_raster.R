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


# VIFy (bio, (mean, cv))


# bias rastery - ppp bezrozměrné, přímo souřadnice? - kde mám poznámky na správný postup stackowerflow Australský profesor?


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


