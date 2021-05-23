synonyms <- list(
    "Spatula clypeata" = "Anas clypeata",
    "Phylloscopus sibilatrix" = "Phylloscopus sibillatrix",
    "Spatula querquedula" = "Anas querquedula",
    "Mareca penelope" = "Anas penelope",
    "Calidris pugnax" = "Philomachus pugnax",
    "Dryobates minor" = "Dendrocopos minor",
    # nové oproti traits
    "Acanthis cabaret" = "Acanthis flammea",
    "Mareca strepera" = "Anas strepera",
    "Clanga pomarina" = "Aquila pomarina",
    "Tetrastes bonasia" = "Bonasa bonasia",
    "Linaria cannabina" = "Carduelis cannabina",
    "Acanthis flammea" = "Carduelis flammea",
    "Dendrocoptes medius" = "Dendrocopos medius",
    "Dryobates minor" = "Dendrocopos minor",
    "Ardea alba" = "Egretta alba",
    "Ichthyaetus melanocephalus" = "Larus melanocephalus",
    "Poecile montanus" = "Parus montanus",
    "Saxicola rubicola" = "Saxicola torquata",
    "Lyrurus tetrix" = "Tetrao tetrix"
)

cmd_arg <- commandArgs(trailingOnly = TRUE)


options(scipen = 999) # výpis čísel v nezkrácené podobě
options(java.parameters = c("-Xmx20g"))
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "dismo", "rmaxent", "ENMToolsPB", "spatstat", "purrr", "abind") # "rmaxent", "blockCV", "ggplot2", "MASS", "data.table",
install.packages(setdiff(required_packages, rownames(installed.packages())))

# library(devtools)
# install_local("/home/petr/Documents/github/enmtools/ENMTools", force = TRUE, build=TRUE)
# library(devtools)
# install_github("danlwarren/ENMTools", force = TRUE)
# install_github("PetrBalej/ENMTools", force = TRUE, ref="pb")
# install_local("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/ENMToolsPB", force = TRUE, build=TRUE)

# nutná změna ve zdrojáku background.buffer.R   x <- circles(points, d=buffer.width, lonlat=FALSE) # původně TRUE

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")


wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))
source(paste0(getwd(), "/R/export_raster/ndop_top.R"))
source(paste0(getwd(), "/R/export_raster/sedi.R"))

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom"
# asi nebudu zapisovat do csv, rovnou z proměnných?  (potenciálně špatně přenositelné a vyhodnotitelné někým jiným...)
# filename_csv <- "gOverlap_3v_10000X.csv"
# limit_max_occurences_ndop_top <- 10000 # 70000; nepoužívané

rcrs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
##################################################################################### vv
#  Rscript "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/enmtools.R" 1
#  source("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/enmtools.R", encoding = "UTF-8")
if (is.na(cmd_arg[1])) {
    limit_min_occurences <- 100 # 100, 10000, 20000,30000
    limit_max_occurences <- 150

    print(str(cmd_arg[1]))
    print("nepředán žádný argument")
    cmd_arg_str <- 0
} else {
    print("předán argument")
    print(cmd_arg[1])
    cmd_arg_str <- cmd_arg[1]
    if (cmd_arg[1] == 1) {
        limit_min_occurences <- 100 # 100, 10000, 20000,30000
        limit_max_occurences <- 2000
    }
    if (cmd_arg[1] == 2) {
        limit_min_occurences <- 2001 # 100, 10000, 20000,30000
        limit_max_occurences <- 5800
    }
    if (cmd_arg[1] == 3) {
        limit_min_occurences <- 5801 # 100, 10000, 20000,30000
        limit_max_occurences <- 14000
    }
    if (cmd_arg[1] == 4) {
        limit_min_occurences <- 14001 # 100, 10000, 20000,30000
        limit_max_occurences <- 70000
    }
}


px_size <- c(10000) # 100, 500, 1000, 5000, 10000 # 10000, 5000, 1000, 500, 100
replicates <- 1
pres <- paste0("XXXXXX", px_size, cmd_arg_str) # předpona png obrázků s predikcí a dalších outputů
generate_bias_raster <- FALSE
trans_coords <- FALSE # když mám předem uložené přetransformované souřadnice, můžu dát FALSE, šetří to čas, musím mít ale vygenerovaný předem celý rozsah druhů (100-70000)
enmsr <- list()

# shapefiles
# BB (+ČR)
blocks <- st_read(paste0(wd, "/shp/blocks.shp"))
# BB - ČR
blocks_erased_cz <- st_read(paste0(wd, "/shp/blocks_erased_cz.shp"))
# ČR
czechia <- st_read(paste0(wd, "/shp/ne_10m_admin_0_countries/czechia/cz_3035.shp"))

# reálně se tím řídit nemusím, neodpovídá to reálnému počtu záznamů použitých do modelů, jen pomocná metrika?
# ndop_top <- ndop_top(paste0(getwd(), "/rgee/species/ndop/ndop-top-2021-03-21.xlsx"))
# species <- ndop_top %>%
#     dplyr::select(species1_, species2_, species1, species2, species1_short, species2_short, Nálezů) %>%
#     filter(Nálezů < limit_max_occurences_ndop_top)

# plot(enms$`10000`$Anthus_campestris[[2]]$p)




### nalezy_start###
# příprava nálezových dat (další krok)
# pokud nebudu i dynamicky měnit přesost nálezů podle pixelu, může zůstat zde
# je zde pro možnost navázání na pixel size, momentálně na pevno na 300m přesnost pro zjědnodušení
# XXX odkud jsem generoval???

if (!exists("plot_ndop_csv") & !exists("plot_gbif_csv")) {
    plot_ndop_csv <- read_csv(paste0(export_path, "/inputs/occurrences/ndop_300.csv"))
    plot_gbif_csv <- read_csv(paste0(export_path, "/inputs/occurrences/gbif_300.csv"))
}


# počet záznamů na druh - jen příprava bez filtrů
ptaci_ndop_distinct <- plot_ndop_csv %>%
    group_by(species) %>%
    summarise(count = n_distinct(key)) %>%
    arrange(desc(count))
ptaci_gbif_distinct <- plot_gbif_csv %>%
    group_by(species) %>%
    summarise(count = n_distinct(key)) %>%
    arrange(desc(count))

# zjištění synonym a přejmenování druhů v GBIF datech podle NDOPu, aby se umožnila následná společná filtrace pro modelování dle druhů
# 1) výběr druhů z NDOP které se nenavázaly na GBIF
df1x <- ptaci_ndop_distinct %>%
    anti_join(ptaci_gbif_distinct, by = c("species" = "species")) %>%
    filter(species %in% names(synonyms)) %>%
    mutate(syn = "XXX")
# 2) přidání synonym
df1x$syn <- sapply(df1x$species, function(x, synonyms) {
    synonyms[[as.character(x)]]
}, synonyms)
# 3) přejmenování případných druhů v GBIF, které se napárují se synonymy
plot_gbif_csv <- plot_gbif_csv %>%
    left_join(df1x, by = c("species" = "syn")) %>%
    mutate(species = ifelse(is.na(species.y), species, species.y)) %>%
    dplyr::select(key, species, latitude, longitude)



# počet záznamů na druh - pro limit nejnižšího (nejvyššího) počtu záznamů
ptaci_ndop_distinct <- plot_ndop_csv %>%
    group_by(species) %>%
    summarise(count = n_distinct(key)) %>%
    arrange(desc(count)) %>%
    filter(count >= limit_min_occurences) %>%
    filter(count < limit_max_occurences) %>%
    filter(!is.na(species))

if (!is.na(cmd_arg[1])) {
    # pokud jedu po skupinách, nemůžu dávat min limit u GBIF dat, nedošlo by k úplným průnikům s NDOP
    limit_min_occurences <- 100
}
ptaci_gbif_distinct <- plot_gbif_csv %>%
    group_by(species) %>%
    summarise(count = n_distinct(key)) %>%
    arrange(desc(count)) %>%
    filter(count >= limit_min_occurences) %>%
    # filter(count <= limit_max_occurences) %>%
    filter(!is.na(species))


if (trans_coords == TRUE) {
    # vybrané druhy dle limitů početnosti
    plot_ndop_csv_100 <- plot_ndop_csv %>% filter(species %in% ptaci_ndop_distinct$species)
    plot_gbif_csv_100 <- plot_gbif_csv %>% filter(species %in% ptaci_gbif_distinct$species)
    # transformace a převod souřadnic
    print("transformace")
    print("cci_gbif_3035")
    cci_gbif_3035 <- plot_gbif_csv_100 %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        st_transform(rcrs)
    print("cci_ndop_3035")
    cci_ndop_3035 <- plot_ndop_csv_100 %>%
        st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        st_transform(rcrs)
    # preventivní ořezy a spojení obou datasetů
    # cci_gbif_3035 <- st_intersection(cc_gbif_3035, blocks_3035)
    # cci_ndop_3035 <- st_intersection(cc_ndop_3035, czechia_3035)
    print("cci_all_3035")
    cci_all_3035 <- cci_gbif_3035 %>%
        dplyr::select(key, species, geometry) %>%
        add_row(cci_ndop_3035 %>% dplyr::select(key, species, geometry))
    ### nalezy_end###

    saveRDS(cci_gbif_3035, file = paste0(export_path, "/inputs/occurrences/cci_gbif_3035.rds"))
    saveRDS(cci_ndop_3035, file = paste0(export_path, "/inputs/occurrences/cci_ndop_3035.rds"))
    saveRDS(cci_all_3035, file = paste0(export_path, "/inputs/occurrences/cci_all_3035.rds"))

    # write_csv(cci_gbif_3035 , paste0(export_path, "cci_gbif_3035.csv"))
    # write_csv(cci_ndop_3035 , paste0(export_path, "cci_ndop_3035.csv"))
    # write_csv(cci_all_3035, paste0(export_path, "cci_all_3035.csv"))
} else {
    ############################## zatím nefunkční větev, nutno opravit logiku v kódu aby se nemusely pokaždé znovu zpracovávat kompletní csv všech záznamů
    ##############################
    ##############################
    cci_gbif_3035 <- readRDS(paste0(export_path, "/inputs/occurrences/cci_gbif_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species) # netřeba filtrovat zde, stejně druh vybírám znovu v cyklu nad druhy
    cci_ndop_3035 <- readRDS(paste0(export_path, "/inputs/occurrences/cci_ndop_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species)
    cci_all_3035 <- readRDS(paste0(export_path, "/inputs/occurrences/cci_all_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species)
}


# uložení všech výstupů
# enms <- list()
start_time <- Sys.time()
for (px_size_item in px_size) {

    # # původní načítání rasterů prediktorů, dočasná optimalizace aby se nemusel pokaždé skrz propisovat NA hodnoty
    rasters_path <- paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/", px_size_item, "/")
    vif5 <- c(

        # # nepoužito sada 0.7 EV a pak 0.7 CZ 10000km
        # paste0("l8_3-5_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_B5"),
        # paste0("l8_6-8_", px_size_item, "_B5"),
        # paste0("l8_9-11_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_NDWI"),
        # paste0("l8_9-11_", px_size_item, "_MNDWI"),
        # paste0("l8_9-11_", px_size_item, "_NDWI"),
        # paste0("wc_", px_size_item, "_bio03"),
        # paste0("wc_", px_size_item, "_bio04"),
        # paste0("wc_", px_size_item, "_bio09"),
        # paste0("wc_", px_size_item, "_bio13"),
        # paste0("wc_", px_size_item, "_bio15")

        # # DBL7 sada 0.7 EV a pak 0.7 CZ 1000km
        # paste0("l8_3-5_", px_size_item, "_B5"),
        # paste0("l8_6-8_", px_size_item, "_B5"),
        # paste0("l8_3-5_", px_size_item, "_B10"),
        # paste0("l8_6-8_", px_size_item, "_B10"),
        # paste0("l8_9-11_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_NDVI"),
        # paste0("l8_9-11_", px_size_item, "_MNDWI"),
        # paste0("wc_", px_size_item, "_bio03"),
        # paste0("wc_", px_size_item, "_bio04"),
        # paste0("wc_", px_size_item, "_bio09"),
        # paste0("wc_", px_size_item, "_bio13"),
        # paste0("wc_", px_size_item, "_bio15")

        ### totéž vyleze při čistém vifu pro ČR 0.7!!!!!!!
        # 1    l8_3.5_1000_B10 2.620665
        # 2     l8_3.5_1000_B5 4.041503
        # 3    l8_6.8_1000_B10 3.855841
        # 4     l8_6.8_1000_B5 3.079406
        # 5   l8_9.11_1000_B10 1.846656
        # 6   l8_3.5_1000_NDVI 2.236170
        # 7  l8_6.8_1000_MNDWI 1.903304
        # 8      wc_1000_bio03 1.638796
        # 9      wc_1000_bio04 2.114912
        # 10     wc_1000_bio09 2.655333
        # 11     wc_1000_bio13 2.187940
        # 12     wc_1000_bio15 2.710518


        # # DBL75 sada 0.7 EV a pak 0.5 CZ 1000km

        # paste0("l8_9-11_", px_size_item, "_B5"),
        # paste0("l8_3-5_", px_size_item, "_B10"),
        # paste0("l8_9-11_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_NDVI"),
        # paste0("l8_9-11_", px_size_item, "_MNDWI"),
        # paste0("wc_", px_size_item, "_bio03"),
        # paste0("wc_", px_size_item, "_bio04"),
        # paste0("wc_", px_size_item, "_bio09"),
        # paste0("wc_", px_size_item, "_bio15")

        # # OWNA
        # paste0("l8_3-5_", px_size_item, "_B10"),
        # paste0("l8_6-8_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_B7"),
        # paste0("l8_6-8_", px_size_item, "_B7"),
        # paste0("l8_3-5_", px_size_item, "_B5"),
        # paste0("l8_6-8_", px_size_item, "_B5"),
        # paste0("l8_3-5_", px_size_item, "_MNDWI"),
        # paste0("l8_6-8_", px_size_item, "_MNDWI"),
        # paste0("l8_3-5_", px_size_item, "_NDWI"),
        # paste0("l8_6-8_", px_size_item, "_NDWI"),
        # paste0("l8_3-5_", px_size_item, "_NDVI"),
        # paste0("l8_6-8_", px_size_item, "_NDVI"),
        # paste0("wc_", px_size_item, "_bio03"),
        # paste0("wc_", px_size_item, "_bio04"),
        # paste0("wc_", px_size_item, "_bio09"),
        # paste0("wc_", px_size_item, "_bio13"),
        # paste0("wc_", px_size_item, "_bio15")


        # # OWNE
        # paste0("l8_3-5_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_B7"),
        # paste0("l8_3-5_", px_size_item, "_B5"),
        # paste0("l8_3-5_", px_size_item, "_B4"),
        # paste0("l8_3-5_", px_size_item, "_B3"),
        # paste0("l8_3-5_", px_size_item, "_B2"),
        # paste0("l8_3-5_", px_size_item, "_B1"),
        # paste0("l8_3-5_", px_size_item, "_MNDWI"),
        # paste0("l8_3-5_", px_size_item, "_NDWI"),
        # paste0("l8_3-5_", px_size_item, "_NDVI"),
        # paste0("wc_", px_size_item, "_bio03"),
        # paste0("wc_", px_size_item, "_bio04"),
        # paste0("wc_", px_size_item, "_bio09"),
        # paste0("wc_", px_size_item, "_bio13"),
        # paste0("wc_", px_size_item, "_bio15")

        # OWNH - hclust 1000m pro Evropu , zakomentované po redukci na 10 clusterů
        # paste0("l8_9-11_", px_size_item, "_B10"),
        paste0("l8_6-8_", px_size_item, "_B5"),
        paste0("l8_3-5_", px_size_item, "_B4"),
        paste0("l8_3-5_", px_size_item, "_B5"),
        paste0("l8_3-5_", px_size_item, "_MNDWI"),
        paste0("l8_3-5_", px_size_item, "_NDVI"),
        # paste0("l8_6-8_", px_size_item, "_NDWI"),
        paste0("wc_", px_size_item, "_bio03"),
        paste0("wc_", px_size_item, "_bio08"), # kolinear v CR, vyloucit?, radeji bio04, ta si vedle 03 vedla vzdycky dobre?
        paste0("wc_", px_size_item, "_bio09"),
        paste0("wc_", px_size_item, "_bio13"),
        paste0("wc_", px_size_item, "_bio15")





        # # # OWNB
        # paste0("l8_3-5_", px_size_item, "_B10"),
        # paste0("l8_6-8_", px_size_item, "_B10"),
        # paste0("l8_9-11_", px_size_item, "_B10"),
        # paste0("l8_3-5_", px_size_item, "_B7"),
        # paste0("l8_6-8_", px_size_item, "_B7"),
        # paste0("l8_9-11_", px_size_item, "_B7"),
        # paste0("l8_3-5_", px_size_item, "_B5"),
        # paste0("l8_6-8_", px_size_item, "_B5"),
        # paste0("l8_9-11_", px_size_item, "_B5"),
        # paste0("l8_3-5_", px_size_item, "_B4"),
        # paste0("l8_6-8_", px_size_item, "_B4"),
        # paste0("l8_9-11_", px_size_item, "_B4"),
        # paste0("l8_3-5_", px_size_item, "_B3"),
        # paste0("l8_6-8_", px_size_item, "_B3"),
        # paste0("l8_9-11_", px_size_item, "_B3"),
        # paste0("l8_3-5_", px_size_item, "_B2"),
        # paste0("l8_6-8_", px_size_item, "_B2"),
        # paste0("l8_9-11_", px_size_item, "_B2"),
        # paste0("l8_3-5_", px_size_item, "_B1"),
        # paste0("l8_6-8_", px_size_item, "_B1"),
        # paste0("l8_9-11_", px_size_item, "_B1"),
        # paste0("l8_3-5_", px_size_item, "_MNDWI"),
        # paste0("l8_6-8_", px_size_item, "_MNDWI"),
        # paste0("l8_9-11_", px_size_item, "_MNDWI"),
        # paste0("l8_3-5_", px_size_item, "_NDWI"),
        # paste0("l8_6-8_", px_size_item, "_NDWI"),
        # paste0("l8_9-11_", px_size_item, "_NDWI"),
        # paste0("l8_3-5_", px_size_item, "_NDVI"),
        # paste0("l8_6-8_", px_size_item, "_NDVI"),
        # paste0("l8_9-11_", px_size_item, "_NDVI"),
        # paste0("wc_", px_size_item, "_bio03"),
        # paste0("wc_", px_size_item, "_bio04"),
        # paste0("wc_", px_size_item, "_bio09"),
        # paste0("wc_", px_size_item, "_bio13"),
        # paste0("wc_", px_size_item, "_bio15")



        # OWNC - perm importance v GBIF i NDOP > 0.03
    )

    vif5sapply <- lapply(vif5, function(x, rasters_path) {
        return(paste0(rasters_path, x, ".tif"))
    }, rasters_path = rasters_path)
    raster_stack <- stack(lapply(vif5sapply, raster::raster))
    # oprava rasterů ve stacku
    raster_stack <- stack_NA_repair(raster_stack)

    # raster_stack <- stack(paste0(export_path, "/inputs/predictors/central-europe-2levelVIF-", px_size_item, ".grd"))

    rcrs <- crs(raster_stack)


    # rr<-writeRaster(raster_stack, paste0(export_path, "/inputs/predictors/central-europe-2levelVIF-", px_size_item, ".grd"), format = "raster")
    # hdr(rr, format = "ENVI")

    # sjednocení CRS
    blocks_3035 <- blocks %>% st_transform(rcrs)
    blocks_erased_cz_3035 <- blocks_erased_cz %>% st_transform(rcrs)
    czechia_3035 <- czechia %>% st_transform(rcrs)

    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack_crop <- crop(raster_stack, extent(czechia_3035))
    raster_stack_mask_czechia <- mask(raster_stack_crop, czechia_3035)
    raster_stack_mask_czechia <- setMinMax(raster_stack_mask_czechia)

    rlist <- list()

    if (generate_bias_raster == TRUE) {
        ext <- extent(raster_stack[[1]])
        ow <- owin(xrange = c(ext@xmin, ext@xmax), yrange = c(ext@ymin, ext@ymax))

        print("GBIF")
        gbif_ppp <- ppp(st_coordinates(cci_gbif_3035)[, 1], st_coordinates(cci_gbif_3035)[, 2], window = ow)
        # ppp <- rescale(ppp, 10, "km")
        bias_gbif <- resample(raster(density(gbif_ppp, sigma = px_size_item)), raster_stack[[1]], method = "bilinear")

        r.min <- minValue(bias_gbif)
        r.max <- maxValue(bias_gbif)
        bias_gbif <- ((bias_gbif - r.min) / (r.max - r.min)) / 10
        crs(bias_gbif) <- rcrs
        writeRaster(bias_gbif, paste0(export_path, "/inputs/bias_rasters/bbXbias_gbif-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

        print("ALL")
        all_ppp <- ppp(st_coordinates(cci_all_3035)[, 1], st_coordinates(cci_all_3035)[, 2], window = ow)
        # ppp <- rescale(ppp, 10, "km")
        bias_all <- resample(raster(density(all_ppp, sigma = px_size_item)), raster_stack[[1]], method = "bilinear")
        r.min <- minValue(bias_all)
        r.max <- maxValue(bias_all)
        bias_all <- ((bias_all - r.min) / (r.max - r.min)) / 10
        crs(bias_all) <- rcrs
        writeRaster(bias_all, paste0(export_path, "/inputs/bias_rasters/bbXbias_all-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

        print("NDOP")
        ext <- extent(raster_stack_mask_czechia[[1]])
        ow <- owin(xrange = c(ext@xmin, ext@xmax), yrange = c(ext@ymin, ext@ymax))
        ndop_ppp <- ppp(st_coordinates(cci_ndop_3035)[, 1], st_coordinates(cci_ndop_3035)[, 2], window = ow)
        # ppp <- rescale(ppp, 10, "km")
        bias_ndop <- resample(raster(density(ndop_ppp, sigma = px_size_item)), raster_stack_mask_czechia[[1]], method = "bilinear")
        r.min <- minValue(bias_ndop)
        r.max <- maxValue(bias_ndop)
        bias_ndop <- ((bias_ndop - r.min) / (r.max - r.min)) / 10
        crs(bias_ndop) <- rcrs
        writeRaster(bias_ndop, paste0(export_path, "/inputs/bias_rasters/bbXbias_ndop-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

        rlist <- list(bias_gbif, bias_ndop, bias_all)
    } else {
        bias_gbif <- raster(paste0(export_path, "/inputs/bias_rasters/Xbias_gbif-density-", px_size_item, ".tif"))
        # r.min <- minValue(bias_gbif)
        # r.max <- maxValue(bias_gbif)
        # bias_gbif <- ((bias_gbif - r.min) / (r.max - r.min))
        # crs(bias_gbif) <- rcrs


        bias_ndop <- raster(paste0(export_path, "/inputs/bias_rasters/Xbias_ndop-density-", px_size_item, ".tif"))
        # r.min <- minValue(bias_ndop)
        # r.max <- maxValue(bias_ndop)
        # bias_ndop <- ((bias_ndop - r.min) / (r.max - r.min))
        # crs(bias_ndop) <- rcrs

        bias_all <- raster(paste0(export_path, "/inputs/bias_rasters/Xbias_all-density-", px_size_item, ".tif"))
        #                 r.min <- minValue(bias_all)
        #         r.max <- maxValue(bias_all)
        #         bias_all <- ((bias_all - r.min) / (r.max - r.min))
        # crs(bias_all) <- rcrs
    }

    # # # bias_gbif[is.na(bias_gbif[])] <- 0
    # # # bias_all[is.na(bias_all[])] <- 0
    # # # bias_ndop[is.na(bias_ndop[])] <- 0

    # obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků

    ptaci_intersect_distinct <- ptaci_ndop_distinct %>%
        filter(species %in% ptaci_gbif_distinct$species) #  %>% filter(species != "Chlidonias niger")

    species <- rev(ptaci_intersect_distinct$species) # přepisuju původní seznam z ndop_top

    for (sp in species) { # sp in ptaci_gbif_distinct$species
        # foreach(sindex = 1:nrow(species), .combine=combine, .packages=c('dismo', "rJava")) %dopar% {
        # species1_, species2_, species1, species2, species1_short, species2_short, Nálezů

        print("********************************************************** ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        print(sp)

        gbif_f <- cci_gbif_3035 %>% filter(species == as.character(sp))
        ndop_f <- cci_ndop_3035 %>% filter(species == as.character(sp))
        all_f <- cci_all_3035 %>% filter(species == as.character(sp))

        sp_gbif_count <- ptaci_gbif_distinct %>%
            filter(species == as.character(sp)) %>%
            dplyr::select(count)
        sp_ndop_count <- ptaci_ndop_distinct %>%
            filter(species == as.character(sp)) %>%
            dplyr::select(count)

        # preventivní ořezy a spojení obou datasetů
        # cci_gbif_3035 <- st_intersection(cc_gbif_3035, blocks_3035)
        # cci_ndop_3035 <- st_intersection(cc_ndop_3035, czechia_3035)
        ndop_f_n <- nrow(ndop_f)
        gbif_f_n <- nrow(gbif_f)
        f_n <- ndop_f_n * 100 / gbif_f_n

        print(paste0(sp, " -  ", nrow(ndop_f), "/", nrow(gbif_f)))

        if (nrow(ndop_f) == 0 | nrow(gbif_f) == 0) {
            print("XXX")
            next
        } else {
            print("***")
        }

        # příprava occurences
        # per pixel
        enm_mxt_gbif.pp.orig <- as.data.frame(st_coordinates(gbif_f))
        enm_mxt_gbif.pp <- as.data.frame(st_coordinates(st_as_sf(rasterToPoints(rasterize(st_coordinates(gbif_f), raster_stack), spatial = TRUE))))
        colnames(enm_mxt_gbif.pp)[1] <- "Longitude"
        colnames(enm_mxt_gbif.pp)[2] <- "Latitude"

        enm_mxt_ndop.pp.orig <- as.data.frame(st_coordinates(ndop_f))
        enm_mxt_ndop.pp <- as.data.frame(st_coordinates(st_as_sf(rasterToPoints(rasterize(st_coordinates(ndop_f), raster_stack_mask_czechia), spatial = TRUE))))
        colnames(enm_mxt_ndop.pp)[1] <- "Longitude"
        colnames(enm_mxt_ndop.pp)[2] <- "Latitude"

        enm_mxt_all.pp.orig <- as.data.frame(st_coordinates(all_f))
        enm_mxt_all.pp <- as.data.frame(st_coordinates(st_as_sf(rasterToPoints(rasterize(st_coordinates(all_f), raster_stack), spatial = TRUE))))
        colnames(enm_mxt_all.pp)[1] <- "Longitude"
        colnames(enm_mxt_all.pp)[2] <- "Latitude"

        # jen ČR NDOP i GBIF
        # st_coordinates(st_intersection(all_f, czechia_3035))
        local.pp <- as.data.frame(st_coordinates(st_as_sf(rasterToPoints(rasterize(st_coordinates(st_intersection(all_f, st_transform(czechia_3035, st_crs(all_f)))), raster_stack_mask_czechia), spatial = TRUE))))
        colnames(local.pp)[1] <- "Longitude"
        colnames(local.pp)[2] <- "Latitude"

        # background.buffer(points = enm_mxt_all.pp, buffer.width = 50000, buffer.type = "circles", return.type = "polygon", n = 10000)
        # - nefunguje ani v cran veryi ani y githubu> could not find function "background.buffer"
        # buffer.global <- background.shape.buffer(points = enm_mxt_all.pp, radius = 50000)
        # buffer.local <- background.shape.buffer(points = local.pp, radius = 50000)

        # buffer.global <- background.buffer(points = enm_mxt_all.pp, buffer.width = 50000, buffer.type = "circles", return.type = "polygon", n = 10000)
        # buffer.local <- background.buffer(points = local.pp , buffer.width = 50000, buffer.type = "circles", return.type = "polygon", n = 10000)

        # buffer.global <- buffer(as_Spatial(all_f), width = 50000, dissolve = TRUE)
        # buffer.local <- buffer(as_Spatial(st_intersection(all_f, czechia_3035)), width = 50000, dissolve = TRUE)

        buffer.global <- background.raster.buffer(points = enm_mxt_all.pp, radius = 100000, mask = raster_stack[[1]])
        buffer.local <- background.raster.buffer(points = local.pp, radius = 100000, mask = raster_stack_mask_czechia[[1]])

        print("maskování prediktorů dle bufferu druhu")
        raster_stack_b <- mask(raster_stack, buffer.global)
        raster_stack_b <- setMinMax(raster_stack_b)

        raster_stack_mask_czechia_b <- mask(raster_stack_mask_czechia, buffer.local)
        raster_stack_mask_czechia_b <- setMinMax(raster_stack_mask_czechia_b)

        bias_gbif_b <- mask(bias_gbif, buffer.global)
        bias_all_b <- mask(bias_all, buffer.global)
        bias_ndop_b <- mask(bias_ndop, buffer.local)
        bias_gbif_b <- setMinMax(bias_gbif_b)
        bias_all_b <- setMinMax(bias_all_b)
        bias_ndop_b <- setMinMax(bias_ndop_b)

        # st_write(st_as_sf(buf), paste0(export_path, "delete-buffery.shp"))
        # buf <- buffer(as_Spatial(st_intersection(all_f, czechia_3035)), width=50000, dissolve=TRUE)

        ###
        ### gbif
        ###
        print("GBIF")


        enm_species <- enmtools.species(
            range = buffer.global, #  raster_stack_b[[1]],
            species.name = as.character(sp), presence.points = enm_mxt_gbif.pp
        )
        # enms[["10000"]][["Buteo rufinus"]][[1]][["o"]]$presence.points$Longitude

        # bias2 <- raster("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/bias_tg_Chlidonias-hybrida_2_gbif.tif") # 100000 10000 - totožné pro všešchny rozlišení rasterů?


        ### test co se děje při zjišťování env.test/training.evaluation
        # species <- check.bg(enm_species, raster_stack_b, verbose = TRUE)
        # presence <- species$presence.points[, 1:2]
        # background <- species$background.points[, 1:2]
        # allpoints <- rbind(presence, background)
        # values <- extract(raster_stack_b, allpoints)
        # maxes <- apply(values, 2, function(x) max(x, na.rm = TRUE))
        # mins <- apply(values, 2, function(x) min(x, na.rm = TRUE))
        # library(lhs)
        # this.lhs <- randomLHS(n.background, length(names(raster_stack_b)))
        # bg.table <- t(t(this.lhs) * (maxes - mins) + mins)
        # colnames(bg.table) <- names(raster_stack_b)
        # p.table <- extract(raster_stack_b, presence)
        # pred.p <- as.numeric(predict(model, newdata = data.frame(p.table), x = data.frame(p.table), type = "response"))
        # pred.bg <- as.numeric(predict(model, newdata = data.frame(bg.table), x = data.frame(bg.table), type = "response"))
        # env.evaluation <- dismo::evaluate(pred.p, pred.bg)
        # stop()

        enm_mxt_gbif.s <- enm_species

        enm_mxt_gbif <- replicate(replicates, enmtools.glm(
            enm_species,
            raster_stack_b,
            test.prop = 0.3,
            bg.source = "range",
            verbose = TRUE,
            bias = bias_gbif_b,
            nback = 10000
            # args = c("threads=4")
        ),
        simplify = FALSE
        )

        cm <- lapply(enm_mxt_gbif, function(x) performance(x$conf))
        enm_mxt_gbif.matrix <- abind(cm, along = 3)
        enm_mxt_gbif.perf <- apply(enm_mxt_gbif.matrix, c(1, 2), mean)

        # thr.gbif <- mean(sapply(enm_mxt_gbif, function(x) x$thr$spec_sens))

        thr.gbif <- sapply(enm_mxt_gbif, function(x) x$thr)

        sedi.gbif.p <- lapply(enm_mxt_gbif, function(x) sedi(x$conf))
        sedi.gbif <- mean(sapply(sedi.gbif.p, function(x) {
            if (x[[1]] == "SEDI equal to") {
                x[[2]]
            } else {
                NA
            }
        }))

        # str(enm_mxt_gbif[[1]]$test.evaluation@auc)
        # str(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]][[1]]$test.evaluation@auc)
        # enm_mxt_gbif.auc <- mean(sapply(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]], function(x) x$test.evaluation@auc))


        enm_mxt_gbif.breadth <- lapply(enm_mxt_gbif, raster.breadth)
        enm_mxt_gbif.breadth.B1 <- mean(sapply(enm_mxt_gbif.breadth, function(x) x$B1))
        enm_mxt_gbif.breadth.B2 <- mean(sapply(enm_mxt_gbif.breadth, function(x) x$B2))

        enm_mxt_gbif.ebreadth <- lapply(enm_mxt_gbif, env.breadth, env = raster_stack_b)
        enm_mxt_gbif.ebreadth.B2 <- mean(sapply(enm_mxt_gbif.ebreadth, function(x) x$env.B2))

        enm_mxt_gbif.auc <- mean(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc))
        enm_mxt_gbif.np.tr <- mean(sapply(enm_mxt_gbif, function(x) x$training.evaluation@np))
        enm_mxt_gbif.np.te <- mean(sapply(enm_mxt_gbif, function(x) x$test.evaluation@np))
        # plot(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]][[1]]$suitability)
        # names(enm_mxt_gbif) <- paste0("rep", 1:repl)
        enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
        enm_mxt_gbif.r.m <- calc(enm_mxt_gbif.r, fun = mean)
        writeRaster(enm_mxt_gbif.r.m, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_gbif.tif"), format = "GTiff", overwrite = TRUE)
        png(paste0(export_path, "/outputs/png/", pres, "_", sp, "_", px_size_item, "_", replicates, "_gbif.png"))
        plot(enm_mxt_gbif.r.m,
            main = paste0(sp, " | GBIF, AUC=", round(enm_mxt_gbif.auc, digits = 2), " (", (px_size_item / 1000), "km)"),
            sub = paste0("GBIF: ", gbif_f_n)
        )
        par(bg = NA)
        plot(czechia_3035$geometry, add = TRUE)
        par()
        points(enm_mxt_gbif.pp.orig, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
        dev.off()


        # Maximum test sensitivity plus specificity
        # pro Maxent nemůže být plogis, při cloglog ýýstupu (nebo raději dát předtím do args RAW? - je ale k dispozici v rmaxent::project(), případně v původní???)
        # použít https://rdrr.io/github/johnbaums/rmaxent/man/to_logistic.html to_logistic(x = thr.gbif.mss, from = "cloglog")
        # https://rpubs.com/mlibxmda/GEOG70922_Week5
        # back-transform these values to approximate probabilities (i.e. values ranging from 0 to 1) using the plogis()
        thr.gbif.mss <- plogis(mean(sapply(enm_mxt_gbif, function(x) {
            x$test.evaluation@t[which.max(x$test.evaluation@TPR + x$test.evaluation@TNR)]
        })))

        raster.gbif <- enm_mxt_gbif.r.m
        raster.gbif[raster.gbif < thr.gbif.mss] <- 0
        raster.gbif[raster.gbif >= thr.gbif.mss] <- 1
        pa.gbif.freq <- freq(raster.gbif)
        writeRaster(raster.gbif, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_gbif.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "INT1U")

        # raster::predict()  pro GLM https://rdrr.io/cran/raster/man/predict.html
        # dismo::predict() pro Maxent https://www.rdocumentation.org/packages/dismo/versions/1.3-3/topics/predict
        # ne, teď už můžu použít rovnou suitability raster...

        enm_mxt_gbif.vip <- sapply(enm_mxt_gbif, enmtools.vip)

        # calculates Continuous Boyce Index
        # str(enms[["10000"]][["Buteo rufinus"]][[4]]
        ### enm_mxt_gbif.cal <- lapply(enm_mxt_gbif, enmtools.calibrate, env = raster_stack_b, n.background = 10000)



        ###
        ### ndop
        ###

        print("NDOP")

        enm_species <- enmtools.species(
            range = buffer.local, # raster_stack_mask_czechia_b[[1]],
            species.name = as.character(sp), presence.points = enm_mxt_ndop.pp
        )


        enm_mxt_ndop.s <- enm_species

        enm_mxt_ndop <- replicate(replicates, enmtools.glm(
            enm_species,
            raster_stack_mask_czechia_b,
            test.prop = 0.3,
            bg.source = "range",
            verbose = TRUE,
            bias = bias_ndop_b,
            nback = 10000
            # args = c("threads=4")
        ),
        simplify = FALSE
        )

        cm <- lapply(enm_mxt_ndop, function(x) performance(x$conf))
        enm_mxt_ndop.matrix <- abind(cm, along = 3)
        enm_mxt_ndop.perf <- apply(enm_mxt_ndop.matrix, c(1, 2), mean)

        thr.ndop <- sapply(enm_mxt_ndop, function(x) x$thr)

        sedi.ndop.p <- lapply(enm_mxt_ndop, function(x) sedi(x$conf))
        sedi.ndop <- mean(sapply(sedi.ndop.p, function(x) {
            if (x[[1]] == "SEDI equal to") {
                x[[2]]
            } else {
                NA
            }
        }))

        enm_mxt_ndop.breadth <- lapply(enm_mxt_ndop, raster.breadth)
        enm_mxt_ndop.breadth.B1 <- mean(sapply(enm_mxt_ndop.breadth, function(x) x$B1))
        enm_mxt_ndop.breadth.B2 <- mean(sapply(enm_mxt_ndop.breadth, function(x) x$B2))

        enm_mxt_ndop.ebreadth <- lapply(enm_mxt_ndop, env.breadth, env = raster_stack_mask_czechia_b)
        enm_mxt_ndop.ebreadth.B2 <- mean(sapply(enm_mxt_ndop.ebreadth, function(x) x$env.B2))

        enm_mxt_ndop.auc <- mean(sapply(enm_mxt_ndop, function(x) x$test.evaluation@auc))
        enm_mxt_ndop.np.tr <- mean(sapply(enm_mxt_ndop, function(x) x$training.evaluation@np))
        enm_mxt_ndop.np.te <- mean(sapply(enm_mxt_ndop, function(x) x$test.evaluation@np))
        enm_mxt_ndop.r <- stack(sapply(enm_mxt_ndop, function(x) x$suitability))
        enm_mxt_ndop.r.m <- calc(enm_mxt_ndop.r, fun = mean)

        writeRaster(enm_mxt_ndop.r.m, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_ndop.tif"), format = "GTiff", overwrite = TRUE)
        png(paste0(export_path, "/outputs/png/", pres, "_", sp, "_", px_size_item, "_", replicates, "_ndop.png"))
        plot(enm_mxt_ndop.r.m,
            main = paste0(sp, " | NDOP, AUC=", round(enm_mxt_ndop.auc, digits = 2), " (", (px_size_item / 1000), "km)"),
            sub = paste0("NDOP: ", ndop_f_n)
        )
        par(bg = NA)
        plot(czechia_3035$geometry, add = TRUE)
        par()
        points(enm_mxt_ndop.pp.orig, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
        dev.off()


        # Maximum test sensitivity plus specificity
        thr.ndop.mss <- plogis(mean(sapply(enm_mxt_ndop, function(x) {
            x$test.evaluation@t[which.max(x$test.evaluation@TPR + x$test.evaluation@TNR)]
        })))

        raster.ndop <- enm_mxt_ndop.r.m
        raster.ndop[raster.ndop < thr.ndop.mss] <- 0
        raster.ndop[raster.ndop >= thr.ndop.mss] <- 1

        pa.ndop.freq <- freq(raster.ndop)
        writeRaster(raster.ndop, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_ndop.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "LOG1S")

        enm_mxt_ndop.vip <- sapply(enm_mxt_ndop, enmtools.vip)

        ### enm_mxt_ndop.cal <- lapply(enm_mxt_ndop, enmtools.calibrate, env = raster_stack_mask_czechia_b, n.background = 10000)



        ###
        ### all (ndpop+gbif)
        ###
        print("ALL")

        enm_species <- enmtools.species(
            range = buffer.global, # raster_stack_b[[1]],
            species.name = as.character(sp), presence.points = enm_mxt_all.pp
        )

        enm_mxt_all.s <- enm_species

        enm_mxt_all <- replicate(replicates, enmtools.glm(
            enm_species,
            raster_stack_b,
            test.prop = 0.3,
            bg.source = "range",
            verbose = TRUE,
            bias = bias_all_b,
            nback = 10000
            # args = c("threads=4")
        ),
        simplify = FALSE
        )

        cm <- lapply(enm_mxt_all, function(x) performance(x$conf))
        enm_mxt_all.matrix <- abind(cm, along = 3)
        enm_mxt_all.perf <- apply(enm_mxt_all.matrix, c(1, 2), mean)

        thr.all <- sapply(enm_mxt_all, function(x) x$thr)

        sedi.all.p <- lapply(enm_mxt_all, function(x) sedi(x$conf))
        sedi.all <- mean(sapply(sedi.all.p, function(x) {
            if (x[[1]] == "SEDI equal to") {
                x[[2]]
            } else {
                NA
            }
        }))

        enm_mxt_all.breadth <- lapply(enm_mxt_all, raster.breadth)
        enm_mxt_all.breadth.B1 <- mean(sapply(enm_mxt_all.breadth, function(x) x$B1))
        enm_mxt_all.breadth.B2 <- mean(sapply(enm_mxt_all.breadth, function(x) x$B2))

        enm_mxt_all.ebreadth <- lapply(enm_mxt_all, env.breadth, env = raster_stack_b)
        enm_mxt_all.ebreadth.B2 <- mean(sapply(enm_mxt_all.ebreadth, function(x) x$env.B2))

        enm_mxt_all.auc <- mean(sapply(enm_mxt_all, function(x) x$test.evaluation@auc))
        enm_mxt_all.np.tr <- mean(sapply(enm_mxt_all, function(x) x$training.evaluation@np))
        enm_mxt_all.np.te <- mean(sapply(enm_mxt_all, function(x) x$test.evaluation@np))
        enm_mxt_all.r <- stack(sapply(enm_mxt_all, function(x) x$suitability))
        enm_mxt_all.r.m <- calc(enm_mxt_all.r, fun = mean)

        writeRaster(enm_mxt_all.r.m, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_all.tif"), format = "GTiff", overwrite = TRUE)
        png(paste0(export_path, "/outputs/png/", pres, "_", sp, "_", px_size_item, "_", replicates, "_all.png"))
        plot(enm_mxt_all.r.m,
            main = paste0(sp, " | GBIF+NDOP, AUC=", round(enm_mxt_all.auc, digits = 2), " (", (px_size_item / 1000), "km)"),
            sub = paste0("(NDOP/GBIF: ", ndop_f_n, "/", gbif_f_n, " = ", round(f_n), "%")
        )
        par(bg = NA)
        plot(czechia_3035$geometry, add = TRUE)
        par()
        points(enm_mxt_all.pp.orig, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5))
        dev.off()

        # Maximum test sensitivity plus specificity
        thr.all.mss <- plogis(mean(sapply(enm_mxt_all, function(x) {
            x$test.evaluation@t[which.max(x$test.evaluation@TPR + x$test.evaluation@TNR)]
        })))

        raster.all <- enm_mxt_all.r.m
        raster.all[raster.all < thr.all.mss] <- 0
        raster.all[raster.all >= thr.all.mss] <- 1
        pa.all.freq <- freq(raster.all)

        writeRaster(raster.all, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_all.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "LOG1S")




        enm_mxt_all.vip <- sapply(enm_mxt_all, enmtools.vip)

        ### enm_mxt_all.cal <- lapply(enm_mxt_all, enmtools.calibrate, env = raster_stack_b, n.background = 10000)


        # # # # # # # # ořezy # # # # # # # # # #


        # po ořezu nenínutné znormalizovat nově vzniklé rastery - dělá se v raster.breadth a raster.overlap automaticky přes raster.standardize (ten ale standardizuje čistě sumou všech pixelů...?!)

        # # ořez výsledné predikce
        # ořez GBIF ČR
        enm_mxt_gbif.r.m.crop <- crop(enm_mxt_gbif.r.m, extent(czechia_3035))
        enm_mxt_gbif.r.m.crop.czechia <- mask(enm_mxt_gbif.r.m.crop, czechia_3035)
        # binární
        raster.gbif.crop <- crop(raster.gbif, extent(czechia_3035))
        raster.gbif.crop.czechia <- mask(raster.gbif.crop, czechia_3035)


        # ořez ALL ČR
        enm_mxt_all.r.m.crop <- crop(enm_mxt_all.r.m, extent(czechia_3035))
        enm_mxt_all.r.m.crop.czechia <- mask(enm_mxt_all.r.m.crop, czechia_3035)
        # binární
        raster.all.crop <- crop(raster.all, extent(czechia_3035))
        raster.all.crop.czechia <- mask(raster.all.crop, czechia_3035)

        # výřez ČR z GBIF
        enm_mxt_gbif.r.m.erase <- crop(enm_mxt_gbif.r.m, extent(blocks_erased_cz_3035))
        enm_mxt_gbif.r.m.erase.czechia <- mask(enm_mxt_gbif.r.m.erase, blocks_erased_cz_3035)
        # binární
        raster.gbif.erase <- crop(raster.gbif, extent(blocks_erased_cz_3035))
        raster.gbif.erase.czechia <- mask(raster.gbif.erase, blocks_erased_cz_3035)

        # výřez ČR z ALL
        enm_mxt_all.r.m.erase <- crop(enm_mxt_all.r.m, extent(blocks_erased_cz_3035))
        enm_mxt_all.r.m.erase.czechia <- mask(enm_mxt_all.r.m.erase, blocks_erased_cz_3035)
        # binární
        raster.all.erase <- crop(raster.all, extent(blocks_erased_cz_3035))
        raster.all.erase.czechia <- mask(raster.all.erase, blocks_erased_cz_3035)


        # dodatečné raster breadth
        # cropnutá ČR
        gbif_crop.breadth <- raster.breadth(enm_mxt_gbif.r.m.crop.czechia)
        all_crop.breadth <- raster.breadth(enm_mxt_all.r.m.crop.czechia)

        # erasnutá ČR
        gbif_erase.breadth <- raster.breadth(enm_mxt_gbif.r.m.crop.czechia)
        all_erase.breadth <- raster.breadth(enm_mxt_all.r.m.crop.czechia)




        eos.gbif_all <- list()
        for (r in 1:replicates) {
            eos.gbif_all[[r]] <- env.overlap(enm_mxt_all[[r]], enm_mxt_gbif[[r]], env = raster_stack_b)[1:3]
        }

        eos.gbif_ndop <- list() # má smysl, když jsou odlišné extenty?
        for (r in 1:replicates) {
            eos.gbif_ndop[[r]] <- env.overlap(enm_mxt_gbif[[r]], enm_mxt_ndop[[r]], env = raster_stack_b)[1:3]
        }

        eos.all_ndop <- list() # má smysl, když jsou odlišné extenty?
        for (r in 1:replicates) {
            eos.all_ndop[[r]] <- env.overlap(enm_mxt_all[[r]], enm_mxt_ndop[[r]], env = raster_stack_b)[1:3]
        }

        #
        ## # # # #  niche identity, zatím ne, není jak to jednoznačně strojově interpretovat dle jedné hodnoty - asi se musí vztáhnout jednotlivá opakování k empirické hodnotě?
        #
        # ni.gbif_ndop <- identity.test(species.1 = enm_mxt_gbif.s, species.2 = enm_mxt_ndop.s, env = raster_stack_b, type = "glm", nreps = replicates)
        # ni.gbif_all <- identity.test(species.1 = enm_mxt_all.s, species.2 = enm_mxt_gbif.s, env = raster_stack_b, type = "glm", nreps = replicates)
        # #jak vypadá srovnání dvou stejných modelů?
        # ni.gbif_gbif <- identity.test(species.1 = enm_mxt_gbif.s, species.2 = enm_mxt_gbif.s, env = raster_stack_b, type = "glm", nreps = replicates)


        enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
            as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
        })

        enm_mxt_ndop.vip.t <- lapply(enm_mxt_ndop.vip[seq(1, replicates * 2, 2)], function(x) {
            as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
        })

        enm_mxt_all.vip.t <- lapply(enm_mxt_all.vip[seq(1, replicates * 2, 2)], function(x) {
            as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
        })

        if (replicates == 1) {
            enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
            enm_mxt_ndop.vip.s <- enm_mxt_ndop.vip.t[[1]]
            enm_mxt_all.vip.s <- enm_mxt_all.vip.t[[1]]
        } else {
            b_g <- enm_mxt_gbif.vip.t[[1]]
            b_n <- enm_mxt_ndop.vip.t[[1]]
            b_a <- enm_mxt_all.vip.t[[1]]
            for (n in 1:replicates) {
                if (n > 1) {
                    b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
                    b_n %<>% add_row(enm_mxt_ndop.vip.t[[n]])
                    b_a %<>% add_row(enm_mxt_all.vip.t[[n]])
                }
            }
            enm_mxt_gbif.vip.s <- b_g %>%
                summarise_if(is.numeric, mean, na.rm = TRUE)

            enm_mxt_ndop.vip.s <- b_n %>%
                summarise_if(is.numeric, mean, na.rm = TRUE)

            enm_mxt_all.vip.s <- b_a %>%
                summarise_if(is.numeric, mean, na.rm = TRUE)
        }
        enm_mxt_gbif.vip.s.z <- enm_mxt_gbif.vip.s %>% unite("enm_mxt_gbif.vip", names(enm_mxt_gbif.vip.t[[1]])) # separate(xy, c("x", "y"))
        enm_mxt_ndop.vip.s.z <- enm_mxt_ndop.vip.s %>% unite("enm_mxt_ndop.vip", names(enm_mxt_ndop.vip.t[[1]])) # separate(xy, c("x", "y"))
        enm_mxt_all.vip.s.z <- enm_mxt_all.vip.s %>% unite("enm_mxt_all.vip", names(enm_mxt_all.vip.t[[1]])) # separate(xy, c("x", "y"))


        gbif_ndop.geo.m <- raster.overlap(enm_mxt_gbif.r.m.crop.czechia, enm_mxt_ndop.r.m)
        gbif_all.geo.m <- raster.overlap(enm_mxt_all.r.m, enm_mxt_gbif.r.m)
        gbif_all_erase.geo.m <- raster.overlap(enm_mxt_all.r.m.erase.czechia, enm_mxt_gbif.r.m.erase.czechia)


        #     gbif <- calc(stack(sapply(enm_mxt_gbif, function(x) x$suitability)), fun = mean),
        #     ndop <- calc(stack(sapply(enm_mxt_ndop, function(x) x$suitability)), fun = mean),
        #     all <- calc(stack(sapply(enm_mxt_all, function(x) x$suitability)), fun = mean)
        #
        #
        #  přidat sensitivitu, specificitu a TSS!!!
        #
        #
        enmsr[[as.character(px_size_item)]][[as.character(sp)]] <- list(
            reps = replicates,
            px_size_item = px_size_item,
            species = as.character(sp),
            ndop_c = ndop_f_n,
            gbif_c = gbif_f_n,
            gbif.np.tr = enm_mxt_gbif.np.tr,
            gbif.np.te = enm_mxt_gbif.np.te,
            all.np.tr = enm_mxt_all.np.tr,
            all.np.te = enm_mxt_all.np.te,
            ndop.np.tr = enm_mxt_ndop.np.tr,
            ndop.np.te = enm_mxt_ndop.np.te,
            # rbreadth
            gbif.breadth.B1 = enm_mxt_gbif.breadth.B1,
            gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
            ndop.breadth.B1 = enm_mxt_ndop.breadth.B1,
            ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
            all.breadth.B1 = enm_mxt_all.breadth.B1,
            all.breadth.B2 = enm_mxt_all.breadth.B2,
            # rbreadth dodatečné po cropu a erasu
            gbif_crop.breadth.B1 = gbif_crop.breadth$B1,
            gbif_crop.breadth.B2 = gbif_crop.breadth$B2,
            all_crop.breadth.B1 = all_crop.breadth$B1,
            all_crop.breadth.B2 = all_crop.breadth$B2,
            gbif_erase.breadth.B1 = gbif_erase.breadth$B1,
            gbif_erase.breadth.B2 = gbif_erase.breadth$B2,
            all_erase.breadth.B1 = all_erase.breadth$B1,
            all_erase.breadth.B2 = all_erase.breadth$B2,

            # ebreath
            gbif.ebreadth.B2 = enm_mxt_gbif.ebreadth.B2,
            ndop.ebreadth.B2 = enm_mxt_ndop.ebreadth.B2,
            all.ebreadth.B2 = enm_mxt_all.ebreadth.B2,

            # AUC
            auc.gbif.tr = mean(sapply(enm_mxt_gbif, function(x) x$training.evaluation@auc)),
            auc.gbif.te = mean(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc)),
            auc.gbif.tr.env = mean(sapply(enm_mxt_gbif, function(x) x$env.training.evaluation@auc)),
            auc.gbif.te.env = mean(sapply(enm_mxt_gbif, function(x) x$env.test.evaluation@auc)),
            # auc.gbif.boyce = mean(sapply(enm_mxt_gbif.cal, function(x) x$continuous.boyce$Spearman.cor)),

            auc.ndop.tr = mean(sapply(enm_mxt_ndop, function(x) x$training.evaluation@auc)),
            auc.ndop.te = mean(sapply(enm_mxt_ndop, function(x) x$test.evaluation@auc)),
            auc.ndop.tr.env = mean(sapply(enm_mxt_ndop, function(x) x$env.training.evaluation@auc)),
            auc.ndop.te.env = mean(sapply(enm_mxt_ndop, function(x) x$env.test.evaluation@auc)),
            # auc.ndop.boyce = mean(sapply(enm_mxt_ndop.cal, function(x) x$continuous.boyce$Spearman.cor)),

            auc.all.tr = mean(sapply(enm_mxt_all, function(x) x$training.evaluation@auc)),
            auc.all.te = mean(sapply(enm_mxt_all, function(x) x$test.evaluation@auc)),
            auc.all.tr.env = mean(sapply(enm_mxt_all, function(x) x$env.training.evaluation@auc)),
            auc.all.te.env = mean(sapply(enm_mxt_all, function(x) x$env.test.evaluation@auc)),
            # auc.all.boyce = mean(sapply(enm_mxt_all.cal, function(x) x$continuous.boyce$Spearman.cor)),

            # sedi
            sedi.gbif = sedi.gbif,
            sedi.all = sedi.all,
            sedi.ndop = sedi.ndop,

            # thr
            thr.gbif = as_tibble(thr.gbif),
            thr.all = as_tibble(thr.all),
            thr.ndop = as_tibble(thr.ndop),

            # PA
            pa.gbif.sum.p = get_freq_by_cat(pa.gbif.freq, 1),
            pa.gbif.sum.a = get_freq_by_cat(pa.gbif.freq, 0),
            pa.all.sum.p = get_freq_by_cat(pa.all.freq, 1),
            pa.all.sum.a = get_freq_by_cat(pa.all.freq, 0),
            pa.ndop.sum.p = get_freq_by_cat(pa.ndop.freq, 1),
            pa.ndop.sum.a = get_freq_by_cat(pa.ndop.freq, 0),

            # překryvy PA map
            gbif_ndop.pa = as_tibble(rasters_confusion(raster.ndop, raster.gbif.crop.czechia)),
            all_ndop.pa = as_tibble(rasters_confusion(raster.ndop, raster.all.crop.czechia)),
            all_gbif.pa = as_tibble(rasters_confusion(raster.all, raster.gbif)),
            all_gbif_erase.pa = as_tibble(rasters_confusion(raster.all.erase.czechia, raster.gbif.erase.czechia)),

            # performance indexy, treshold maxSSS (dismo: spec_sens)
            perf.gbif = as_tibble(enm_mxt_gbif.perf),
            perf.all = as_tibble(enm_mxt_all.perf),
            perf.ndop = as_tibble(enm_mxt_ndop.perf),

            # variable permutation importance - tibble
            vip1.gbif = enm_mxt_gbif.vip.s,
            vip1.ndop = enm_mxt_ndop.vip.s,
            vip1.all = enm_mxt_all.vip.s,

            # geogr. overlap
            gbif_ndop.geo.D = gbif_ndop.geo.m$D,
            gbif_ndop.geo.I = gbif_ndop.geo.m$I,
            gbif_ndop.geo.cor = gbif_ndop.geo.m$rank.cor,

            gbif_all.geo.D = gbif_all.geo.m$D,
            gbif_all.geo.I = gbif_all.geo.m$I,
            gbif_all.geo.cor = gbif_all.geo.m$rank.cor,

            gbif_all_erase.geo.D = gbif_all_erase.geo.m$D,
            gbif_all_erase.geo.I = gbif_all_erase.geo.m$I,
            gbif_all_erase.geo.cor = gbif_all_erase.geo.m$rank.cor,

            # niche overlap
            gbif_all.env.D = mean(map_dbl(eos.gbif_all, ~ (.$env.D))),
            gbif_all.env.I = mean(map_dbl(eos.gbif_all, ~ (.$env.I))),
            gbif_all.env.cor = mean(map_dbl(eos.gbif_all, ~ (.$env.cor))),

            gbif_ndop.env.D = mean(map_dbl(eos.gbif_ndop, ~ (.$env.D))),
            gbif_ndop.env.I = mean(map_dbl(eos.gbif_ndop, ~ (.$env.I))),
            gbif_ndop.env.cor = mean(map_dbl(eos.gbif_ndop, ~ (.$env.cor))),

            all_ndop.env.D = mean(map_dbl(eos.all_ndop, ~ (.$env.D))),
            all_ndop.env.I = mean(map_dbl(eos.all_ndop, ~ (.$env.I))),
            all_ndop.env.cor = mean(map_dbl(eos.all_ndop, ~ (.$env.cor)))
        )
        gc()
    }

    #  dev.off()
}
timestamp <- unclass(as.POSIXct(Sys.time()))
saveRDS(enmsr, file = paste0(export_path, "/outputs/rds/enmsr_", pres, "_", px_size, "_", replicates, "_", timestamp, ".rds"))

end_time <- Sys.time()

# časové rozmezí a celkový čas generování
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)





# # dočasné ošetření (NA - pomalé u 100m) a uložení používaných bandů do jednoho rasteru
# px_size_item <- 10000
# rasters_path <- paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/", px_size_item, "/")
# vif5 <- c(
#     paste0("l8_3-5_", px_size_item, "_MNDWI"),
#     paste0("l8_6-8_", px_size_item, "_EVI"),
#     paste0("l8_6-8_", px_size_item, "_MNDWI"),
#     paste0("l8_9-11_", px_size_item, "_B5"),
#     paste0("l8_9-11_", px_size_item, "_EVI"),
#     paste0("l8_9-11_", px_size_item, "_MNDWI"),
#     paste0("wc_", px_size_item, "_bio02"),
#     paste0("wc_", px_size_item, "_bio03"),
#     paste0("wc_", px_size_item, "_bio09"),
#     paste0("wc_", px_size_item, "_bio13"),
#     paste0("wc_", px_size_item, "_bio15")
# )
# vif5sapply <- lapply(vif5, function(x, rasters_path) {
#     return(paste0(rasters_path, x, ".tif"))
# }, rasters_path = rasters_path)
# raster_stack <- stack(lapply(vif5sapply, raster::raster))
#    names(raster_stack) <- vif5
# # propíše všude jednotně NA - nutné, asi dříve problém s predikcemi nad většími oblastmi s NA nad Alpami?
# raster_stack <- raster::mask(raster_stack, sum(raster_stack))
# rcrs <- crs(raster_stack)
# raster_stack <- setMinMax(raster_stack)
# rr <- writeRaster(raster_stack, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons-prep-11/central-europe-", px_size_item, ".grd"), format = "raster")
# hdr(rr, format = "ENVI")

# source("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/enmtools.R", encoding = "UTF-8")
# system(paste("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/enmtools.R", 1))
#      evipn <- lapply(evip, function(x) {

# as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[,2], x$Variable))))))
#       # print(str(x))
# #purrr::transpose(x[,2], x$Variable)
# #as.data.frame(t(as.matrix(unlist(purrr::transpose(x[,2], x$Variable)))))

# # ttest <- as_tibble(x)
# #   ttest %>%
# #      dplyr::select(-StDev) %>%
# #    pivot_longer(cols = c(Importance)) %>%
# #    group_by(Variable) %>%
# #    pivot_wider(names_from = Variable, values_from = value)

#     })








# evip <- enm_mxt_gbif.vip[seq(1, replicates * 2, 2)]
# evipn <- lapply(evip, function(x) {
#     as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
# })

# ta <- do.call("add_row", evipn)

# taS <- ta %>% summarise_if(is.numeric, mean, na.rm = TRUE)
# taS %>% unite("new", names(taS)) # separate(xy, c("x", "y"))


# do.call(paste, c(as.list(taS), sep = ","))


# write_csv(taS, paste0(export_path, "uzzzz.csv"), append = TRUE)


# enmsr[[1]][[1]]$gbif_all.env
# mean(map_dbl(enmsr[[1]][[1]]$gbif_all.env, ~ (.$env.D)))

# separate(enmsr[[1]][[1]]$vip.all , c("a","b","c","d","e","f","g","h","i","j","k"))

# as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(enmsr[[1]][[1]]$vip[[1]][, 2], enmsr[[1]][[1]]$vip[[1]]$Variable))))))


# as_tibble(purrr::transpose(enmsr[[1]][[1]]$vip[[1]][, 2], enmsr[[1]][[1]]$vip[[1]]$Variable))
# names(x)


# transpose(enmsr[[1]][[1]]$vip[[1]][, 2])

# ttest <- as_tibble(enmsr[[1]][[1]]$vip[[1]])

# ttest %>%
#     gather(Importance, 1:ncol(ttest)) %>%
#     spread(Series.Description, val)



# as_tibble(cbind(vars = enmsr[[1]][[1]]$vip[[1]]$Variable, t(enmsr[[1]][[1]]$vip[[1]][, 2]))) %>% .[-c(1:2), ]


# ttest %>%
#     dplyr::select(-StDev) %>%
#     pivot_longer(cols = c(Importance)) %>%
#     group_by(Variable) %>%
#     pivot_wider(names_from = Variable, values_from = value)



# write.table(mat, file = "test.txt")


# write.csv2(as.data.frame(t(evipn)), paste(export_path, "jeden-delete2.csv"))



# mtcars %>%
#     rownames_to_column() %>%
#     pivot_longer(-rowname, "variable", "value") %>%
#     pivot_wider(variable, rowname)




# m_poz <- matrix(c(1,2,3,4,5,6), ncol = 2)
# m_negat <- matrix(c(1,2,3,6,5,4), ncol= 2)
# print(m_poz)
# print(m_negat)
# plot(m_poz)
# plot(m_negat)
# print(cor(m_poz[,1] , m_poz[,2] , method="spearman"))
# print(cor(m_negat[,1] , m_poz[,2] , method="spearman"))
# stop()