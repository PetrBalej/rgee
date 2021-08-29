options(scipen = 999) # výpis čísel v nezkrácené podobě
options(java.parameters = c("-Xmx20g"))
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "dismo", "ENMToolsPB", "spatstat", "purrr", "abind") # "rmaxent", "blockCV", "ggplot2", "MASS", "data.table", "virtualspecies" (convertToPA - problematické definovat parametry v reálném světě...)
install.packages(setdiff(required_packages, rownames(installed.packages())))
# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip
# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
# wd <- "G:/balej/iga/rgee"
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

# export_path <- "G:/balej/iga/vse-v-jednom"
export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom"

alg <- "glm" # "glm" "maxent" "gam"
px_size <- c(10000) # 100, 500, 1000, 5000, 10000 # 10000, 5000, 1000, 500, 100
replicates <- 1 # 4 pro checkerboard2 (4foldy)
pref <- "_LV_" # předpona png obrázků s predikcí a dalších outputů / OWNPFr /// _OF-ps80_ BFit
test.prop <- 0.3 # "block" "checkerboard2" 0.3; pro bias fitting 0.0 - manuálně pro jistotu přehazuju ještě přímo na místě!!!
generate_predictor_raster <- FALSE
generate_bias_raster <- FALSE # pokud je TRUE, tak jen generuje bias rastery a vše ostatní modelování přeskočí - nutné volat celkově (ne s parametrem pro rozdělení na 4 skupiny druhů!!!)
adjust <- 0.65
generate_bias_raster_version <- paste0("scottIso-adj-", format(round(adjust, 2), nsmall = 2)) # "scottIso-adj0.1"
use_bias <- FALSE
trans_coords <- FALSE # když mám předem uložené přetransformované souřadnice, můžu dát FALSE, šetří to čas, musím mít ale vygenerovaný předem celý rozsah druhů (100-70000)
export_suitability_raster <- FALSE
export_pa_raster <- FALSE
eval <- FALSE # 1) bez evaluace si fituji bias rastery, 2) s evaluací dělám konečné modely
use_fitted_bias <- FALSE
do_vip <- FALSE # s Maxentem trvá extrémně dlouho, nereálné časy, možná jen nevhodná implementace v ENMTools? (vyhodnocuje se při eval TRUE)
env_breadth <- FALSE
do_all <- FALSE
do_pa <- FALSE
fit_gbif_crop <- TRUE # fitovat r.breadth až na výřezu ČR z GBIF dat, ne na celých středoevropských datech

###################################################################################
# library(devtools)
# install_local("/home/petr/Documents/github/enmtools/ENMTools", force = TRUE, build=TRUE)
# install_github("danlwarren/ENMTools", force = TRUE)
# install_github("PetrBalej/ENMTools", force = TRUE, ref="pb")
# install_local("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/ENMToolsPB", force = TRUE, build=TRUE)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#  Rscript "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/enmtoolsN.R" 1
#
#  "C:\Program Files\R\R-4.0.5\bin\x64\Rscript.exe" "G:\balej\rgee\R\export_raster\enmtoolsN.R" 1
#  Rtools https://cran.r-project.org/bin/windows/Rtools/
#  source("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/enmtoolsN.R", encoding = "UTF-8")
#  source("D:/balej/rgee/R/export_raster/enmtoolsN.R", encoding = "UTF-8")

# install_github("jamiemkass/ENMeval", force = TRUE, ref="Version-0.3.1") ### nutná tato verze pro funkčnost v ENMToolsPB!!!!!

# Nakopirovani struktury adresaru
# xcopy "K:\Downloads\rgee2\vse-v-jednom" "C:\Users\petr\Downloads\vse-v-jednom" /t /e

###################################################################################

cmd_arg <- commandArgs(trailingOnly = TRUE)
if (is.na(cmd_arg[1])) {
    limit_min_occurences <- 100 # 100, 10000, 20000,30000
    limit_max_occurences <- 70000

    print(str(cmd_arg[1]))
    print("nepředán žádný argument")
    cmd_arg_str <- 0
} else {
    #  s12 <- ptaci_intersect_distinct$count
    #  intervals <- unique(cut(s12, breaks = quantile(s12, seq(0, 1, l=13)), include.lowest = TRUE,dig.lab=5))
    #  intervals.l <- c(substring(str_extract(intervals, regex("[\\(|\\[]\\d+")),2), 70000)
    #  sort( as.numeric(intervals.l))
    # 123   341   649   1161  2013  2686  3551  4655  6265  8346  11697 18360 70000
    # 123   204   341   512   649   901  1161  1432  2013  2350  2686  3207  3551  4165  4655  5597  6265  7096  8346  9509 11697 14828 18360 22399 70000

    print("předán argument")
    print(cmd_arg[1])
    cmd_arg_str <- cmd_arg[1]
    if (alg == "glm" | alg == "gam" | alg == "maxent") {
        # if (cmd_arg[1] == 1) {
        #     limit_min_occurences <- 100 #
        #     limit_max_occurences <- 1900
        # }
        # if (cmd_arg[1] == 2) {
        #     limit_min_occurences <- 1901 #
        #     limit_max_occurences <- 5600
        # }
        # if (cmd_arg[1] == 3) {
        #     limit_min_occurences <- 5601 #
        #     limit_max_occurences <- 11000
        # }
        # if (cmd_arg[1] == 4) {
        #     limit_min_occurences <- 11001 #
        #     limit_max_occurences <- 70000
        # }

        # 123   341   649   1161  2013  2686  3551  4655  6265  8346  11697 18360 70000
        if (cmd_arg[1] == 1) {
            limit_min_occurences <- 100 # 25
            limit_max_occurences <- 400
        }
        if (cmd_arg[1] == 2) {
            limit_min_occurences <- 401 # 24
            limit_max_occurences <- 900
        }
        if (cmd_arg[1] == 3) {
            limit_min_occurences <- 901 # 22
            limit_max_occurences <- 1460
        }
        if (cmd_arg[1] == 4) {
            limit_min_occurences <- 1461 # 21
            limit_max_occurences <- 2400
        }
        if (cmd_arg[1] == 5) {
            limit_min_occurences <- 2401 # 20
            limit_max_occurences <- 3350
        }
        if (cmd_arg[1] == 6) {
            limit_min_occurences <- 3351 # 19
            limit_max_occurences <- 4300
        }
        if (cmd_arg[1] == 7) {
            limit_min_occurences <- 4301 # 19
            limit_max_occurences <- 5700
        }
        if (cmd_arg[1] == 8) {
            limit_min_occurences <- 5701 # 18
            limit_max_occurences <- 7200
        }
        if (cmd_arg[1] == 9) {
            limit_min_occurences <- 7201 # 18
            limit_max_occurences <- 9400
        }
        if (cmd_arg[1] == 10) {
            limit_min_occurences <- 9401 # 17
            limit_max_occurences <- 12500
        }
        if (cmd_arg[1] == 11) {
            limit_min_occurences <- 12501 # 17
            limit_max_occurences <- 20000
        }
        if (cmd_arg[1] == 12) {
            limit_min_occurences <- 20001 # 16
            limit_max_occurences <- 70000
        }


        # # 123   204   341   512   649   901  1161  1432  2013  2350  2686  3207  3551  4165  4655  5597  6265  7096  8346  9509 11697 14828 18360 22399 70000
        # if (cmd_arg[1] == 1) {
        #     limit_min_occurences <- 100 #
        #     limit_max_occurences <- 204
        # }
        # if (cmd_arg[1] == 2) {
        #     limit_min_occurences <- 205 #
        #     limit_max_occurences <- 341
        # }
        # if (cmd_arg[1] == 3) {
        #     limit_min_occurences <- 342 #
        #     limit_max_occurences <- 512
        # }
        # if (cmd_arg[1] == 4) {
        #     limit_min_occurences <- 513 #
        #     limit_max_occurences <- 649
        # }
        # if (cmd_arg[1] == 5) {
        #     limit_min_occurences <- 650 #
        #     limit_max_occurences <- 901
        # }
        # if (cmd_arg[1] == 6) {
        #     limit_min_occurences <- 902 #
        #     limit_max_occurences <- 1161
        # }
        # if (cmd_arg[1] == 7) {
        #     limit_min_occurences <- 1162 #
        #     limit_max_occurences <- 1432
        # }
        # if (cmd_arg[1] == 8) {
        #     limit_min_occurences <- 1433 #
        #     limit_max_occurences <- 2013
        # }
        # if (cmd_arg[1] == 9) {
        #     limit_min_occurences <- 2014 #
        #     limit_max_occurences <- 2350
        # }
        # if (cmd_arg[1] == 10) {
        #     limit_min_occurences <- 2351 #
        #     limit_max_occurences <- 2686
        # }
        # if (cmd_arg[1] == 11) {
        #     limit_min_occurences <- 2687 #
        #     limit_max_occurences <- 3207
        # }
        # if (cmd_arg[1] == 12) {
        #     limit_min_occurences <- 3208 #
        #     limit_max_occurences <- 3551
        # }
        # if (cmd_arg[1] == 13) {
        #     limit_min_occurences <- 3552 #
        #     limit_max_occurences <- 4165
        # }
        # if (cmd_arg[1] == 14) {
        #     limit_min_occurences <- 4166 #
        #     limit_max_occurences <- 4655
        # }
        # if (cmd_arg[1] == 15) {
        #     limit_min_occurences <- 4656 #
        #     limit_max_occurences <- 5597
        # }
        # if (cmd_arg[1] == 16) {
        #     limit_min_occurences <- 5598 #
        #     limit_max_occurences <- 6265
        # }
        # if (cmd_arg[1] == 17) {
        #     limit_min_occurences <- 6266 #
        #     limit_max_occurences <- 7096
        # }
        # if (cmd_arg[1] == 18) {
        #     limit_min_occurences <- 7097 #
        #     limit_max_occurences <- 8346
        # }
        # if (cmd_arg[1] == 19) {
        #     limit_min_occurences <- 8347 #
        #     limit_max_occurences <- 9509
        # }
        # if (cmd_arg[1] == 20) {
        #     limit_min_occurences <- 9510 #
        #     limit_max_occurences <- 11697
        # }
        # if (cmd_arg[1] == 21) {
        #     limit_min_occurences <- 11698 #
        #     limit_max_occurences <- 14828
        # }
        # if (cmd_arg[1] == 22) {
        #     limit_min_occurences <- 14829 #
        #     limit_max_occurences <- 18360
        # }
        # if (cmd_arg[1] == 23) {
        #     limit_min_occurences <- 18361 #
        #     limit_max_occurences <- 22399
        # }
        # if (cmd_arg[1] == 24) {
        #     limit_min_occurences <- 22400 #
        #     limit_max_occurences <- 70000
        # }
    }
    # if (alg == "maxent") {
    #     if (cmd_arg[1] == 1) {
    #         limit_min_occurences <- 100 #
    #         limit_max_occurences <- 1800
    #     }
    #     if (cmd_arg[1] == 2) {
    #         limit_min_occurences <- 1801 #
    #         limit_max_occurences <- 4500
    #     }
    #     if (cmd_arg[1] == 3) {
    #         limit_min_occurences <- 4501 #
    #         limit_max_occurences <- 10000
    #     }
    #     if (cmd_arg[1] == 4) {
    #         limit_min_occurences <- 10001 #
    #         limit_max_occurences <- 70000
    #     }
    # }
}


# pomocné funkce
source(paste0(wd, "/R/export_raster/functions.R"))
source(paste0(wd, "/R/export_raster/ndop_top.R"))
source(paste0(wd, "/R/export_raster/sedi.R"))

# sběrná proměnná pro výsledky
enmsr <- list()
# sběrné proměnné pro ideální biasy
fm_gbif_f_i_c <- list()
fm_ndop_f_i_c <- list()
fm_all_f_i_c <- list()
fm_gbif_f_i_c.t <- list()
fm_ndop_f_i_c.t <- list()
fm_all_f_i_c.t <- list()
fm_gbif_f_i_c.t.c <- list()
fm_ndop_f_i_c.t.c <- list()
fm_all_f_i_c.t.c <- list()
# fm_gbif_f_i_cSD <- list()
# fm_ndop_f_i_cSD <- list()
# fm_all_f_i_cSD <- list()

rcrs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# shapefiles
# BB (+ČR)
blocks <- st_read(paste0(wd, "/shp/blocks.shp"))
# BB - ČR
blocks_erased_cz <- st_read(paste0(wd, "/shp/blocks_erased_cz.shp"))
# ČR
czechia <- st_read(paste0(wd, "/shp/ne_10m_admin_0_countries/czechia/cz_3035.shp"))

countries <- st_read(paste0(wd, "/shp/ne_10m_admin_0_countries/ne_10m_admin_0_countries_3035.shp"))

places <- st_read(paste0(wd, "/shp/ne_10m_populated_places_simple/ne_10m_populated_places_simple_3035.shp"))
places_cz <- st_read(paste0(wd, "/shp/ne_10m_populated_places_simple/ne_10m_populated_places_simple_3035_cz.shp"))

rivers <- st_read(paste0(wd, "/shp/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines_3035.shp"))
rivers_cz <- st_read(paste0(wd, "/shp/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines_3035_cz.shp"))

# sjednocení CRS - preventivní
blocks_3035 <- blocks %>% st_transform(rcrs)
blocks_erased_cz_3035 <- blocks_erased_cz %>% st_transform(rcrs)
czechia_3035 <- czechia %>% st_transform(rcrs)

countries_3035 <- countries %>% st_transform(rcrs)

places_3035 <- places %>% st_transform(rcrs)
places_cz_3035 <- places_cz %>% st_transform(rcrs)

rivers_3035 <- rivers %>% st_transform(rcrs)
rivers_cz_3035 <- rivers_cz %>% st_transform(rcrs)

# reálně se tím řídit nemusím, neodpovídá to reálnému počtu záznamů použitých do modelů, jen pomocná metrika?
# ndop_top <- ndop_top(paste0(getwd(), "/rgee/species/ndop/ndop-top-2021-03-21.xlsx"))
# species <- ndop_top %>%
#     dplyr::select(species1_, species2_, species1, species2, species1_short, species2_short, Nálezů) %>%
#     filter(Nálezů < limit_max_occurences_ndop_top)


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
    "Lyrurus tetrix" = "Tetrao tetrix",
    "Chlidonias hybrida" = "Chlidonias hybridus"
)

### nalezy_start###
# příprava nálezových dat (další krok)
# pokud nebudu i dynamicky měnit přesost nálezů podle pixelu, může zůstat zde
# je zde pro možnost navázání na pixel size, momentálně na pevno na 300m přesnost pro zjědnodušení - pokud bych umožnil více nálezů při větších pixelech , modely by nebyly napříč porovnatelné (zavléklo by to tam další možný faktor který by mohl mít vliv na výsledky)

if (!exists("plot_ndop_csv") & !exists("plot_gbif_csv")) {
    # prepare_occurrences_thin_load_csv.R
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
} else {
    cci_gbif_3035 <- readRDS(paste0(export_path, "/inputs/occurrences/cci_gbif_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species) # netřeba filtrovat zde, stejně druh vybírám znovu v cyklu nad druhy
    cci_ndop_3035 <- readRDS(paste0(export_path, "/inputs/occurrences/cci_ndop_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species)
    cci_all_3035 <- readRDS(paste0(export_path, "/inputs/occurrences/cci_all_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species)
}


# uložení všech výstupů
# enms <- list()
start_time <- Sys.time()
for (px_size_item in px_size) {
    pres <- paste0(alg, pref, px_size_item, cmd_arg_str) # pres <- paste0(px_size_item, "_", pref, "_", alg, "_", cmd_arg_str)

    if (eval == TRUE & use_fitted_bias == TRUE & generate_bias_raster == FALSE) {
        fm_gbif_f_i_c <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fm_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
        fm_ndop_f_i_c <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fm_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
        fm_all_f_i_c <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fm_all_", px_size_item, "-", cmd_arg_str, ".rds"))
        # fm_gbif_f_i_cSD <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fmSD_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
        # fm_ndop_f_i_cSD <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fmSD_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
        # fm_all_f_i_cSD <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fmSD_all_", px_size_item, "-", cmd_arg_str, ".rds"))
    }

    # # původní načítání rasterů prediktorů, dočasná optimalizace aby se nemusel pokaždé skrz propisovat NA hodnoty
    if (generate_predictor_raster == TRUE) {
        rasters_path <- paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/", px_size_item, "/")
        vif5 <- c(

            # OWNH - hclust 1000m pro Evropu , zakomentované po redukci na 10 clusterů
            # paste0("l8_9-11_", px_size_item, "_B10"),
            paste0("l8_6-8_", px_size_item, "_B5"),
            paste0("l8_3-5_", px_size_item, "_B4"),
            paste0("l8_3-5_", px_size_item, "_B5"),
            paste0("l8_6-8_", px_size_item, "_MNDWI"), # XXX změnit na letní (původně jarní) kvůli artefaktům ze sněhu a okrajů ledovců na horách
            paste0("l8_3-5_", px_size_item, "_NDVI"),
            # paste0("l8_6-8_", px_size_item, "_NDWI"),
            paste0("wc_", px_size_item, "_bio03"),
            # paste0("wc_", px_size_item, "_bio04"), # navíc - vyšla z lokálu
            paste0("wc_", px_size_item, "_bio08"), # kolinear v CR, vyloucit?, radeji bio04, ta si vedle 03 vedla vzdycky dobre?
            paste0("wc_", px_size_item, "_bio09"),
            paste0("wc_", px_size_item, "_bio13"),
            paste0("wc_", px_size_item, "_bio15")
        )

        vif5sapply <- lapply(vif5, function(x, rasters_path) {
            return(paste0(rasters_path, x, ".tif"))
        }, rasters_path = rasters_path)
        raster_stack <- stack(lapply(vif5sapply, raster::raster))

        raster_stack_ce <- crop(raster_stack, extent(blocks_3035))
        raster_stack_mask_ce <- mask(raster_stack_ce, blocks_3035)
        raster_stack_mask_ce <- setMinMax(raster_stack_mask_ce)
        raster_stack <- raster_stack_mask_ce
        # oprava rasterů ve stacku
        raster_stack <- stack_NA_repair(raster_stack)

        rr <- writeRaster(raster_stack, paste0(export_path, "/inputs/predictors/central-europe3-", px_size_item, ".grd"), format = "raster")
        hdr(rr, format = "ENVI")

        # # ořez původního raster_stack na skutečný extent - nebude to brzdit??? - předořezat
        # raster_stack_ce <- crop(raster_stack, extent(blocks_3035))
        # raster_stack_mask_ce <- mask(raster_stack_ce, blocks_3035)
        # raster_stack_mask_ce <- setMinMax(raster_stack_mask_ce)
        # raster_stack <- raster_stack_mask_ce
        # # oprava rasterů ve stacku
        # raster_stack <- stack_NA_repair(raster_stack)
        # rr <- writeRaster(raster_stack, paste0(export_path, "/inputs/predictors/central-europe3-", px_size_item, ".grd"), format = "raster")
        # hdr(rr, format = "ENVI")

        # ořez původního raster_stack na ČR pro lokální SDM
        raster_stack_crop <- crop(raster_stack, extent(czechia_3035))
        raster_stack_mask_czechia <- mask(raster_stack_crop, czechia_3035)
        raster_stack_mask_czechia <- setMinMax(raster_stack_mask_czechia)
        raster_stack_mask_czechia <- stack_NA_repair(raster_stack_mask_czechia)
        rr <- writeRaster(raster_stack_mask_czechia, paste0(export_path, "/inputs/predictors/czechia3-", px_size_item, ".grd"), format = "raster")
        hdr(rr, format = "ENVI")
    } else {
        raster_stack <- stack(paste0(export_path, "/inputs/predictors/central-europe3-", px_size_item, ".grd"))
        raster_stack_mask_czechia <- stack(paste0(export_path, "/inputs/predictors/czechia3-", px_size_item, ".grd"))
    }

    rcrs <- crs(raster_stack)

    # sjednocení CRS - dodatečná úprava CRS aby bylo jednotné
    blocks_3035 <- blocks %>% st_transform(rcrs)
    blocks_erased_cz_3035 <- blocks_erased_cz %>% st_transform(rcrs)
    czechia_3035 <- czechia %>% st_transform(rcrs)

    countries_3035 <- countries %>% st_transform(rcrs)

    places_3035 <- places %>% st_transform(rcrs)
    places_cz_3035 <- places_cz %>% st_transform(rcrs)

    rivers_3035 <- rivers %>% st_transform(rcrs)
    rivers_cz_3035 <- rivers_cz %>% st_transform(rcrs)

    # chci alespoň poměrově určitou část plochy jako background, ne fixních 10000, které nemusí stačit
    nback_all <- round(ncell(raster_stack[[1]]))
    nback_ndop <- round(ncell(raster_stack_mask_czechia[[1]]))
    nback_all_10 <- round(ncell(raster_stack[[1]]) / 10)
    nback_ndop_10 <- round(ncell(raster_stack_mask_czechia[[1]]) / 10)
    if (nback_all > 10000) {
        if (nback_all_10 < 10000) {
            nback_all <- 10000
        } else {
            nback_all <- nback_all_10
        }
    }
    if (nback_ndop > 10000) {
        if (nback_ndop_10 < 10000) {
            nback_ndop <- 10000
        } else {
            nback_ndop <- nback_ndop_10
        }
    }

    # nad 100000 BG nejdu, trvá extrémně dlouho
    if (nback_all > 100000) {
        nback_all <- 100000
    }
    if (nback_ndop > 100000) {
        nback_ndop <- 100000
    }

    if (use_bias == TRUE) {
        if (generate_bias_raster == TRUE) {
            # gbif+all extent
            ext <- extent(raster_stack[[1]])
            ow <- owin(xrange = c(ext@xmin, ext@xmax), yrange = c(ext@ymin, ext@ymax))
            # ndop extent
            ext_ndop <- extent(raster_stack_mask_czechia[[1]])
            ow_ndop <- owin(xrange = c(ext_ndop@xmin, ext_ndop@xmax), yrange = c(ext_ndop@ymin, ext_ndop@ymax))


            print("GBIF")
            gbif_coords <- st_coordinates(cci_gbif_3035)
            gbif_ppp <- ppp(gbif_coords[, 1], gbif_coords[, 2], window = ow)
            print("ALL")
            all_coords <- st_coordinates(cci_all_3035)
            all_ppp <- ppp(all_coords[, 1], all_coords[, 2], window = ow)
            print("NDOP")
            ndop_coords <- st_coordinates(cci_ndop_3035)
            ndop_ppp <- ppp(ndop_coords[, 1], ndop_coords[, 2], window = ow_ndop)


            adj_prep <- seq(0.05, 1.00, by = 0.10)
            # adj_prep <- adj_prep[round((adj_prep * 10), digits = 3) %% 0.5 != 0] # XXX jen dočasná úprava aby se negenerovaly znovu tytéž adjusty, možno pak odstranit


            for (adjust in adj_prep) {
                generate_bias_raster_version <- paste0("scottIso-adj-", format(round(adjust, 2), nsmall = 2)) # "scottIso-adj0.1"

                # ppp <- rescale(ppp, 10, "km")
                # https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/density.ppp
                bias_gbif <- resample(raster(density.ppp(gbif_ppp, sigma = bw.scott.iso(gbif_ppp), adjust = adjust)), raster_stack[[1]], method = "bilinear") # sigma = bw.scott.iso(gbif_ppp), adjust=0.5)

                r.min <- minValue(bias_gbif)
                r.max <- maxValue(bias_gbif)
                bias_gbif <- ((bias_gbif - r.min) / (r.max - r.min))
                crs(bias_gbif) <- rcrs

                # # https://rdrr.io/cran/spatialEco/man/sp.kde.html # chcípne na nedostatek paměti (už při 1000m)
                # pt.kde <- sp.kde(x = as_Spatial(cci_gbif_3035), bw = 10000, newdata = raster_stack[[1]], standardize = TRUE,  nr=xres(raster_stack[[1]]), nc=yres(raster_stack[[1]]) ) # , scale.factor = 10000
                # dens <- kde2d(gbif_coords[,1], gbif_coords[,2], n = c(nrow(raster_stack[[1]]), ncol(raster_stack[[1]])))  # chcípne na nedostatek paměti (už při 1000m)
                # https://vita.had.co.nz/papers/density-estimation.pdf

                writeRaster(bias_gbif, paste0(export_path, "/inputs/bias_rasters2/Xbias_gbif-density-", generate_bias_raster_version, "-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

                # gbif_coords <- st_coordinates(cci_gbif_3035)
                # gbif_dens <- bkde2D(gbif_coords[, ncol(gbif_coords):1], bandwidth = c(dpik(gbif_coords[, 1], gridsize = nrow(raster_stack[[1]])), dpik(gbif_coords[, 2], gridsize = ncol(raster_stack[[1]]))), gridsize = c(nrow(raster_stack[[1]]), ncol(raster_stack[[1]])))
                # bias_gbif <- raster::flip(raster(gbif_dens$fhat), direction = "y")
                # extent(bias_gbif) <- extent(raster_stack[[1]])
                # crs(bias_gbif) <- rcrs
                # r.min <- minValue(bias_gbif)
                # r.max <- maxValue(bias_gbif)
                # bias_gbif <- ((bias_gbif - r.min) / (r.max - r.min))
                # bias_gbif <- raster::setMinMax(bias_gbif)
                # writeRaster(bias_gbif, paste0(export_path, "/inputs/bias_rasters2/Xbias_gbif-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)


                # ppp <- rescale(ppp, 10, "km")
                bias_all <- resample(raster(density.ppp(all_ppp, sigma = bw.scott.iso(all_ppp), adjust = adjust)), raster_stack[[1]], method = "bilinear")
                r.min <- minValue(bias_all)
                r.max <- maxValue(bias_all)
                bias_all <- ((bias_all - r.min) / (r.max - r.min))
                crs(bias_all) <- rcrs
                writeRaster(bias_all, paste0(export_path, "/inputs/bias_rasters2/Xbias_all-density-", generate_bias_raster_version, "-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

                # ppp <- rescale(ppp, 10, "km")
                bias_ndop <- resample(raster(density.ppp(ndop_ppp, sigma = bw.scott.iso(ndop_ppp), adjust = adjust)), raster_stack_mask_czechia[[1]], method = "bilinear")
                r.min <- minValue(bias_ndop)
                r.max <- maxValue(bias_ndop)
                bias_ndop <- ((bias_ndop - r.min) / (r.max - r.min))
                crs(bias_ndop) <- rcrs
                writeRaster(bias_ndop, paste0(export_path, "/inputs/bias_rasters2/Xbias_ndop-density-", generate_bias_raster_version, "-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)
            }

            next
        } else {
            bias_gbif <- raster(paste0(export_path, "/inputs/bias_rasters2/Xbias_gbif-density-", generate_bias_raster_version, "-", px_size_item, ".tif"))
            bias_ndop <- NA
            bias_all <- NA
        }
    } else {
        bias_gbif <- NA
        bias_ndop <- NA
        bias_all <- NA
    }

    if (generate_bias_raster == TRUE) {
        # pokud generuju bias rastery, tak si je jen připravuji a přeskočím modelování
        next
    }


    # obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků

    ptaci_intersect_distinct <- ptaci_ndop_distinct %>%
        filter(species %in% ptaci_gbif_distinct$species) # %>% filter(species != "Tichodroma muraria" & px_size_item != 10000)
    # %>% filter(species == "Larus argentatus")
    # %>% filter(species == "Emberiza calandra")
    # %>% filter(species == "Podiceps grisegena")
    # %>% filter(species == "Aquila chrysaetos")
    # %>% filter(species == "Lanius collurio")
    # %>% filter(species == "Hydroprogne caspia")

    species <- rev(ptaci_intersect_distinct$species) # přepisuju původní seznam z ndop_top
    species_time <- Sys.time()
    for (sp in species) { # sp in ptaci_gbif_distinct$species
        # foreach(sindex = 1:nrow(species), .combine=combine, .packages=c('dismo', "rJava")) %dopar% {
        # species1_, species2_, species1, species2, species1_short, species2_short, Nálezů

        print("********************************************************** ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        print(sp)
        print(paste0(which(species == sp), ". z ", length(species)))
        print("celkem od startu: ")
        print(Sys.time() - start_time)
        print("předchozí druh trval: ")
        print(Sys.time() - species_time)
        species_time <- Sys.time()


        gbif_f <- cci_gbif_3035 %>% filter(species == as.character(sp))
        ndop_f <- cci_ndop_3035 %>% filter(species == as.character(sp))
        all_f <- cci_all_3035 %>% filter(species == as.character(sp))

        # sp_gbif_count <- ptaci_gbif_distinct %>%
        #     filter(species == as.character(sp)) %>%
        #     dplyr::select(count)
        # sp_ndop_count <- ptaci_ndop_distinct %>%
        #     filter(species == as.character(sp)) %>%
        #     dplyr::select(count)

        # preventivní ořezy a spojení obou datasetů
        # cci_gbif_3035 <- st_intersection(cc_gbif_3035, blocks_3035)
        # cci_ndop_3035 <- st_intersection(cc_ndop_3035, czechia_3035)
        ndop_f_n <- nrow(ndop_f)
        gbif_f_n <- nrow(gbif_f)
        f_n <- ndop_f_n * 100 / gbif_f_n

        # GBIF má cca 3.4M nálezů, NDOp cca 1.5 (poměr 0.44) - stejný poměr musí být cca dodržován kvůli použití bias rasterů u ALL varianty, které jsou tímto poměrem zkonstruované!
        # opačně to je 2.27
        ndop_all_fraction <- round(gbif_f_n * 0.44)
        gbif_all_fraction <- round(ndop_f_n * 2.27)
        all2_f <- list()
        all2_f[[1]] <- all_f
        fraction_used <- "X"
        if (ndop_f_n > ndop_all_fraction) {
            fraction_used <- "NDOP"
            # musím omezit počet záznamů v NDOP na 44% GBIFu
            if (replicates > 1) {
                for (r in 1:replicates) {
                    # vhodné pro každou replikaci jiný náhodný vzorek z ndop
                    all2_f[[r]] <- bind_rows(gbif_f, ndop_f %>% sample_n(ndop_all_fraction))
                }
            } else {
                all2_f[[1]] <- bind_rows(gbif_f, ndop_f %>% sample_n(ndop_all_fraction))
            }
        } else {
            fraction_used <- "XGBIF"
            # jen pojistka, kdyby náhdou zaokrouhlením nastaly oba případy...
            if (gbif_f_n > gbif_all_fraction) {
                fraction_used <- "GBIF"
                # musím omezit počet záznamů v GBIF na 227% NDOPu
                if (replicates > 1) {
                    for (r in 1:replicates) {
                        # vhodné pro každou replikaci jiný náhodný vzorek z ndop
                        all2_f[[r]] <- bind_rows(ndop_f, gbif_f %>% sample_n(gbif_all_fraction))
                    }
                } else {
                    all2_f[[1]] <- bind_rows(ndop_f, gbif_f %>% sample_n(gbif_all_fraction))
                }
            }
        }



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
        colnames(enm_mxt_gbif.pp.orig)[1] <- "Longitude"
        colnames(enm_mxt_gbif.pp.orig)[2] <- "Latitude"
        # zaokrouhlení (optimalizace), aby se vytvářelo méně bufferů
        enm_mxt_gbif.pp <- unique(round_df(enm_mxt_gbif.pp.orig, -4)[c("Longitude", "Latitude")])

        enm_mxt_ndop.pp.orig <- as.data.frame(st_coordinates(ndop_f))
        colnames(enm_mxt_ndop.pp.orig)[1] <- "Longitude"
        colnames(enm_mxt_ndop.pp.orig)[2] <- "Latitude"
        # zaokrouhlení (optimalizace), aby se vytvářelo méně bufferů
        enm_mxt_ndop.pp <- unique(round_df(enm_mxt_ndop.pp.orig, -4)[c("Longitude", "Latitude")])


        # # odstranění osamocených nálezů, symbolickou podmínkou je alespoň skupina 1-2 ("k") nálezů v okolí 100 km, aby nešlo o náhodný nález
        ext <- extent(raster_stack[[1]])
        ow <- owin(xrange = c(ext@xmin, ext@xmax), yrange = c(ext@ymin, ext@ymax))
        all_coords <- st_coordinates(all_f)
        all_ppp <- ppp(all_coords[, 1], all_coords[, 2], window = ow)
        d <- nndist(all_ppp, k = 2)
        if (!is.empty(which(d > 100000))) {
            all_f <- all_f[-which(d > 100000), ]
        }


        enm_mxt_all.pp.orig <- as.data.frame(st_coordinates(all_f))
        colnames(enm_mxt_all.pp.orig)[1] <- "Longitude"
        colnames(enm_mxt_all.pp.orig)[2] <- "Latitude"
        # zaokrouhlení (optimalizace), aby se vytvářelo méně bufferů
        enm_mxt_all.pp <- unique(round_df(enm_mxt_all.pp.orig, -4)[c("Longitude", "Latitude")])


        enm_mxt_all.pp.orig <- list()
        # u ALL musím ošetřit odpovídající počet 44% nálezů z NDOP
        for (r in 1:replicates) {
            # vhodné pro každou replikaci jiný náhodný vzorek z ndop
            enm_mxt_all.pp.orig[[r]] <- as.data.frame(st_coordinates(all2_f[[r]]))
            colnames(enm_mxt_all.pp.orig[[r]])[1] <- "Longitude"
            colnames(enm_mxt_all.pp.orig[[r]])[2] <- "Latitude"
        }


        # jen ČR NDOP i GBIF
        local.pp.orig <- as.data.frame(st_coordinates(st_intersection(all_f, st_transform(czechia_3035, st_crs(all_f)))))

        colnames(local.pp.orig)[1] <- "Longitude"
        colnames(local.pp.orig)[2] <- "Latitude"
        # zaokrouhlení (optimalizace), aby se vytvářelo méně bufferů
        local.pp <- unique(round_df(local.pp.orig, -4)[c("Longitude", "Latitude")])


        # background.buffer(points = enm_mxt_all.pp, buffer.width = 50000, buffer.type = "circles", return.type = "polygon", n = 10000)
        # - nefunguje ani v cran verzi ani z githubu: could not find function "background.buffer"
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

        # příprava occurences - ořezání pixely výsledného rastru prediktorů - nutno takto, ořezání polygonem nestačí (aby mi do modelu nevstupovaly nálezy které leží v NA oblastech)!
        # nedělám pro all!!! (zatím nepotřebuju) - ale měl bych s tím později také dále počítat při určování poměru GBIF/NDOP pro ALL
        # zatím toto ENMTools neumí...
        # GBIF
        spatialPoints.gbif <- extract(buffer.global, as(gbif_f, "Spatial"))
        gbif_f.f <- gbif_f[which(spatialPoints.gbif == 1), ]
        gbif_f.f <- st_as_sf(gbif_f.f)
        enm_mxt_gbif.pp.orig <- as.data.frame(st_coordinates(gbif_f.f))
        colnames(enm_mxt_gbif.pp.orig)[1] <- "Longitude"
        colnames(enm_mxt_gbif.pp.orig)[2] <- "Latitude"
        # NDOP
        spatialPoints.ndop <- extract(buffer.local, as(ndop_f, "Spatial"))
        ndop_f.f <- ndop_f[which(spatialPoints.ndop == 1), ]
        ndop_f.f <- st_as_sf(ndop_f.f)
        enm_mxt_ndop.pp.orig <- as.data.frame(st_coordinates(ndop_f.f))
        colnames(enm_mxt_ndop.pp.orig)[1] <- "Longitude"
        colnames(enm_mxt_ndop.pp.orig)[2] <- "Latitude"

        ndop_f_n.f <- nrow(ndop_f.f)
        gbif_f_n.f <- nrow(gbif_f.f)

        # pro NDOP ještě prostorově dofiltruju 100m pixelem a posunu souřadnice do jeho středu (+50)
        enm_mxt_ndop.pp.orig <- unique(floor_df(enm_mxt_ndop.pp.orig, -2)[c("Longitude", "Latitude")]) + 50
        ndop_f_n_thin <- nrow(enm_mxt_ndop.pp.orig)

        # # # zbytečné, nakonec se stejně znovu provede v check.bg() v enmtools.glm() pokud jsou takto zamaskované env prediktory
        # bias_gbif_b <- mask(bias_gbif, buffer.global)
        # bias_all_b <- mask(bias_all, buffer.global)
        # bias_ndop_b <- mask(bias_ndop, buffer.local)
        # bias_gbif_b <- setMinMax(bias_gbif_b)
        # bias_all_b <- setMinMax(bias_all_b)
        # bias_ndop_b <- setMinMax(bias_ndop_b)

        ###
        ### příprava druhů
        ###
        enm_mxt_gbif.s <- enmtools.species(
            range = buffer.global, # raster_stack_b[[1]],
            species.name = as.character(sp), presence.points = enm_mxt_gbif.pp.orig
        )
        enm_mxt_ndop.s <- enmtools.species(
            range = buffer.local, # raster_stack_mask_czechia_b[[1]],
            species.name = as.character(sp), presence.points = enm_mxt_ndop.pp.orig
        )

        # u ALL musím ošetřit odpovídající počet 44% nálezů z NDOP
        enm_mxt_all.s <- list()
        if (do_all) {
            for (r in 1:replicates) {
                # vhodné pro každou replikaci jiný náhodný vzorek z ndop

                enm_mxt_all.s[[r]] <- enmtools.species(
                    range = buffer.global, # raster_stack_b[[1]],
                    species.name = as.character(sp), presence.points = enm_mxt_all.pp.orig[[r]]
                )
            }
        }

        if (eval == FALSE) {
            test.prop.fit <- 0.1 # pro jistotu manuální přepis proměnné, nasazení podílu testovacích nálezů se totiž mění při jednotlivých adjustech -> nestabilní
            replicates.fit <- 5
            gc()
            # intervals <- c(
            #     c(0.05, 0.15, 0.30, 0.40, 0.50),
            #     seq(0.55, 1.00, by = 0.05),
            #     seq(1.10, 1.50, by = 0.1),
            #     seq(2.05, 8.05, by = 1.00)
            # )
            # intervals <- c(0.7, 0.75, 0.79, 0.85, 1.1, 1.7)

            intervals <- c(seq(0.55, 0.65, by = 0.05), seq(0.67, 0.83, by = 0.02), seq(0.85, 0.95, by = 0.05)) # 75 modelů po 5

            if (px_size_item == 5000 & alg == "glm") {
                intervals <- c(seq(0.45, 0.55, by = 0.05), seq(0.57, 0.73, by = 0.02), seq(0.75, 0.95, by = 0.05)) # 85 modelů po 5
            }

            if (px_size_item == 5000 & alg == "maxent") {
                intervals <- c(seq(0.45, 0.55, by = 0.05), seq(0.57, 0.83, by = 0.02), seq(0.85, 0.95, by = 0.05)) # 85 modelů po 5
            }

            if (px_size_item == 10000) {
                intervals <- c(seq(0.05, 0.95, by = 0.10)) # 50 modelů po 5
            }

            # výběr ideální raster.breadth, vygenerování seznamu ideálních bias rasterů pro každý druh a pixel size

            print("1) základní předvýběr s větším intervalem ------------------------------------------------------------------------------")
            # 1) základní předvýběr s větším intervalem (by)

            fm <- list()
            for (s in intervals) { # seq(0.05, 10.05, by = by)
                # počítám pouze neokrajové hodnoty
                print(paste0("scottIso-adj-", format(round(s, 2), nsmall = 2)))

                bvn <- format(round(s, 2), nsmall = 2)
                bv <- paste0("scottIso-adj-", bvn)
                ### volání / fitování modelů
                bias_gbif <- raster(paste0(export_path, "/inputs/bias_rasters2/Xbias_gbif-density-", bv, "-", px_size_item, ".tif"))
                bias_ndop <- NA
                bias_all <- NA

                fm[[bvn]] <- fit_modelsN(alg, replicates.fit, eval, test.prop.fit, enm_mxt_gbif.s, NA, NA, raster_stack_b, raster_stack_mask_czechia_b, bias_gbif, bias_ndop, bias_all, nback_all, nback_ndop, TRUE, fit_gbif_crop, czechia_3035)
            }

            fm_all <- list()
            fm_gbif <- list()
            fm_ndop <- list()

            # fm_all.md <- list()
            # fm_gbif.md <- list()
            # fm_ndop.md <- list()
            for (i in names(fm)) {
                fm_all[[i]] <- fm[[i]]$enm_mxt_all.breadth.B2
                fm_gbif[[i]] <- fm[[i]]$enm_mxt_gbif.breadth.B2
                fm_ndop[[i]] <- fm[[i]]$enm_mxt_ndop.breadth.B2
                # fm_all.md[[i]] <- fm[[i]]$enm_mxt_all.breadth.B2.md
                # fm_gbif.md[[i]] <- fm[[i]]$enm_mxt_gbif.breadth.B2.md
                # fm_ndop.md[[i]] <- fm[[i]]$enm_mxt_ndop.breadth.B2.md
            }


            # # nejednoznačné
            #             fm_allE <- list()
            #             fm_gbifE <- list()
            #             fm_ndopE <- list()
            #             for (i in names(fm)) {
            #                 enm_mxt_gbif.ebreadth <- lapply(fm[[i]]$enm_mxt_gbif, env.breadth, env = raster_stack_b) # podlední  [[1]] je počet replikací - nahradit? Budu to někdy určovat pro více replikací, asi ne?
            #                 fm_gbifE[[i]] <- median(sapply(enm_mxt_gbif.ebreadth, function(x) x$env.B2))

            #                 enm_mxt_all.ebreadth <- lapply(fm[[i]]$enm_mxt_all, env.breadth, env = raster_stack_b)
            #                 fm_allE[[i]] <- median(sapply(enm_mxt_all.ebreadth, function(x) x$env.B2))

            #                 enm_mxt_ndop.ebreadth <- lapply(fm[[i]]$enm_mxt_ndop, env.breadth, env = raster_stack_mask_czechia_b)
            #                 fm_ndopE[[i]] <- median(sapply(enm_mxt_ndop.ebreadth, function(x) x$env.B2))
            #             }


            # rychlé orientační určení vrcholu (s lepším px_size se u ndop zdá být vždy nejlepší 0.5...)
            nt_all <- as_tibble(fm_all)
            ntt_all <- as_tibble(cbind(nms = names(nt_all), t(nt_all))) %>%
                mutate(across(V2, as.numeric)) %>%
                mutate(across(nms, as.numeric)) %>%
                arrange(nms)

            # quantile - výsledný počet prvků závislý na výchozím počtu prvků!
            ntt_all_5 <- ntt_all %>% slice_max(V2, n = 1)
            # ntt_all_5[2, 1]
            # ntt_all_31 <- ntt_all_5[3,1] - ntt_all_5[1,1]
            # ntt_all_32 <- ntt_all_5[3,1] - ntt_all_5[2,1]
            # ntt_all_21 <- ntt_all_5[2,1] - ntt_all_5[1,1]

            nt_gbif <- as_tibble(fm_gbif)
            ntt_gbif <- as_tibble(cbind(nms = names(nt_gbif), t(nt_gbif))) %>%
                mutate(across(V2, as.numeric)) %>%
                mutate(across(nms, as.numeric)) %>%
                arrange(nms)

            ntt_gbif_5 <- ntt_gbif %>% slice_max(V2, n = 1)
            # ntt_gbif_5[2, 1]

            nt_ndop <- as_tibble(fm_ndop)
            ntt_ndop <- as_tibble(cbind(nms = names(nt_ndop), t(nt_ndop))) %>%
                mutate(across(V2, as.numeric)) %>%
                mutate(across(nms, as.numeric)) %>%
                arrange(nms)

            ntt_ndop_5 <- ntt_ndop %>% slice_max(V2, n = 1)
            # ntt_ndop_5[2, 1]


            all_top_adj <- ntt_all_5 %>%
                select(nms)
            gbif_top_adj <- ntt_gbif_5 %>%
                select(nms)
            ndop_top_adj <- ntt_ndop_5 %>%
                select(nms)

            # předvýběr: který adjust koeficient má nejširší (nejvyšší B2) niku (největší heterogenitu prostředí)?
            # fm_gbif_f_i_c[[as.character(px_size_item)]][[as.character(sp)]] <- fm_gbif_c <- as.numeric(ntt_gbif_5[2, 1])
            # fm_ndop_f_i_c[[as.character(px_size_item)]][[as.character(sp)]] <- fm_ndop_c <- as.numeric(ntt_ndop_5[2, 1])
            # fm_all_f_i_c[[as.character(px_size_item)]][[as.character(sp)]] <- fm_all_c <- as.numeric(ntt_all_5[2, 1])
            fm_gbif_f_i_c[[as.character(px_size_item)]][[as.character(sp)]] <- fm_gbif_c <- as.numeric(gbif_top_adj)
            fm_ndop_f_i_c[[as.character(px_size_item)]][[as.character(sp)]] <- fm_ndop_c <- as.numeric(ndop_top_adj)
            fm_all_f_i_c[[as.character(px_size_item)]][[as.character(sp)]] <- fm_all_c <- as.numeric(all_top_adj)
            fm_gbif_f_i_c.t[[as.character(px_size_item)]][[as.character(sp)]] <- ntt_gbif
            fm_ndop_f_i_c.t[[as.character(px_size_item)]][[as.character(sp)]] <- ntt_ndop
            fm_all_f_i_c.t[[as.character(px_size_item)]][[as.character(sp)]] <- ntt_all
            fm_gbif_f_i_c.t.c[[as.character(px_size_item)]][[as.character(sp)]] <- paste(round(unname(unlist(ntt_gbif)), 4), collapse = ",")
            fm_ndop_f_i_c.t.c[[as.character(px_size_item)]][[as.character(sp)]] <- paste(round(unname(unlist(ntt_ndop)), 4), collapse = ",")
            fm_all_f_i_c.t.c[[as.character(px_size_item)]][[as.character(sp)]] <- paste(round(unname(unlist(ntt_all)), 4), collapse = ",")



            png(paste0(export_path, "/outputs/png-adjust/", sp, "_", px_size_item, "_", pres, "_", replicates, "_gbif.png"))
            plot(ntt_gbif, main = paste0(sp, " | GBIF, (", (px_size_item / 1000), "km)"), sub = paste0("1st: ", gbif_top_adj))
            dev.off()
            if (do_all) {
                png(paste0(export_path, "/outputs/png-adjust/", sp, "_", px_size_item, "_", pres, "_", replicates, "_all.png"))
                plot(ntt_all, main = paste0(sp, " | ALL, (", (px_size_item / 1000), "km)"), sub = paste0("1st: ", all_top_adj))
                dev.off()
            }
            if (do_all) {
                png(paste0(export_path, "/outputs/png-adjust/", sp, "_", px_size_item, "_", pres, "_", replicates, "_ndop.png"))
                plot(ntt_ndop, main = paste0(sp, " | NDOP, (", (px_size_item / 1000), "km)"), sub = paste0("1st: ", ndop_top_adj))
                dev.off()
            }

            ## pokud generuju bias rastery, tak si je jen připravuji a přeskočím modelování
            # next
        } else {
            ###############
            ###
            ###
            ### start už nafitovaných modelů dle ideálních adjust koeficientů bias rasterů uložených v fm_*
            ###
            ###
            ###############
            print("start fitted models")
            bvn_gbif <- 99.0
            bvn_ndop <- 99.0
            bvn_all <- 99.0
            if (use_bias) {
                if (use_fitted_bias) {
                    # vyhlazení extrémně ujetých bodů plovoucím mediánem do tvaru pěknějších "křivek"
                    gbif_t <- readRDS(paste0(export_path, "/inputs/occurrences/", alg, "_fmt_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
                    sp1 <- gbif_t[[as.character(px_size_item)]][[as.character(sp)]]
                    x <- sp1$V2
                    rmed <- stats::runmed(x, 5, endrule = "median")
                    sp1.s <- sp1[which.max(rmed), ]
                    ideal_adj_gbif <- sp1.s[, 1]
                    # bvn_gbif <- format(round(fm_gbif_f_i_c[[as.character(px_size_item)]][[as.character(sp)]], 2), nsmall = 2)
                    bvn_gbif <- format(unname(unlist(round(ideal_adj_gbif, 2))), nsmall = 2)
                    bv_gbif <- paste0("scottIso-adj-", bvn_gbif)

                    bvn_ndop <- format(round(fm_ndop_f_i_c[[as.character(px_size_item)]][[as.character(sp)]], 2), nsmall = 2)
                    bv_ndop <- paste0("scottIso-adj-", bvn_ndop)

                    bvn_all <- format(round(fm_all_f_i_c[[as.character(px_size_item)]][[as.character(sp)]], 2), nsmall = 2)
                    bv_all <- paste0("scottIso-adj-", bvn_all)

                    # výběr ideálních variant bias rasterů
                    bias_gbif <- raster(paste0(export_path, "/inputs/bias_rasters2/Xbias_gbif-density-", bv_gbif, "-", px_size_item, ".tif"))
                    bias_ndop <- NA

                    bias_all <- NA
                    if (do_all) {
                        bias_all <- raster(paste0(export_path, "/inputs/bias_rasters2/Xbias_all-density-", bv_all, "-", px_size_item, ".tif"))
                    }
                } else {
                    bias_gbif <- raster(paste0(export_path, "/inputs/bias_rasters2/Xbias_gbif-density-", generate_bias_raster_version, "-", px_size_item, ".tif"))
                    bias_ndop <- NA
                    bias_all <- NA
                }
            } else {
                bias_gbif <- NA
                bias_ndop <- NA
                bias_all <- NA
            }



            ### volání fitovaných modelů
            fm <- fit_modelsN(alg, replicates, eval, test.prop, enm_mxt_gbif.s, enm_mxt_ndop.s, enm_mxt_all.s, raster_stack_b, raster_stack_mask_czechia_b, bias_gbif, NA, bias_all, nback_all, nback_ndop, FALSE, fit_gbif_crop, czechia_3035)

            enm_mxt_gbif <- fm$enm_mxt_gbif
            enm_mxt_ndop <- fm$enm_mxt_ndop
            enm_mxt_all <- fm$enm_mxt_all
            enm_mxt_gbif.breadth.B1 <- fm$enm_mxt_gbif.breadth.B1
            enm_mxt_gbif.breadth.B2 <- fm$enm_mxt_gbif.breadth.B2
            enm_mxt_ndop.breadth.B1 <- fm$enm_mxt_ndop.breadth.B1
            enm_mxt_ndop.breadth.B2 <- fm$enm_mxt_ndop.breadth.B2
            enm_mxt_all.breadth.B1 <- fm$enm_mxt_all.breadth.B1
            enm_mxt_all.breadth.B2 <- fm$enm_mxt_all.breadth.B2


            ###
            ### gbif
            ###
            print("GBIF")
            cm_gbif <- lapply(enm_mxt_gbif, function(x) performance(x$conf))
            enm_mxt_gbif.matrix <- abind(cm_gbif, along = 3)
            enm_mxt_gbif.perf <- apply(enm_mxt_gbif.matrix, c(1, 2), median)

            # thr.gbif <- median(sapply(enm_mxt_gbif, function(x) x$thr$spec_sens))

            thr.gbif <- sapply(enm_mxt_gbif, function(x) x$thr)

            sedi.gbif.p <- lapply(enm_mxt_gbif, function(x) sedi(x$conf))
            sedi.gbif <- median(sapply(sedi.gbif.p, function(x) {
                if (x[[1]] == "SEDI equal to") {
                    x[[2]]
                } else {
                    NA
                }
            }))


            enm_mxt_gbif.ebreadth <- NA
            enm_mxt_gbif.ebreadth.B2 <- NA
            if (env_breadth) {
                enm_mxt_gbif.ebreadth <- lapply(enm_mxt_gbif, env.breadth, env = raster_stack_b)
                enm_mxt_gbif.ebreadth.B2 <- median(sapply(enm_mxt_gbif.ebreadth, function(x) x$env.B2))
            }

            enm_mxt_gbif.auc <- median(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc))
            enm_mxt_gbif.np.tr <- median(sapply(enm_mxt_gbif, function(x) x$training.evaluation@np))
            enm_mxt_gbif.np.te <- median(sapply(enm_mxt_gbif, function(x) x$test.evaluation@np))
            # plot(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]][[1]]$suitability)
            # names(enm_mxt_gbif) <- paste0("rep", 1:repl)
            enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
            enm_mxt_gbif.r.m <- calc(enm_mxt_gbif.r, fun = median)
            if (export_suitability_raster) {
                writeRaster(enm_mxt_gbif.r.m, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_gbif.tif"), format = "GTiff", overwrite = TRUE)
            }
            png(paste0(export_path, "/outputs/png/", sp, "_", px_size_item, "_", pres, "_", replicates, "_gbif.png"), width = 800, height = 800)
            plot(enm_mxt_gbif.r.m,
                main = paste0(sp, " | GBIF, AUC=", round(enm_mxt_gbif.auc, digits = 2), " (", (px_size_item / 1000), "km)"),
                sub = paste0("GBIF: ", gbif_f_n.f, " | adj: ", bvn_gbif)
            )
            par(bg = NA)
            plot(rivers_3035$geometry, add = TRUE, col = "blue")
            par()
            plot(countries_3035$geometry, add = TRUE)
            par()
            plot(places_3035$geometry, add = TRUE, col = "red", pch = 0)
            par()
            points(enm_mxt_gbif.pp.orig, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
            dev.off()


            # Maximum test sensitivity plus specificity
            # pro Maxent nemůže být plogis, při cloglog ýýstupu (nebo raději dát předtím do args RAW? - je ale k dispozici v rmaxent::project(), případně v původní???)
            # použít https://rdrr.io/github/johnbaums/rmaxent/man/to_logistic.html to_logistic(x = thr.gbif.mss, from = "cloglog")
            # https://rpubs.com/mlibxmda/GEOG70922_Week5
            # back-transform these values to approximate probabilities (i.e. values ranging from 0 to 1) using the plogis()
            # The threshold values for the maxnet model relate to the raw outputs (prior to transformation). We can achieve our presence/absence map simply by applying our threshold to these raw values. To do so, we map the raw results of the model (i.e, this time we do not opt for a ‘cloglog’ or other transform) and then remove all values below our threshold.
            # Protože se v ENMTools u maxentu používá predict() bez
            if (do_pa) {
                thr.gbif.mss <- median(sapply(enm_mxt_gbif, function(x) {
                    x$test.evaluation@t[which.max(x$test.evaluation@TPR + x$test.evaluation@TNR)]
                }))
                if (alg == "glm") {
                    thr.gbif.mss <- plogis(thr.gbif.mss)
                }

                raster.gbif <- enm_mxt_gbif.r.m
                raster.gbif[raster.gbif < thr.gbif.mss] <- 0
                raster.gbif[raster.gbif >= thr.gbif.mss] <- 1
                pa.gbif.freq <- freq(raster.gbif)
                if (export_pa_raster) {
                    writeRaster(raster.gbif, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_gbif.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "INT1U")
                }
            }
            # raster::predict()  pro GLM https://rdrr.io/cran/raster/man/predict.html
            # dismo::predict() pro Maxent https://www.rdocumentation.org/packages/dismo/versions/1.3-3/topics/predict
            # ne, teď už můžu použít rovnou suitability raster...
            if (do_vip) {
                enm_mxt_gbif.vip <- sapply(enm_mxt_gbif, enmtools.vip)
            }
            # calculates Continuous Boyce Index - problematické, dělá errory
            ### enm_mxt_gbif.cal <- lapply(enm_mxt_gbif, enmtools.calibrate, env = raster_stack_b, n.background = 10000)



            ###
            ### ndop
            ###

            print("NDOP")

            cm_ndop <- lapply(enm_mxt_ndop, function(x) performance(x$conf))
            enm_mxt_ndop.matrix <- abind(cm_ndop, along = 3)
            enm_mxt_ndop.perf <- apply(enm_mxt_ndop.matrix, c(1, 2), median)

            thr.ndop <- sapply(enm_mxt_ndop, function(x) x$thr)

            sedi.ndop.p <- lapply(enm_mxt_ndop, function(x) sedi(x$conf))
            sedi.ndop <- median(sapply(sedi.ndop.p, function(x) {
                if (x[[1]] == "SEDI equal to") {
                    x[[2]]
                } else {
                    NA
                }
            }))

            enm_mxt_ndop.ebreadth <- NA
            enm_mxt_ndop.ebreadth.B2 <- NA
            if (env_breadth) {
                enm_mxt_ndop.ebreadth <- lapply(enm_mxt_ndop, env.breadth, env = raster_stack_mask_czechia_b)
                enm_mxt_ndop.ebreadth.B2 <- median(sapply(enm_mxt_ndop.ebreadth, function(x) x$env.B2))
            }


            enm_mxt_ndop.auc <- median(sapply(enm_mxt_ndop, function(x) x$test.evaluation@auc))
            enm_mxt_ndop.np.tr <- median(sapply(enm_mxt_ndop, function(x) x$training.evaluation@np))
            enm_mxt_ndop.np.te <- median(sapply(enm_mxt_ndop, function(x) x$test.evaluation@np))
            enm_mxt_ndop.r <- stack(sapply(enm_mxt_ndop, function(x) x$suitability))
            enm_mxt_ndop.r.m <- calc(enm_mxt_ndop.r, fun = median)
            if (export_suitability_raster) {
                writeRaster(enm_mxt_ndop.r.m, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_ndop.tif"), format = "GTiff", overwrite = TRUE)
            }
            png(paste0(export_path, "/outputs/png/", sp, "_", px_size_item, "_", pres, "_", replicates, "_ndop.png"), width = 800, height = 800)
            plot(enm_mxt_ndop.r.m,
                main = paste0(sp, " | NDOP, AUC=", round(enm_mxt_ndop.auc, digits = 2), " (", (px_size_item / 1000), "km)"),
                sub = paste0("NDOP: ", ndop_f_n.f, " | adj: ", bvn_ndop)
            )
            par(bg = NA)
            plot(rivers_cz_3035$geometry, add = TRUE, col = "blue")
            par()
            plot(czechia_3035$geometry, add = TRUE)
            par()
            plot(places_cz_3035$geometry, add = TRUE, col = "red", pch = 0)
            par()
            points(enm_mxt_ndop.pp.orig, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
            dev.off()

            # Maximum test sensitivity plus specificity
            if (do_pa) {
                thr.ndop.mss <- median(sapply(enm_mxt_ndop, function(x) {
                    x$test.evaluation@t[which.max(x$test.evaluation@TPR + x$test.evaluation@TNR)]
                }))
                if (alg == "glm") {
                    thr.ndop.mss <- plogis(thr.ndop.mss)
                }
                raster.ndop <- enm_mxt_ndop.r.m
                raster.ndop[raster.ndop < thr.ndop.mss] <- 0
                raster.ndop[raster.ndop >= thr.ndop.mss] <- 1

                pa.ndop.freq <- freq(raster.ndop)
                if (export_pa_raster) {
                    writeRaster(raster.ndop, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_ndop.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "LOG1S")
                }
            }
            if (do_vip) {
                enm_mxt_ndop.vip <- sapply(enm_mxt_ndop, enmtools.vip)
            }
            ### enm_mxt_ndop.cal <- lapply(enm_mxt_ndop, enmtools.calibrate, env = raster_stack_mask_czechia_b, n.background = 10000)



            ###
            ### all (ndpop+gbif)
            ###
            print("ALL")

            cm_all <- NA
            enm_mxt_all.matrix <- NA
            enm_mxt_all.perf <- NA
            thr.all <- NA
            sedi.all.p <- NA
            sedi.all <- NA
            if (do_all) {
                cm_all <- lapply(enm_mxt_all, function(x) performance(x$conf))
                enm_mxt_all.matrix <- abind(cm_all, along = 3)
                enm_mxt_all.perf <- apply(enm_mxt_all.matrix, c(1, 2), median)

                thr.all <- sapply(enm_mxt_all, function(x) x$thr)

                sedi.all.p <- lapply(enm_mxt_all, function(x) sedi(x$conf))
                sedi.all <- median(sapply(sedi.all.p, function(x) {
                    if (x[[1]] == "SEDI equal to") {
                        x[[2]]
                    } else {
                        NA
                    }
                }))
            }
            enm_mxt_all.ebreadth <- NA
            enm_mxt_all.ebreadth.B2 <- NA
            if (env_breadth) {
                enm_mxt_all.ebreadth <- lapply(enm_mxt_all, env.breadth, env = raster_stack_b)
                enm_mxt_all.ebreadth.B2 <- median(sapply(enm_mxt_all.ebreadth, function(x) x$env.B2))
            }

            enm_mxt_all.auc <- median(sapply(enm_mxt_all, function(x) x$test.evaluation@auc))
            enm_mxt_all.np.tr <- median(sapply(enm_mxt_all, function(x) x$training.evaluation@np))
            enm_mxt_all.np.te <- median(sapply(enm_mxt_all, function(x) x$test.evaluation@np))
            if (do_all) {
                enm_mxt_all.r <- stack(sapply(enm_mxt_all, function(x) x$suitability))
                enm_mxt_all.r.m <- calc(enm_mxt_all.r, fun = median)
                if (export_suitability_raster) {
                    writeRaster(enm_mxt_all.r.m, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_all.tif"), format = "GTiff", overwrite = TRUE)
                }
                png(paste0(export_path, "/outputs/png/", sp, "_", px_size_item, "_", pres, "_", replicates, "_all.png"), width = 800, height = 800)
                plot(enm_mxt_all.r.m,
                    main = paste0(sp, " | GBIF+NDOP, AUC=", round(enm_mxt_all.auc, digits = 2), " (", (px_size_item / 1000), "km)"),
                    sub = paste0("NDOP/GBIF: ", ndop_f_n.f, "/", gbif_f_n.f, " = ", round(f_n), "% (", ndop_all_fraction, "/", gbif_all_fraction, ", ", fraction_used, ")", " | adj: ", bvn_all)
                )
                par(bg = NA)
                plot(rivers_3035$geometry, add = TRUE, col = "blue")
                par()
                plot(countries_3035$geometry, add = TRUE)
                par()
                plot(places_3035$geometry, add = TRUE, col = "red", pch = 0)
                par()
                points(enm_mxt_all.pp.orig[[1]], col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3))
                dev.off()

                # Maximum test sensitivity plus specificity
                if (do_pa) {
                    thr.all.mss <- median(sapply(enm_mxt_all, function(x) {
                        x$test.evaluation@t[which.max(x$test.evaluation@TPR + x$test.evaluation@TNR)]
                    }))
                    if (alg == "glm") {
                        thr.all.mss <- plogis(thr.all.mss)
                    }
                    raster.all <- enm_mxt_all.r.m
                    raster.all[raster.all < thr.all.mss] <- 0
                    raster.all[raster.all >= thr.all.mss] <- 1
                    pa.all.freq <- freq(raster.all)
                    if (export_pa_raster) {
                        writeRaster(raster.all, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_all.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "LOG1S")
                    }
                }
                if (do_vip) {
                    enm_mxt_all.vip <- sapply(enm_mxt_all, enmtools.vip)
                }
                ### enm_mxt_all.cal <- lapply(enm_mxt_all, enmtools.calibrate, env = raster_stack_b, n.background = 10000)
            }
            ### rychlá vizualizace PA map
            if (do_pa) {
                png(paste0(export_path, "/outputs/png-pa/", sp, "_", px_size_item, "_", pres, "_", replicates, "_gbif.png"))
                plot(raster.gbif, main = paste0(sp, " | GBIF, (", (px_size_item / 1000), "km)"))
                dev.off()
                if (do_all) {
                    png(paste0(export_path, "/outputs/png-pa/", sp, "_", px_size_item, "_", pres, "_", replicates, "_all.png"))
                    plot(raster.all, main = paste0(sp, " | ALL, (", (px_size_item / 1000), "km)"))
                    dev.off()
                }
                png(paste0(export_path, "/outputs/png-pa/", sp, "_", px_size_item, "_", pres, "_", replicates, "_ndop.png"))
                plot(raster.ndop, main = paste0(sp, " | NDOP, (", (px_size_item / 1000), "km)"))
                dev.off()
            }
            # # # # # # # # ořezy # # # # # # # # # #
            print("ořezy")

            # po ořezu nenínutné znormalizovat nově vzniklé rastery - dělá se v raster.breadth a raster.overlap automaticky přes raster.standardize (ten ale standardizuje čistě sumou všech pixelů...?!)

            # # ořez výsledné predikce
            # ořez GBIF ČR
            enm_mxt_gbif.r.m.crop <- crop(enm_mxt_gbif.r.m, extent(czechia_3035))
            enm_mxt_gbif.r.m.crop.czechia <- mask(enm_mxt_gbif.r.m.crop, czechia_3035)
            # binární
            if (do_pa) {
                raster.gbif.crop <- crop(raster.gbif, extent(czechia_3035))
                raster.gbif.crop.czechia <- mask(raster.gbif.crop, czechia_3035)
            }
            # # binární korekce
            # enm_mxt_gbif.r.m.crop.czechia.n <- normalize(enm_mxt_gbif.r.m.crop.czechia)
            # raster.gbif.crop.czechia.n <- enm_mxt_gbif.r.m.crop.czechia.n
            # raster.gbif.crop.czechia.n[raster.gbif.crop.czechia.n < thr.gbif.mss] <- 0
            # raster.gbif.crop.czechia.n[raster.gbif.crop.czechia.n >= thr.gbif.mss] <- 1
            # pa.gbif.freq.n <- freq(raster.gbif.crop.czechia.n)
            # if (export_pa_raster) {
            #     writeRaster(raster.gbif.crop.czechia.n, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_gbif.n.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "INT1U")
            # }

            # ořez ALL ČR
            if (do_all) {
                enm_mxt_all.r.m.crop <- crop(enm_mxt_all.r.m, extent(czechia_3035))
                enm_mxt_all.r.m.crop.czechia <- mask(enm_mxt_all.r.m.crop, czechia_3035)
                # binární
                if (do_pa) {
                    raster.all.crop <- crop(raster.all, extent(czechia_3035))
                    raster.all.crop.czechia <- mask(raster.all.crop, czechia_3035)
                }
            }
            # # binární korekce
            # enm_mxt_all.r.m.crop.czechia.n <- normalize(enm_mxt_all.r.m.crop.czechia)
            # raster.all.crop.czechia.n <- enm_mxt_all.r.m.crop.czechia.n
            # raster.all.crop.czechia.n[raster.all.crop.czechia.n < thr.all.mss] <- 0
            # raster.all.crop.czechia.n[raster.all.crop.czechia.n >= thr.all.mss] <- 1
            # pa.all.freq.n <- freq(raster.all.crop.czechia.n)
            # if (export_pa_raster) {
            #     writeRaster(raster.all.crop.czechia.n, paste0(export_path, "/outputs/r/", pres, "_", sp, "_", px_size_item, "_", replicates, "_all.n.pa.tif"), format = "GTiff", overwrite = TRUE, datatype = "INT1U")
            # }


            # výřez ČR z GBIF
            enm_mxt_gbif.r.m.erase <- crop(enm_mxt_gbif.r.m, extent(blocks_erased_cz_3035))
            enm_mxt_gbif.r.m.erase.czechia <- mask(enm_mxt_gbif.r.m.erase, blocks_erased_cz_3035)
            # binární
            if (do_pa) {
                raster.gbif.erase <- crop(raster.gbif, extent(blocks_erased_cz_3035))
                raster.gbif.erase.czechia <- mask(raster.gbif.erase, blocks_erased_cz_3035)
            }
            if (do_all) {
                # výřez ČR z ALL
                enm_mxt_all.r.m.erase <- crop(enm_mxt_all.r.m, extent(blocks_erased_cz_3035))
                enm_mxt_all.r.m.erase.czechia <- mask(enm_mxt_all.r.m.erase, blocks_erased_cz_3035)
                # binární
                if (do_pa) {
                    raster.all.erase <- crop(raster.all, extent(blocks_erased_cz_3035))
                    raster.all.erase.czechia <- mask(raster.all.erase, blocks_erased_cz_3035)
                }
            }

            # dodatečné raster breadth
            # cropnutá ČR
            gbif_crop.breadth <- raster.breadth(enm_mxt_gbif.r.m.crop.czechia)
            if (do_all) {
                all_crop.breadth <- raster.breadth(enm_mxt_all.r.m.crop.czechia)
            }
            # erasnutá ČR
            gbif_erase.breadth <- raster.breadth(enm_mxt_gbif.r.m.crop.czechia)
            if (do_all) {
                all_erase.breadth <- raster.breadth(enm_mxt_all.r.m.crop.czechia)
            }

            print("env.overlap")

            if (do_all) {
                eos.gbif_all <- list()
                for (r in 1:replicates) {
                    eos.gbif_all[[r]] <- env.overlap(enm_mxt_all[[r]], enm_mxt_gbif[[r]], env = raster_stack_b)[1:3]
                }

                eos.all_ndop <- list() # má smysl, když jsou odlišné extenty?
                for (r in 1:replicates) {
                    eos.all_ndop[[r]] <- env.overlap(enm_mxt_all[[r]], enm_mxt_ndop[[r]], env = raster_stack_b)[1:3]
                }

                eos.gbif_ndop <- list() # má smysl, když jsou odlišné extenty?
                for (r in 1:replicates) {
                    eos.gbif_ndop[[r]] <- env.overlap(enm_mxt_gbif[[r]], enm_mxt_ndop[[r]], env = raster_stack_b)[1:3]
                }
            }
            #
            ## # # # #  niche identity, zatím ne, není jak to jednoznačně strojově interpretovat dle jedné hodnoty - asi se musí vztáhnout jednotlivá opakování k empirické hodnotě?
            #
            # ni.gbif_ndop <- identity.test(species.1 = enm_mxt_gbif.s, species.2 = enm_mxt_ndop.s, env = raster_stack_b, type = "glm", nreps = replicates)
            # ni.gbif_all <- identity.test(species.1 = enm_mxt_all.s, species.2 = enm_mxt_gbif.s, env = raster_stack_b, type = "glm", nreps = replicates)
            # #jak vypadá srovnání dvou stejných modelů?
            # ni.gbif_gbif <- identity.test(species.1 = enm_mxt_gbif.s, species.2 = enm_mxt_gbif.s, env = raster_stack_b, type = "glm", nreps = replicates)

            if (do_vip) {
                enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
                    as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
                })

                enm_mxt_ndop.vip.t <- lapply(enm_mxt_ndop.vip[seq(1, replicates * 2, 2)], function(x) {
                    as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
                })
                if (do_all) {
                    enm_mxt_all.vip.t <- lapply(enm_mxt_all.vip[seq(1, replicates * 2, 2)], function(x) {
                        as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
                    })
                }
                if (replicates == 1) {
                    enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
                    enm_mxt_ndop.vip.s <- enm_mxt_ndop.vip.t[[1]]
                    if (do_all) {
                        enm_mxt_all.vip.s <- enm_mxt_all.vip.t[[1]]
                    }
                } else {
                    b_g <- enm_mxt_gbif.vip.t[[1]]
                    b_n <- enm_mxt_ndop.vip.t[[1]]
                    b_a <- enm_mxt_all.vip.t[[1]]
                    for (n in 1:replicates) {
                        if (n > 1) {
                            b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
                            b_n %<>% add_row(enm_mxt_ndop.vip.t[[n]])
                            if (do_all) {
                                b_a %<>% add_row(enm_mxt_all.vip.t[[n]])
                            }
                        }
                    }
                    enm_mxt_gbif.vip.s <- b_g %>%
                        summarise_if(is.numeric, median, na.rm = TRUE)

                    enm_mxt_ndop.vip.s <- b_n %>%
                        summarise_if(is.numeric, median, na.rm = TRUE)

                    enm_mxt_all.vip.s <- b_a %>%
                        summarise_if(is.numeric, median, na.rm = TRUE)
                }
                enm_mxt_gbif.vip.s.z <- enm_mxt_gbif.vip.s %>% unite("enm_mxt_gbif.vip", names(enm_mxt_gbif.vip.t[[1]])) # separate(xy, c("x", "y"))
                enm_mxt_ndop.vip.s.z <- enm_mxt_ndop.vip.s %>% unite("enm_mxt_ndop.vip", names(enm_mxt_ndop.vip.t[[1]])) # separate(xy, c("x", "y"))
                if (do_all) {
                    enm_mxt_all.vip.s.z <- enm_mxt_all.vip.s %>% unite("enm_mxt_all.vip", names(enm_mxt_all.vip.t[[1]])) # separate(xy, c("x", "y"))
                }
            } else {
                enm_mxt_gbif.vip.s <- NA
                enm_mxt_ndop.vip.s <- NA
                enm_mxt_all.vip.s <- NA
            }

            gbif_ndop.geo.m <- raster.overlap(enm_mxt_gbif.r.m.crop.czechia, enm_mxt_ndop.r.m)
            if (do_all) {
                gbif_all.geo.m <- raster.overlap(enm_mxt_all.r.m, enm_mxt_gbif.r.m)
                gbif_all_erase.geo.m <- raster.overlap(enm_mxt_all.r.m.erase.czechia, enm_mxt_gbif.r.m.erase.czechia)
            }
            if (!do_pa) {
                pa.gbif.freq <- NA
                pa.ndop.freq <- NA
                pa.all.freq <- NA

                raster.ndop <- NA
                raster.all <- NA
                raster.gbif <- NA
                raster.gbif.crop.czechia <- NA
                raster.all.crop.czechia <- NA
                raster.all.erase.czechia <- NA
                aster.gbif.erase.czechia <- NA
            }

            print("RMSE")
            rmse <- rRMSE(enm_mxt_gbif.r.m.crop.czechia, enm_mxt_ndop.r.m)
            print("RMSEs")
            rmses <- rRMSE(raster.standardize(enm_mxt_gbif.r.m.crop.czechia), raster.standardize(enm_mxt_ndop.r.m))

            print("EPS")
            eps <- rEPS(enm_mxt_gbif.r.m.crop.czechia, enm_mxt_ndop.r.m)
            print("EPSs")
            epss <- rEPS(raster.standardize(enm_mxt_gbif.r.m.crop.czechia), raster.standardize(enm_mxt_ndop.r.m))

            print("fill emsr")
            if (do_all) {
                enmsr[[as.character(px_size_item)]][[as.character(sp)]] <- list(
                    reps = replicates,
                    px_size_item = px_size_item,
                    species = as.character(sp),
                    ndop_c = ndop_f_n,
                    ndop_c_thin = ndop_f_n_thin,
                    gbif_c = gbif_f_n,
                    ndop_c.f = ndop_f_n.f,
                    gbif_c.f = gbif_f_n.f,
                    ndop_all_fraction = ndop_all_fraction,
                    gbif_all_fraction = gbif_all_fraction,
                    fraction_used = fraction_used,
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
                    # selected adjust
                    bvn_gbif = bvn_gbif,
                    bvn_ndop = bvn_ndop,
                    bvn_all = bvn_all,
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
                    auc.gbif.tr = median(sapply(enm_mxt_gbif, function(x) x$training.evaluation@auc)),
                    auc.gbif.te = median(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc)),
                    auc.gbif.tr.env = median(sapply(enm_mxt_gbif, function(x) x$env.training.evaluation@auc)),
                    auc.gbif.te.env = median(sapply(enm_mxt_gbif, function(x) x$env.test.evaluation@auc)),
                    # auc.gbif.boyce = median(sapply(enm_mxt_gbif.cal, function(x) x$continuous.boyce$Spearman.cor)),

                    auc.ndop.tr = median(sapply(enm_mxt_ndop, function(x) x$training.evaluation@auc)),
                    auc.ndop.te = median(sapply(enm_mxt_ndop, function(x) x$test.evaluation@auc)),
                    auc.ndop.tr.env = median(sapply(enm_mxt_ndop, function(x) x$env.training.evaluation@auc)),
                    auc.ndop.te.env = median(sapply(enm_mxt_ndop, function(x) x$env.test.evaluation@auc)),
                    # auc.ndop.boyce = median(sapply(enm_mxt_ndop.cal, function(x) x$continuous.boyce$Spearman.cor)),

                    auc.all.tr = median(sapply(enm_mxt_all, function(x) x$training.evaluation@auc)),
                    auc.all.te = median(sapply(enm_mxt_all, function(x) x$test.evaluation@auc)),
                    auc.all.tr.env = median(sapply(enm_mxt_all, function(x) x$env.training.evaluation@auc)),
                    auc.all.te.env = median(sapply(enm_mxt_all, function(x) x$env.test.evaluation@auc)),
                    # auc.all.boyce = median(sapply(enm_mxt_all.cal, function(x) x$continuous.boyce$Spearman.cor)),

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
                    # pa.gbif.sum.p.n = get_freq_by_cat(pa.gbif.freq.n, 1),
                    # pa.gbif.sum.a.n = get_freq_by_cat(pa.gbif.freq.n, 0),
                    # pa.all.sum.p.n = get_freq_by_cat(pa.all.freq.n, 1),
                    # pa.all.sum.a.n = get_freq_by_cat(pa.all.freq.n, 0),

                    # překryvy PA map
                    gbif_ndop.pa = as_tibble(rasters_confusion(raster.gbif.crop.czechia, raster.ndop)),
                    ndop_gbif.pa = as_tibble(rasters_confusion(raster.ndop, raster.gbif.crop.czechia)),
                    all_ndop.pa = as_tibble(rasters_confusion(raster.all.crop.czechia, raster.ndop)),
                    ndop_all.pa = as_tibble(rasters_confusion(raster.ndop, raster.all.crop.czechia)),
                    all_gbif.pa = as_tibble(rasters_confusion(raster.all, raster.gbif)),
                    gbif_all.pa = as_tibble(rasters_confusion(raster.gbif, raster.all)),
                    all_gbif_erase.pa = as_tibble(rasters_confusion(raster.all.erase.czechia, raster.gbif.erase.czechia)),
                    gbif_all_erase.pa = as_tibble(rasters_confusion(raster.gbif.erase.czechia, raster.all.erase.czechia)),
                    # gbif_ndop.pa = as_tibble(rasters_confusion(raster.ndop, raster.gbif.crop.czechia.n)),
                    # all_ndop.pa = as_tibble(rasters_confusion(raster.ndop, raster.all.crop.czechia.n)),

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

                    gbif_ndop.geo.rmse = rmse,
                    gbif_ndop.geo.eps = eps,
                    gbif_ndop.geo.rmses = rmses,
                    gbif_ndop.geo.epss = epss,

                    # niche overlap
                    gbif_all.env.D = NA,
                    gbif_all.env.I = NA,
                    gbif_all.env.cor = NA,

                    gbif_ndop.env.D = NA,
                    gbif_ndop.env.I = NA,
                    gbif_ndop.env.cor = NA,

                    all_ndop.env.D = NA,
                    all_ndop.env.I = NA,
                    all_ndop.env.cor = NA
                )
            } else {
                enmsr[[as.character(px_size_item)]][[as.character(sp)]] <- list(
                    reps = replicates,
                    px_size_item = px_size_item,
                    species = as.character(sp),
                    ndop_c = ndop_f_n,
                    ndop_c_thin = ndop_f_n_thin,
                    gbif_c = gbif_f_n,
                    ndop_c.f = ndop_f_n.f,
                    gbif_c.f = gbif_f_n.f,
                    ndop_all_fraction = ndop_all_fraction,
                    gbif_all_fraction = gbif_all_fraction,
                    fraction_used = fraction_used,
                    gbif.np.tr = enm_mxt_gbif.np.tr,
                    gbif.np.te = enm_mxt_gbif.np.te,
                    all.np.tr = NA,
                    all.np.te = NA,
                    ndop.np.tr = enm_mxt_ndop.np.tr,
                    ndop.np.te = enm_mxt_ndop.np.te,
                    # rbreadth
                    gbif.breadth.B1 = enm_mxt_gbif.breadth.B1,
                    gbif.breadth.B2 = enm_mxt_gbif.breadth.B2,
                    ndop.breadth.B1 = enm_mxt_ndop.breadth.B1,
                    ndop.breadth.B2 = enm_mxt_ndop.breadth.B2,
                    all.breadth.B1 = NA,
                    all.breadth.B2 = NA,
                    # selected adjust
                    bvn_gbif = bvn_gbif,
                    bvn_ndop = bvn_ndop,
                    bvn_all = NA,
                    # rbreadth dodatečné po cropu a erasu
                    gbif_crop.breadth.B1 = gbif_crop.breadth$B1,
                    gbif_crop.breadth.B2 = gbif_crop.breadth$B2,
                    all_crop.breadth.B1 = NA,
                    all_crop.breadth.B2 = NA,
                    gbif_erase.breadth.B1 = gbif_erase.breadth$B1,
                    gbif_erase.breadth.B2 = gbif_erase.breadth$B2,
                    all_erase.breadth.B1 = NA,
                    all_erase.breadth.B2 = NA,

                    # ebreath
                    gbif.ebreadth.B2 = enm_mxt_gbif.ebreadth.B2,
                    ndop.ebreadth.B2 = enm_mxt_ndop.ebreadth.B2,
                    all.ebreadth.B2 = NA,

                    # AUC
                    auc.gbif.tr = median(sapply(enm_mxt_gbif, function(x) x$training.evaluation@auc)),
                    auc.gbif.te = median(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc)),
                    auc.gbif.tr.env = median(sapply(enm_mxt_gbif, function(x) x$env.training.evaluation@auc)),
                    auc.gbif.te.env = median(sapply(enm_mxt_gbif, function(x) x$env.test.evaluation@auc)),
                    # auc.gbif.boyce = median(sapply(enm_mxt_gbif.cal, function(x) x$continuous.boyce$Spearman.cor)),

                    auc.ndop.tr = median(sapply(enm_mxt_ndop, function(x) x$training.evaluation@auc)),
                    auc.ndop.te = median(sapply(enm_mxt_ndop, function(x) x$test.evaluation@auc)),
                    auc.ndop.tr.env = median(sapply(enm_mxt_ndop, function(x) x$env.training.evaluation@auc)),
                    auc.ndop.te.env = median(sapply(enm_mxt_ndop, function(x) x$env.test.evaluation@auc)),
                    # auc.ndop.boyce = median(sapply(enm_mxt_ndop.cal, function(x) x$continuous.boyce$Spearman.cor)),

                    auc.all.tr = NA,
                    auc.all.te = NA,
                    auc.all.tr.env = NA,
                    auc.all.te.env = NA,
                    # auc.all.boyce = median(sapply(enm_mxt_all.cal, function(x) x$continuous.boyce$Spearman.cor)),

                    # sedi
                    sedi.gbif = sedi.gbif,
                    sedi.all = NA,
                    sedi.ndop = sedi.ndop,

                    # thr
                    thr.gbif = as_tibble(thr.gbif),
                    thr.all = NA,
                    thr.ndop = as_tibble(thr.ndop),

                    # PA
                    pa.gbif.sum.p = get_freq_by_cat(pa.gbif.freq, 1),
                    pa.gbif.sum.a = get_freq_by_cat(pa.gbif.freq, 0),
                    pa.all.sum.p = NA,
                    pa.all.sum.a = NA,
                    pa.ndop.sum.p = get_freq_by_cat(pa.ndop.freq, 1),
                    pa.ndop.sum.a = get_freq_by_cat(pa.ndop.freq, 0),
                    # pa.gbif.sum.p.n = get_freq_by_cat(pa.gbif.freq.n, 1),
                    # pa.gbif.sum.a.n = get_freq_by_cat(pa.gbif.freq.n, 0),
                    # pa.all.sum.p.n = get_freq_by_cat(pa.all.freq.n, 1),
                    # pa.all.sum.a.n = get_freq_by_cat(pa.all.freq.n, 0),

                    # překryvy PA map
                    gbif_ndop.pa = as_tibble(rasters_confusion(raster.gbif.crop.czechia, raster.ndop)),
                    ndop_gbif.pa = as_tibble(rasters_confusion(raster.ndop, raster.gbif.crop.czechia)),
                    all_ndop.pa = NA,
                    ndop_all.pa = NA,
                    all_gbif.pa = NA,
                    gbif_all.pa = NA,
                    all_gbif_erase.pa = NA,
                    gbif_all_erase.pa = NA,
                    # gbif_ndop.pa = as_tibble(rasters_confusion(raster.ndop, raster.gbif.crop.czechia.n)),
                    # all_ndop.pa = as_tibble(rasters_confusion(raster.ndop, raster.all.crop.czechia.n)),

                    # performance indexy, treshold maxSSS (dismo: spec_sens)
                    perf.gbif = as_tibble(enm_mxt_gbif.perf),
                    perf.all = NA,
                    perf.ndop = as_tibble(enm_mxt_ndop.perf),

                    # variable permutation importance - tibble
                    vip1.gbif = enm_mxt_gbif.vip.s,
                    vip1.ndop = enm_mxt_ndop.vip.s,
                    vip1.all = NA,

                    # geogr. overlap
                    gbif_ndop.geo.D = gbif_ndop.geo.m$D,
                    gbif_ndop.geo.I = gbif_ndop.geo.m$I,
                    gbif_ndop.geo.cor = gbif_ndop.geo.m$rank.cor,

                    gbif_all.geo.D = NA,
                    gbif_all.geo.I = NA,
                    gbif_all.geo.cor = NA,

                    gbif_all_erase.geo.D = NA,
                    gbif_all_erase.geo.I = NA,
                    gbif_all_erase.geo.cor = NA,

                    gbif_ndop.geo.rmse = rmse,
                    gbif_ndop.geo.eps = eps,
                    gbif_ndop.geo.rmses = rmses,
                    gbif_ndop.geo.epss = epss,

                    # niche overlap
                    gbif_all.env.D = NA,
                    gbif_all.env.I = NA,
                    gbif_all.env.cor = NA,

                    gbif_ndop.env.D = NA,
                    gbif_ndop.env.I = NA,
                    gbif_ndop.env.cor = NA,

                    all_ndop.env.D = NA,
                    all_ndop.env.I = NA,
                    all_ndop.env.cor = NA
                )
            }
        }
        gc()
    }
    gc()
    #  dev.off()
    timestamp <- round(unclass(as.POSIXct(Sys.time())))

    if (eval) {
        saveRDS(enmsr, file = paste0(export_path, "/outputs/rds/enmsr_", pres, "_", px_size_item, "_", replicates, "_", timestamp, "_", generate_bias_raster_version, ".rds"))
        enmsr <- list()
    } else {
        # uložení ideálních adjustovaných raster biasů podle pixel size a druhu
        saveRDS(fm_gbif_f_i_c, file = paste0(export_path, "/inputs/occurrences/", alg, "_fm_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
        saveRDS(fm_ndop_f_i_c, file = paste0(export_path, "/inputs/occurrences/", alg, "_fm_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
        saveRDS(fm_all_f_i_c, file = paste0(export_path, "/inputs/occurrences/", alg, "_fm_all_", px_size_item, "-", cmd_arg_str, ".rds"))
        fm_gbif_f_i_c <- list()
        fm_ndop_f_i_c <- list()
        fm_all_f_i_c <- list()
        saveRDS(fm_gbif_f_i_c.t, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmt_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
        saveRDS(fm_ndop_f_i_c.t, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmt_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
        saveRDS(fm_all_f_i_c.t, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmt_all_", px_size_item, "-", cmd_arg_str, ".rds"))
        fm_gbif_f_i_c.t <- list()
        fm_ndop_f_i_c.t <- list()
        fm_all_f_i_c.t <- list()
        saveRDS(fm_gbif_f_i_c.t.c, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmtc_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
        saveRDS(fm_ndop_f_i_c.t.c, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmtc_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
        saveRDS(fm_all_f_i_c.t.c, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmtc_all_", px_size_item, "-", cmd_arg_str, ".rds"))
        fm_gbif_f_i_c.t.c <- list()
        fm_ndop_f_i_c.t.c <- list()
        fm_all_f_i_c.t.c <- list()
        # saveRDS(fm_gbif_f_i_cSD, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmSD_gbif_", px_size_item, "-", cmd_arg_str, ".rds"))
        # saveRDS(fm_ndop_f_i_cSD, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmSD_ndop_", px_size_item, "-", cmd_arg_str, ".rds"))
        # saveRDS(fm_all_f_i_cSD, file = paste0(export_path, "/inputs/occurrences/", alg, "_fmSD_all_", px_size_item, "-", cmd_arg_str, ".rds"))
        # fm_gbif_f_i_cSD <- list()
        # fm_ndop_f_i_cSD <- list()
        # fm_all_f_i_cSD <- list()
    }
}

end_time <- Sys.time()
print("///////////////////////total konec///////////////////////")
# časové rozmezí a celkový čas generování
print(paste("start:", start_time))
print(paste("konec:", end_time))
print(end_time - start_time)
print("///////////////////////total konec///////////////////////")