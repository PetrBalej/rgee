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

options(scipen = 999) # výpis čísel v nezkrácené podobě
options(java.parameters = c("-Xmx20g"))
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "blockCV", "ggplot2", "dismo", "rmaxent", "ENMToolsRMaxent", "data.table", "MASS", "spatstat")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/rgee/R/export_raster/functions.R"))
source(paste0(getwd(), "/rgee/R/export_raster/ndop_top.R"))


export_path <-
    paste0(getwd(), "/tmp3/")
# asi nebudu zapisovat do csv, rovnou z proměnných?  (potenciálně špatně přenositelné a vyhodnotitelné někým jiným...)
# filename_csv <- "gOverlap_3v_10000X.csv"
# limit_max_occurences_ndop_top <- 10000 # 70000; nepoužívané

rcrs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
limit_min_occurences <- 100
limit_max_occurences <- 70000
px_size <- c(10000) # 100, 500, 1000, 5000, 10000 # 10000, 5000, 1000, 500, 100
replicates <- 3
generate_bias_raster <- FALSE
bias_bw <- 100000 # menší pro 1000 a 100m?

# shapefiles
# BB (+ČR)
blocks <- st_read(paste0(wd, "/rgee/shp/blocks.shp"))
# BB - ČR
blocks_erased_cz <- st_read(paste0(wd, "/rgee/shp/blocks_erased_cz.shp"))
# ČR
czechia <- st_read(paste0(wd, "/rgee/shp/ne_10m_admin_0_countries/czechia/cz_3035.shp"))

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
plot_ndop_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/ndop_300.csv")
plot_gbif_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/gbif_300.csv")



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
    filter(count <= limit_max_occurences) %>%
    filter(!is.na(species))
ptaci_gbif_distinct <- plot_gbif_csv %>%
    group_by(species) %>%
    summarise(count = n_distinct(key)) %>%
    arrange(desc(count)) %>%
    filter(count >= limit_min_occurences) %>%
    filter(count <= limit_max_occurences) %>%
    filter(!is.na(species))


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





# uložení všech výstupů
enms <- list()
start_time <- Sys.time()
for (px_size_item in px_size) {


    #  pdf(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/predikce_", px_size_item, ".pdf"))

    # # # původní načítání rasterů prediktorů, dočasná optimalizace aby se nemusel ypokaždé skrz propisovat NA hodnoty
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
    # # propíše všude jednotně NA - nutné, asi dříve problém s predikcemi nad většími oblastmi s NA nad Alpami?
    # raster_stack <- raster::mask(raster_stack, sum(raster_stack))

    raster_stack <- stack(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons-prep-11/central-europe-", px_size_item, ".grd"))
    rcrs <- crs(raster_stack)
    raster_stack <- setMinMax(raster_stack)

    # sjednocení CRS
    blocks_3035 <- blocks %>% st_transform(rcrs)
    blocks_erased_cz_3035 <- blocks_erased_cz %>% st_transform(rcrs)
    czechia_3035 <- czechia %>% st_transform(rcrs)

    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack_crop <- crop(raster_stack, extent(czechia_3035))
    raster_stack_mask_czechia <- mask(raster_stack_crop, czechia_3035)


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
        bias_gbif <- ((bias_gbif - r.min) / (r.max - r.min))
        crs(bias_gbif) <- rcrs
        writeRaster(bias_gbif, paste0(export_path, "Xbias_gbif-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

        print("ALL")
        all_ppp <- ppp(st_coordinates(cci_all_3035)[, 1], st_coordinates(cci_all_3035)[, 2], window = ow)
        # ppp <- rescale(ppp, 10, "km")
        bias_all <- resample(raster(density(all_ppp, sigma = px_size_item)), raster_stack[[1]], method = "bilinear")
        r.min <- minValue(bias_all)
        r.max <- maxValue(bias_all)
        bias_all <- ((bias_all - r.min) / (r.max - r.min))
        crs(bias_all) <- rcrs
        writeRaster(bias_all, paste0(export_path, "Xbias_all-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

        print("NDOP")
        ext <- extent(raster_stack_mask_czechia[[1]])
        ow <- owin(xrange = c(ext@xmin, ext@xmax), yrange = c(ext@ymin, ext@ymax))
        ndop_ppp <- ppp(st_coordinates(cci_ndop_3035)[, 1], st_coordinates(cci_ndop_3035)[, 2], window = ow)
        # ppp <- rescale(ppp, 10, "km")
        bias_ndop <- resample(raster(density(ndop_ppp, sigma = px_size_item)), raster_stack_mask_czechia[[1]], method = "bilinear")
        r.min <- minValue(bias_ndop)
        r.max <- maxValue(bias_ndop)
        bias_ndop <- ((bias_ndop - r.min) / (r.max - r.min))
        crs(bias_ndop) <- rcrs
        writeRaster(bias_ndop, paste0(export_path, "Xbias_ndop-density-", px_size_item, ".tif"), format = "GTiff", overwrite = TRUE)

        rlist <- list(bias_gbif, bias_ndop, bias_all)
    } else {
        bias_gbif <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/Xbias_gbif-density-", px_size_item, ".tif"))
        bias_ndop <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/Xbias_ndop-density-", px_size_item, ".tif"))
        bias_all <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/Xbias_all-density-", px_size_item, ".tif"))
    }

    # # # bias_gbif[is.na(bias_gbif[])] <- 0
    # # # bias_all[is.na(bias_all[])] <- 0
    # # # bias_ndop[is.na(bias_ndop[])] <- 0

    # obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků
    species <- rev(ptaci_ndop_distinct$species) # přepisuju původní seznam z ndop_top

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

        print(paste0(sp, " -  ", nrow(ndop_f), "/", nrow(gbif_f)))

        if (nrow(ndop_f) == 0 | nrow(gbif_f) == 0) {
            print("XXX")
            next
        } else {
            print("***")
        }

        ###
        ### gbif
        ###
        print("GBIF")
        enm_mxt_gbif.pp <- as.data.frame(st_coordinates(gbif_f))

        colnames(enm_mxt_gbif.pp)[1] <- "Longitude"
        colnames(enm_mxt_gbif.pp)[2] <- "Latitude"
        enm_species <- enmtools.species(range = raster_stack[[1]], species.name = as.character(sp), presence.points = enm_mxt_gbif.pp)
        # enms[["10000"]][["Buteo rufinus"]][[1]][["o"]]$presence.points$Longitude

        # bias2 <- raster("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/bias_tg_Chlidonias-hybrida_2_gbif.tif") # 100000 10000 - totožné pro všešchny rozlišení rasterů?


        enm_mxt_gbif.s <- enm_species

        enm_mxt_gbif <- replicate(replicates, enmtools.maxent(
            enm_species,
            raster_stack,
            test.prop = 0.3,
            verbose = TRUE,
            bias = bias_gbif,
            nback = 10000
        ),
        simplify = FALSE
        )

        # str(enm_mxt_gbif[[1]]$test.evaluation@auc)
        # str(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]][[1]]$test.evaluation@auc)
        # enm_mxt_gbif.auc <- mean(sapply(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]], function(x) x$test.evaluation@auc))
        enm_mxt_gbif.auc <- mean(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc))
        # plot(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]][[1]]$suitability)
        # names(enm_mxt_gbif) <- paste0("rep", 1:repl)
        enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
        enm_mxt_gbif.r.m <- calc(enm_mxt_gbif.r, fun = mean)
        # writeRaster(enm_mxt_gbif.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", px_size_item, "_", replicates, "_gbif.tif"), format = "GTiff", overwrite = TRUE)
        png(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/predikce/_", sp, "_", px_size_item, "_", replicates, "_gbif.png"))
        plot(enm_mxt_gbif.r.m, main = paste0(sp, ", GBIF, AUC=", round(enm_mxt_gbif.auc, digits = 3)))
        par(bg = NA)
        plot(czechia_3035$geometry, add = TRUE)
        par()
        points(enm_mxt_gbif.s$presence.points)
        dev.off()

        # možnost vypsat si VIP jednotlivých proměnných - vypisuje se po PÁRECH (výsledky+grafika) pr každé opakování!!!
        # - [[1]] obsahuje statistiky (tibble: Variable, Importance, StDev), [[2]] pak grafický výstup (ggplot) zobrajující přispění
        # str(enm_mxt_gbif.vip[[1]], max.level = 2) # možnost vypsat si VIP jednotlivých proměnných
        # enms[["10000"]][["Buteo rufinus"]][[1]][["vip"]][[1]]$Variables $Importance $StDev
        enm_mxt_gbif.vip <- sapply(enm_mxt_gbif, enmtools.vip)

        # calculates Continuous Boyce Index
        # str(enms[["10000"]][["Buteo rufinus"]][[4]]
        enm_mxt_gbif.cal <- lapply(enm_mxt_gbif, enmtools.calibrate, env = raster_stack, n.background = 10000)



        ###
        ### ndop
        ###

        print("NDOP")
        enm_mxt_ndop.pp <- as.data.frame(st_coordinates(ndop_f))

        colnames(enm_mxt_ndop.pp)[1] <- "Longitude"
        colnames(enm_mxt_ndop.pp)[2] <- "Latitude"
        enm_species <- enmtools.species(range = raster_stack_mask_czechia[[1]], species.name = as.character(sp), presence.points = enm_mxt_ndop.pp)


        enm_mxt_ndop.s <- enm_species

        enm_mxt_ndop <- replicate(replicates, enmtools.maxent(
            enm_species,
            raster_stack_mask_czechia,
            test.prop = 0.3,
            verbose = TRUE,
            bias = bias_ndop,
            nback = 10000
        ),
        simplify = FALSE
        )
        enm_mxt_ndop.auc <- mean(sapply(enm_mxt_ndop, function(x) x$test.evaluation@auc))
        enm_mxt_ndop.r <- stack(sapply(enm_mxt_ndop, function(x) x$suitability))
        enm_mxt_ndop.r.m <- calc(enm_mxt_ndop.r, fun = mean)


        # writeRaster(enm_mxt_ndop.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", px_size_item, "_", replicates, "_ndop.tif"), format = "GTiff", overwrite = TRUE)
        png(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/predikce/_", sp, "_", px_size_item, "_", replicates, "_ndop.png"))
        plot(enm_mxt_ndop.r.m, main = paste0(sp, ", NDOP, AUC=", round(enm_mxt_ndop.auc, digits = 3)))
        par(bg = NA)
        plot(czechia_3035$geometry, add = TRUE)
        par()
        points(enm_mxt_ndop.s$presence.points)
        dev.off()

        enm_mxt_ndop.vip <- sapply(enm_mxt_ndop, enmtools.vip)

        enm_mxt_ndop.cal <- lapply(enm_mxt_ndop, enmtools.calibrate, env = raster_stack, n.background = 10000)



        ###
        ### all (ndpop+gbif)
        ###
        print("ALL")

        enm_mxt_all.pp <- as.data.frame(st_coordinates(all_f))

        colnames(enm_mxt_all.pp)[1] <- "Longitude"
        colnames(enm_mxt_all.pp)[2] <- "Latitude"
        enm_species <- enmtools.species(range = raster_stack[[1]], species.name = as.character(sp), presence.points = enm_mxt_all.pp)

        enm_mxt_all.s <- enm_species

        enm_mxt_all <- replicate(replicates, enmtools.maxent(
            enm_species,
            raster_stack,
            test.prop = 0.3,
            verbose = TRUE,
            bias = bias_all,
            nback = 10000
        ),
        simplify = FALSE
        )
        enm_mxt_all.auc <- mean(sapply(enm_mxt_all, function(x) x$test.evaluation@auc))
        enm_mxt_all.r <- stack(sapply(enm_mxt_all, function(x) x$suitability))
        enm_mxt_all.r.m <- calc(enm_mxt_all.r, fun = mean)

        # writeRaster(enm_mxt_all.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", px_size_item, "_", replicates, "_all.tif"), format = "GTiff", overwrite = TRUE)
        png(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/predikce/_", sp, "_", px_size_item, "_", replicates, "_all.png"))
        plot(enm_mxt_all.r.m, main = paste0(sp, ",GBIF+NDOP, AUC=", round(enm_mxt_all.auc, digits = 3)))
        par(bg = NA)
        plot(czechia_3035$geometry, add = TRUE)
        par()
        points(enm_mxt_all.s$presence.points)
        dev.off()

        enm_mxt_all.vip <- sapply(enm_mxt_all, enmtools.vip)

        enm_mxt_all.cal <- lapply(enm_mxt_all, enmtools.calibrate, env = raster_stack, n.background = 10000)



        # # ořez výsledné predikce
        # ořez GBIF ČR
        enm_mxt_gbif.r.m.crop <- crop(enm_mxt_gbif.r.m, extent(czechia_3035))
        enm_mxt_gbif.r.m.crop.czechia <- mask(enm_mxt_gbif.r.m.crop, czechia_3035)


        # výřez ČR z GBIF
        enm_mxt_gbif.r.m.erase <- crop(enm_mxt_gbif.r.m, extent(blocks_erased_cz_3035))
        enm_mxt_gbif.r.m.erase.czechia <- mask(enm_mxt_gbif.r.m.erase, blocks_erased_cz_3035)

        # výřez ČR z ALL
        enm_mxt_all.r.m.erase <- crop(enm_mxt_all.r.m, extent(blocks_erased_cz_3035))
        enm_mxt_all.r.m.erase.czechia <- mask(enm_mxt_all.r.m.erase, blocks_erased_cz_3035)

        eos <- list()
        for (r in 1:replicates) {
            eos[[r]] <- env.overlap(enm_mxt_all[[r]], enm_mxt_gbif[[r]], env = raster_stack)
        }

        # kompletní výsledky
        enms[[as.character(px_size_item)]][[as.character(sp)]] <- list(
            list(m = enm_mxt_gbif, o = enm_mxt_gbif.s, vip = enm_mxt_gbif.vip),
            list(m = enm_mxt_ndop, o = enm_mxt_ndop.s, vip = enm_mxt_ndop.vip),
            list(m = enm_mxt_all, o = enm_mxt_all.s, vip = enm_mxt_all.vip),
            list(
                gbif_ndop.geo = raster.overlap(enm_mxt_gbif.r.m.crop.czechia, enm_mxt_ndop.r.m),
                gbif_all.geo = raster.overlap(enm_mxt_all.r.m, enm_mxt_gbif.r.m),
                gbif_all_erase.geo = raster.overlap(enm_mxt_all.r.m.erase.czechia, enm_mxt_gbif.r.m.erase.czechia),
                gbif_all.env = eos
            ),
            list(enm_mxt_gbif.cal, enm_mxt_ndop.cal, enm_mxt_all.cal)
        )
    }
    #  dev.off()
}

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