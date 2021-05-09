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
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "blockCV", "ggplot2", "dismo", "ENMTools", "data.table", "rmaxent", "MASS")
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
limit_min_occurences <- 100
limit_max_occurences <- 200
px_size <- c(10000) # 100, 500, 1000, 5000, 10000
replicates <- 3
generate_bias_raster <- TRUE
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

# uložení všech výstupů
enms <- list()

for (px_size_item in px_size) {
    rasters_path <- paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/", px_size_item, "/")
    vif5 <- c(
        paste0("l8_3-5_", px_size_item, "_MNDWI"),
        paste0("l8_6-8_", px_size_item, "_EVI"),
        paste0("l8_6-8_", px_size_item, "_MNDWI"),
        paste0("l8_9-11_", px_size_item, "_B5"),
        paste0("l8_9-11_", px_size_item, "_EVI"),
        paste0("l8_9-11_", px_size_item, "_MNDWI"),
        paste0("wc_", px_size_item, "_bio02"),
        paste0("wc_", px_size_item, "_bio03"),
        paste0("wc_", px_size_item, "_bio09"),
        paste0("wc_", px_size_item, "_bio13"),
        paste0("wc_", px_size_item, "_bio15")
    )


    vif5sapply <- lapply(vif5, function(x, rasters_path) {
        return(paste0(rasters_path, x, ".tif"))
    }, rasters_path = rasters_path)

    raster_stack <- stack(lapply(vif5sapply, raster::raster))
    rcrs <- crs(raster_stack)
    raster_stack <- setMinMax(raster_stack)

    # sjednocení CRS
    blocks_3035 <- blocks %>% st_transform(rcrs)
    blocks_erased_cz_3035 <- blocks_erased_cz %>% st_transform(rcrs)
    czechia_3035 <- czechia %>% st_transform(rcrs)

    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack_crop <- crop(raster_stack, extent(czechia_3035))
    raster_stack_mask_czechia <- mask(raster_stack_crop, czechia_3035)


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





    # taky pomalé?
    # https://rdrr.io/cran/raster/man/focal.html




    # st_write(cci_gbif_3035$geometry, paste0(export_path, "cci_gbif_3035.shp"))
    # st_write(cci_all_3035$geometry, paste0(export_path, "cci_all_3035.shp"))
    # st_write(cci_ndop_3035$geometry, paste0(export_path, "cci_ndop_3035.shp"))



    # if (generate_bias_raster == TRUE) {

    #     #
    #     #
    #     # Nebo si bias rastery udělat předem v QGISu...
    #     #
    #     #



    #     print("generate_bias_raster")
    #     gc()
    #     cci_gbif_3035_bias <- list()
    #     cci_ndop_3035_bias <- list()
    #     cci_all_3035_bias <- list()
    #     # for (bw in c(100000, 80000, 50000, 10000)) {
    #     #     print(bw)

    #     # pro 100m tam už asi bias nebude reálně zachytitelný - všude bude jen pár jednotek záznamů...
    #     # - jedině subsamplovat 1000m? Nebo zgroupovat okolní hodnoty - přidat jao druhý krok (stačila by standard deviation?)
    #     print("GBIF")
    #     # cci_gbif_3035_bias <- rasterize(as_Spatial(cci_gbif_3035), raster_stack[[1]], fun = "count", background = 0)
    #     # writeRaster(cci_gbif_3035_bias[[1]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterize_gbif.tif"), format = "GTiff", overwrite = TRUE)
    #     # r.min <- minValue(cci_gbif_3035_bias)
    #     # r.max <- maxValue(cci_gbif_3035_bias)

    #     gbif_xy <- as.data.frame(st_coordinates(cci_gbif_3035))
    #     gbif_xy.dens <- kde2d(gbif_xy$X, gbif_xy$Y, n = c(nrow(raster_stack[[1]]), ncol(raster_stack[[1]])))
    #     gbif_xy.dens.ras <- raster(gbif_xy.dens)
    #     plot(gbif_xy.dens.ras)


    #     cci_gbif_3035_bias.scale <- ((cci_gbif_3035_bias - r.min) / (r.max - r.min))
    #     writeRaster(cci_gbif_3035_bias.scale[[1]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterizeS_gbif.tif"), format = "GTiff", overwrite = TRUE)
    #     gc()

    #     print("NDOP")
    #     cci_ndop_3035_bias <- rasterize(as_Spatial(cci_ndop_3035), raster_stack_mask_czechia[[1]], fun = "count", background = 0)
    #     cci_ndop_3035_bias_mask_czechia <- mask(cci_ndop_3035_bias, czechia_3035)
    #     writeRaster(cci_ndop_3035_bias_mask_czechia[[1]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterize_ndop.tif"), format = "GTiff", overwrite = TRUE)
    #     r.min <- minValue(cci_ndop_3035_bias)
    #     r.max <- maxValue(cci_ndop_3035_bias)

    #     cci_ndop_3035_bias.scale <- ((cci_ndop_3035_bias - r.min) / (r.max - r.min))
    #     cci_ndop_3035_bias_mask_czechia.scale <- mask(cci_ndop_3035_bias.scale, czechia_3035)

    #     writeRaster(cci_ndop_3035_bias_mask_czechia.scale[[1]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterizeS_ndop.tif"), format = "GTiff", overwrite = TRUE)
    #     gc()

    #     print("ALL")
    #     cci_all_3035_bias <- rasterize(as_Spatial(cci_all_3035), raster_stack[[1]], fun = "count", background = 0)
    #     writeRaster(cci_all_3035_bias[[1]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterize_all.tif"), format = "GTiff", overwrite = TRUE)
    #     r.min <- minValue(cci_all_3035_bias)
    #     r.max <- maxValue(cci_all_3035_bias)

    #     cci_all_3035_bias.scale <- ((cci_all_3035_bias - r.min) / (r.max - r.min))
    #     writeRaster(cci_all_3035_bias.scale[[1]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterizeS_all.tif"), format = "GTiff", overwrite = TRUE)
    #     gc()
    #     # gc()
    #     # # GBIF
    #     # print("GBIF")
    #     # cci_gbif_3035_bias[[bw]] <- sp.kde(
    #     #     x = as_Spatial(cci_gbif_3035), bw = bw,
    #     #     newdata = raster_stack[[1]], standardize = TRUE,
    #     #     scale.factor = 10000
    #     # )
    #     # writeRaster(cci_gbif_3035_bias[[bw]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_", bw, "_gbif.tif"), format = "GTiff", overwrite = TRUE)

    #     # gc()
    #     # # NDOP
    #     # print("NDOP")
    #     # cci_ndop_3035_bias[[bw]] <- sp.kde(
    #     #     x = as_Spatial(cci_ndop_3035), bw = bw,
    #     #     newdata = raster_stack_mask_czechia[[1]], standardize = TRUE,
    #     #     scale.factor = 10000
    #     # )
    #     # writeRaster(cci_ndop_3035_bias[[bw]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_", bw, "_ndop.tif"), format = "GTiff", overwrite = TRUE)

    #     # gc()
    #     # # ALL
    #     # print("ALL")
    #     # cci_all_3035_bias[[bw]] <- sp.kde(
    #     #     x = as_Spatial(cci_all_3035), bw = bw,
    #     #     newdata = raster_stack[[1]], standardize = TRUE,
    #     #     scale.factor = 10000
    #     # )
    #     # writeRaster(cci_all_3035_bias[[bw]], paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_", bw, "_all.tif"), format = "GTiff", overwrite = TRUE)
    #     # }
    # }
    # stop()
    # bias_gbif <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterizeS_gbif.tif"))
    # bias_ndop <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterizeS_ndop.tif"))
    # bias_all <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/rs/", px_size_item, "_rasterizeS_all.tif"))


    bias_gbif <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/qgis/", px_size_item, "/rkd_", px_size_item, "_10000.tif"))
    bias_all <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/qgis/", px_size_item, "/rkd2_", px_size_item, "_10000.tif"))
    bias_ndop <- raster(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/bias_files/qgis/", px_size_item, "/rkd3_", px_size_item, "_10000.tif"))

    bias_gbif[is.na(bias_gbif[])] <- 0
    bias_all[is.na(bias_all[])] <- 0
    bias_ndop[is.na(bias_ndop[])] <- 0


    bias_gbif_crop <- crop(bias_gbif, extent(raster_stack))

    bias_all_crop <- crop(bias_all, extent(raster_stack))

    bias_ndop_crop <- crop(bias_ndop, extent(raster_stack_mask_czechia[[1]]))
    bias_ndop_crop_mask <- mask(bias_ndop_crop, czechia_3035)
    bias_ndop_crop_mask <- setExtent(bias_ndop_crop_mask, extent(raster_stack_mask_czechia[[1]]), keepres = TRUE)
    origin(bias_ndop_crop_mask) <- 0

    species <- ptaci_ndop_distinct$species # přepisuju původní seznam z ndop_top
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


        check.bg(species = enm_species, env = raster_stack, nback = 10000, bias = bias_gbif_crop)

        check.species(enm_species)
        enm_mxt_gbif.s <- enm_species

        enm_mxt_gbif <- replicate(replicates, enmtools.maxent(
            enm_species,
            raster_stack,
            test.prop = 0.3,
            verbose = TRUE,
            bias = bias_gbif_crop
        ),
        simplify = FALSE
        )

        # str(enm_mxt_gbif[[1]]$test.evaluation@auc)
        # str(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]][[1]]$test.evaluation@auc)
        # enm_mxt_gbif.auc <- mean(sapply(enms[["10000"]][["Buteo rufinus"]][[1]][["m"]], function(x) x$test.evaluation@auc))
        # enm_mxt_gbif.auc <- mean(sapply(enm_mxt_gbif, function(x) x$test.evaluation@auc))
        # names(enm_mxt_gbif) <- paste0("rep", 1:repl)
        enm_mxt_gbif.r <- stack(sapply(enm_mxt_gbif, function(x) x$suitability))
        enm_mxt_gbif.r.m <- calc(enm_mxt_gbif.r, fun = mean)
        writeRaster(enm_mxt_gbif.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", px_size_item, "_", replicates, "_gbif.tif"), format = "GTiff", overwrite = TRUE)


        # možnost vypsat si VIP jednotlivých proměnných - vypisuje se po PÁRECH (výsledky+grafika) pr každé opakování!!!
        # - [[1]] obsahuje statistiky (tibble: Variable, Importance, StDev), [[2]] pak grafický výstup (ggplot) zobrajující přispění
        # str(enm_mxt_gbif.vip[[1]], max.level = 2) # možnost vypsat si VIP jednotlivých proměnných
        enm_mxt_gbif.vip <- sapply(enm_mxt_gbif, enmtools.vip)

        # calculates Continuous Boyce Index
        enm_mxt_gbif.cal <- lapply(enm_mxt_gbif, enmtools.calibrate, env = raster_stack, n.background = 10000)

        # # # # # # # # # # # # # # # # #
        # # simuluje occurence points!!! - jde použít k novému otestování pro PPPM (Poisson point proces model): třeba pomocí blockCV (nebo novým modelem?) takto to dělal Gabbas??? - pak uvádím úplně nové statistiky modelované již z těchto PPPM bodů?!
        # # # # # # # # # # # # # # # # #
        # # po párech X a Y souřadnice
        # # str(enm_mxt_gbif.sim[,1], max.level = 1) / první sada x,y souřadnic: enm_mxt_gbif.sim[,1]$x enm_mxt_gbif.sim[,1]$y
        # # str(enm_mxt_gbif.sim[,2], max.level = 1)
        # enm_mxt_gbif.sim <- sapply(enm_mxt_gbif, sim.points, n.points = 10000)
        # # as.data.frame(enm_mxt_gbif.sim[,2])




        # str(enm_mxt_gbif, max.level = 1)
        # str(enm_mxt_gbif[[1]]$test.evaluation)

        # interactive.plot( enm_mxt_gbif)

        # v <- visualize.enm(enm_mxt_gbif, raster_stack, layers = c=("wc_1000_bio09"))
        # v$suit.plot
        # v$background.plot







        # ############################## ořez výsledné predikce
        # pred_gbif_mean_crop <- crop(pred_gbif_mean, extent(czechia_3035))
        # pred_gbif_mean_mask_czechia <- mask(pred_gbif_mean_crop, czechia_3035)



        ###
        ### ndop
        ###

        print("NDOP")
        enm_mxt_ndop.pp <- as.data.frame(st_coordinates(ndop_f))

        colnames(enm_mxt_ndop.pp)[1] <- "Longitude"
        colnames(enm_mxt_ndop.pp)[2] <- "Latitude"
        enm_species <- enmtools.species(range = raster_stack_mask_czechia[[1]], species.name = as.character(sp), presence.points = enm_mxt_ndop.pp)

        check.bg(species = enm_species, env = raster_stack_mask_czechia, nback = 1000, bias = bias_ndop_crop_mask)

        check.species(enm_species)
        enm_mxt_ndop.s <- enm_species

        enm_mxt_ndop <- replicate(replicates, enmtools.maxent(
            enm_species,
            raster_stack_mask_czechia,
            test.prop = 0.3,
            verbose = TRUE,
            bias = bias_ndop_crop_mask
        ),
        simplify = FALSE
        )

        enm_mxt_ndop.r <- stack(sapply(enm_mxt_ndop, function(x) x$suitability))
        enm_mxt_ndop.r.m <- calc(enm_mxt_ndop.r, fun = mean)
        writeRaster(enm_mxt_ndop.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", px_size_item, "_", replicates, "_ndop.tif"), format = "GTiff", overwrite = TRUE)

        enm_mxt_ndop.vip <- sapply(enm_mxt_ndop, enmtools.vip)

        enm_mxt_ndop.cal <- lapply(enm_mxt_ndop, enmtools.calibrate, env = raster_stack, n.background = 1000)

        ###
        ### all (ndpop+gbif)
        ###
        print("ALL")

        enm_mxt_all.pp <- as.data.frame(st_coordinates(all_f))

        colnames(enm_mxt_all.pp)[1] <- "Longitude"
        colnames(enm_mxt_all.pp)[2] <- "Latitude"
        enm_species <- enmtools.species(range = raster_stack[[1]], species.name = as.character(sp), presence.points = enm_mxt_all.pp)

        check.bg(species = enm_species, env = raster_stack, nback = 10000, bias = bias_all_crop)

        check.species(enm_species)
        enm_mxt_all.s <- enm_species

        enm_mxt_all <- replicate(replicates, enmtools.maxent(
            enm_species,
            raster_stack,
            test.prop = 0.3,
            verbose = TRUE,
            bias = bias_all_crop
        ),
        simplify = FALSE
        )

        enm_mxt_all.r <- stack(sapply(enm_mxt_all, function(x) x$suitability))
        enm_mxt_all.r.m <- calc(enm_mxt_all.r, fun = mean)
        writeRaster(enm_mxt_all.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", px_size_item, "_", replicates, "_all.tif"), format = "GTiff", overwrite = TRUE)

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
}