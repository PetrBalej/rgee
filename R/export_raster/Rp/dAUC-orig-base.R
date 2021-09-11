options(scipen = 999)
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra", "viridis", "ENMToolsPB")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

########
## vygeneruje páry mapek z původních TIFů do PDF (původně jsem používal i na znovudopočet EPS z párů originálních TIFFů)
########

# vede na git rgee
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/LV/pdf"

# pomocné funkce
source(paste0(wd, "/R/export_raster/functions.R"))

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



# cci_all_3035 <- readRDS(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/inputs/occurrences/cci_all_3035.rds")) # %>% filter(species %in% ptaci_gbif_distinct$species)
# cci_all_3035_czechia <- st_intersection(cci_all_3035, st_transform(czechia, st_crs(cci_all_3035)))
# saveRDS(cci_all_3035_czechia, file = "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/inputs/occurrences/cci_all_3035_czechia.rds")
cci_all_3035_czechia <- readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/inputs/occurrences/cci_all_3035_czechia.rds")

prefix <- "glm_LV_" # "OWNPFr" "maxent_thr" "glm_GB_"

rds_list <-
    list.files(
        path = paste0(export_path, "/../outputs/rds"), # vse-v-jednom/outputs/rds/
        pattern = paste0("^enmsr_", prefix, ".+\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
        ignore.case = TRUE,
        full.names = TRUE
    )


rds_append <- readRDS(rds_list[[1]])
rds_list <- rds_list[-1]
for (i in seq_along(rds_list)) {
    rds_append <- append(rds_append, readRDS(rds_list[[i]]))
}


# https://cs.wikipedia.org/wiki/Seznam_pt%C3%A1k%C5%AF_%C4%8Ceska
nepuvodni <- c(
    # C
    "Phasianus colchicus",
    "Syrmaticus reevesi",
    "Branta canadensis",
    "Columba livia",
    "Alopochen aegyptiacus",
    "Alopochen aegyptiaca",
    "Threskiornis aethiopicus",
    "Aix galericulata",
    "Oxyura jamaicensis",
    # D
    "Bucephala albeola",
    "Bucephala islandica",
    "Lophodytes cucullatus",
    "Histrionicus histrionicus",
    "Gypaetus barbatus",
    # E
    "Phylloscopus sibilatrix",
    "Aix sponsa",
    "Platalea leucorodia"
)

problematicke <- c(
    "Turdus merula", # lesní vs. městské populace
    "Luscinia svecica", # dva poddruhy s odlišnými nároky, nejsem schopný je jednoduše odlišit...
    "Luscinia luscinia" # problematické nálezy zejména z Červenohorského sedla (>1000mnm, ikdyž jsou vícekrát a dlouhodobě nezávisle potvrzené, možná jde jen o oblíbenou zastávku při průtahu (kam?) nebo záměny s L. mega.? Raději vyloučit.
)

"%notin%" <- Negate("%in%")
for (i in seq_along(names(rds_append))) {
    tibble <- rds_append[[i]] %>% # ttemp
        map_depth(1, na.omit) %>%
        map(as_tibble) %>%
        bind_rows(.id = "species") %>%
        # group_by(species) %>% dplyr::select(-r)
        distinct(species, .keep_all = TRUE) %>%
        filter(species %notin% nepuvodni)
    # %>% filter(species %notin% problematicke)


    # tibble_ordered %>% summarize(vip1.all)
    tibble %<>% tibble %>% group_by(species, px_size_item) # sp = paste(species, px_size_item)

    tibble_gbif <- tibble %>%
        summarize(vip1.gbif, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".gbif")
    tibble_all <- tibble %>%
        summarize(vip1.all, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".all")
    tibble_ndop <- tibble %>%
        summarize(vip1.ndop, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".ndop")




    tibble_gbif_p <- tibble %>%
        summarize(perf.gbif, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".gbif")
    tibble_all_p <- tibble %>%
        summarize(perf.all, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".all")
    tibble_ndop_p <- tibble %>%
        summarize(perf.ndop, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".ndop")




    tibble.gbif_ndop.pa <- tibble %>%
        summarize(gbif_ndop.pa, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".gbif_ndop")
    tibble.ndop_gbif.pa <- tibble %>%
        summarize(ndop_gbif.pa, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".ndop_gbif")

    tibble.all_ndop.pa <- tibble %>%
        summarize(all_ndop.pa, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".all_ndop")
    tibble.ndop_all.pa <- tibble %>%
        summarize(ndop_all.pa, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".ndop_all")

    tibble.all_gbif.pa <- tibble %>%
        summarize(all_gbif.pa, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".all_gbif")
    tibble.all_gbif_erase.pa <- tibble %>%
        summarize(all_gbif_erase.pa, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".all_gbif_erase")


    tibble_result <- tibble %>%
        left_join(tibble_gbif, by = c("species" = "species.gbif")) %>%
        left_join(tibble_all, by = c("species" = "species.all")) %>%
        left_join(tibble_ndop, by = c("species" = "species.ndop")) %>%
        left_join(tibble_gbif_p, by = c("species" = "species.gbif")) %>%
        left_join(tibble_all_p, by = c("species" = "species.all")) %>%
        left_join(tibble_ndop_p, by = c("species" = "species.ndop")) %>%
        left_join(tibble.gbif_ndop.pa, by = c("species" = "species.gbif_ndop")) %>%
        left_join(tibble.ndop_gbif.pa, by = c("species" = "species.ndop_gbif")) %>%
        left_join(tibble.all_ndop.pa, by = c("species" = "species.all_ndop")) %>%
        left_join(tibble.ndop_all.pa, by = c("species" = "species.ndop_all")) %>%
        left_join(tibble.all_gbif.pa, by = c("species" = "species.all_gbif")) %>%
        left_join(tibble.all_gbif_erase.pa, by = c("species" = "species.all_gbif_erase")) %>%
        ungroup() %>%
        dplyr::select(-contains("vip")) %>%
        dplyr::select(-contains("perf.")) %>%
        dplyr::select(-contains(".pa")) %>%
        rename_all(gsub, pattern = ".Importance.", replacement = ".Imp.")



    # spojím to až na konci!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!§§
    if (i == 1) {
        # založím novou tibble na základě vyprázdněné první
        tibble_grains <- tibble_result[NULL, ]
    }

    tibble_grains %<>% add_row(tibble_result)
}

tibble_grains %<>% mutate(gbif_ndop_rate = (gbif_c / ndop_c))
tibble_grains %<>% mutate(gbif_ndop_perc = ((ndop_c * 100) / gbif_c))

tibble_grains_numeric <- tibble_grains %>% select_if(., is.numeric) # pro základní deskriptivní statistiku


# # statistika jednotlivé layery
# p.stats <- tibble_grains %>%  dplyr::select(ends_with(c(".Imp.ndop", ".Imp.gbif")))  %>%
# get_summary_stats(., show = c("mean", "sd", "median", "iqr"))  %>% filter(median >= 0.03) %>%  arrange(variable)
# print(p.stats, n = 100)






# celkem: 228*5: 1140
# auc.ndop.te > 0.70: 606
## gbif_ndop.geo.cor >= 0.70: 181
## gbif_ndop.geo.cor >= 0.75: 115
## gbif_ndop.geo.D >= 0.70: 629
## gbif_ndop.geo.D >= 0.80: 219


# základní filtrace podle AUC a korelace
# tibble_grains.reduced <- tibble_grains %>%
#     select_if(~ is.numeric(.) | is.character(.))
# %>%
# filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75)
# tibble_grains %>%  filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.D >= 0.80)
# write_csv(tibble_grains %>% select_if(~ is.numeric(.) | is.character(.)), "tibble_grains.glm.csv")
# tibble_grains.reduced %>% filter(px_size_item == 10000)

# # NDOP závislost AUC na r.breadth
# plot(tibble_grains$ndop.breadth.B2, tibble_grains$auc.ndop.te)
# plot(tibble_grains.reduced$ndop.breadth.B2, tibble_grains.reduced$auc.ndop.te)


# #
# # kopírování
# #
# png_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/glm-prelim2/outputs/png"
# # Acanthis cabaret_500_glm_BFitA_5003_4_all.png
# tibble_grains.reduced.path <- tibble_grains.reduced %>%
#     mutate(path = paste0(species, "_", px_size_item, "_", "glm_BFitA", "_", px_size_item, "[0-9]{1,2}_4_gbif.png")) %>%
#     select(path)
# # tibble_grains.reduced.path <-tibble_grains.reduced %>%  mutate(path = paste0(species, "_", px_size_item, "_","glm_BFitA", "_", px_size_item, "[0-9]{1,2}_4_gbif.png")) %>% select(path)
# png_list <- list()
# for (p in tibble_grains.reduced.path$path) {
#     png_list[[p]] <-
#         list.files(
#             path = png_path,
#             # původní: "^glm_fmt_", d, "_.*\\.rds$"
#             # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
#             pattern = paste0("^", p, "$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
#             ignore.case = TRUE,
#             full.names = TRUE
#         )
# }

# png_list <- unname(unlist(png_list))

# # file.copy(unname(unlist(png_list)), "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/glm-prelim2/outputs/png-check-ndop")
# #  file.copy(unname(unlist(png_list)), "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/glm-prelim2/outputs/png-check-gbif")



# #
# # 1) výpočet a přídaní RMSE sloupce
# #
# png_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/BFitA/outputs/r"
# # Acanthis cabaret_500_glm_BFitA_5003_4_all.png   glm_BFitA_5001_Aix sponsa_500_4_all.tif
# tibble_grains.reduced.path <- tibble_grains.reduced %>%
#     mutate(path = paste0(prefix, px_size_item, "[0-9]{1,2}_", species, "_", px_size_item, "_4_ndop.tif"))
# # tibble_grains.reduced.path <-tibble_grains.reduced %>%  mutate(path = paste0(species, "_", px_size_item, "_","glm_BFitA", "_", px_size_item, "[0-9]{1,2}_4_gbif.png")) %>% select(path)
# png_list <- list()


# tibble_grains.reduced.path %<>% mutate(rmse = 0.123)

# for (p in seq_along(tibble_grains.reduced.path$path)) {
#     pt <- tibble_grains.reduced.path[p, ]
#     png_list[[pt$path]] <-
#         list.files(
#             path = png_path,
#             # původní: "^glm_fmt_", d, "_.*\\.rds$"
#             # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
#             pattern = paste0("^", pt$path, "$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
#             ignore.case = TRUE,
#             full.names = TRUE
#         )

#     # NDOP
#     # ořez původního raster_stack na ČR pro lokální SDM
#     raster_stack <- raster(png_list[[p]])
#     raster_stack_crop <- crop(raster_stack, extent(czechia))
#     raster_stack_mask_czechia <- mask(raster_stack_crop, czechia)

#     # GBIF
#     # ořez původního raster_stack na ČR pro lokální SDM
#     raster_stack2 <- raster(gsub("_ndop", "_gbif", png_list[[p]]))
#     raster_stack_crop2 <- crop(raster_stack2, extent(czechia))
#     raster_stack_mask_czechia2 <- mask(raster_stack_crop2, czechia)

#     rmse <- rRMSE(raster_stack_mask_czechia, raster_stack_mask_czechia2)

#     tibble_grains.reduced.path[p, ]$rmse <- rmse
# }

# hist(tibble_grains.reduced.path$rmse)

# tibble_grains.reduced.path.zaloha <- tibble_grains.reduced.path

# #
# ### uložení RMSE a pak jeho načtení
# #
# # saveRDS(tibble_grains.reduced.path.zaloha, file = paste0(export_path, "/tibble_grains.reduced.path.zaloha-", prefix, ".rds"))
# # tibble_grains.reduced.path.zaloha <- readRDS(paste0(export_path, "/tibble_grains.reduced.path.zaloha-", prefix, ".rds"))

# stop()

# tibble_grains.reduced <- tibble_grains %>%
#     select_if(~ is.numeric(.) | is.character(.)) %>%
#     filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75)

#
# generování map do PDF
#

tibble_grains.reduced <- tibble_grains %>%
    select_if(~ is.numeric(.) | is.character(.)) %>%
    filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.70 & gbif_ndop.geo.rmse <= 0.20 & gbif_ndop.geo.D >= 0.80) # auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75 & gbif_ndop.geo.rmse <= 0.20 & gbif_ndop.geo.D >= 0.80
# 2km mediany: .cor 0.71; .eps 0.45; rmse 0.19

# NDOP závislost AUC na r.breadth
plot(tibble_grains$ndop.breadth.B2, tibble_grains$auc.ndop.te)
plot(tibble_grains.reduced$ndop.breadth.B2, tibble_grains.reduced$auc.ndop.te)



png_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/LV/outputs/r"
# Acanthis cabaret_500_glm_BFitA_5003_4_all.png   glm_BFitA_5001_Aix sponsa_500_4_all.tif
tibble_grains.reduced.path <- tibble_grains.reduced %>%
    mutate(path = paste0(prefix, px_size_item, "[0-9]{1,2}_", species, "_", px_size_item, "_4_ndop.tif"))
# tibble_grains.reduced.path <-tibble_grains.reduced %>%  mutate(path = paste0(species, "_", px_size_item, "_","glm_BFitA", "_", px_size_item, "[0-9]{1,2}_4_gbif.png")) %>% select(path)
png_list <- list()



#  source("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/Rp/dAUC-orig-base.R", encoding = "UTF-8")

pdf(paste0(export_path, "/check-predictions-", prefix, "_10-0.5_-7728.pdf"), width = 18, height = 5) # -cor-0.75 -rmse-0.17 -D-0.85 -7728
par(mar = c(5, 4, 4, 7))
for (p in seq_along(tibble_grains.reduced.path$path)) {
    pt <- tibble_grains.reduced.path[p, ]
    png_list[[pt$path]] <-
        list.files(
            path = png_path,
            # původní: "^glm_fmt_", d, "_.*\\.rds$"
            # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
            pattern = paste0("^", pt$path, "$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
            ignore.case = TRUE,
            full.names = TRUE
        )

    points <- as.data.frame(st_coordinates(cci_all_3035_czechia %>% filter(species == as.character(pt$species))))

    par(mfrow = c(1, 3))
    palU <- colorRampPalette(c("gray98", "limegreen", "darkgreen"))
    # NDOP
    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack <- raster(png_list[[pt$path]])
    raster_stack_crop <- crop(raster_stack, extent(czechia))
    r.ndop <- raster_stack_mask_czechia <- normalize(raster.standardize(mask(raster_stack_crop, czechia)))
    plot(raster_stack_mask_czechia,
        legend.width = 1, col = palU(20),
        main = paste0(pt$species, " | NDOP, AUC=", round(pt$auc.ndop.te, digits = 2), " (", (pt$px_size_item / 1000), "km)"),
        sub = paste0("NDOP/GBIF: ", pt$ndop_c, "/", pt$gbif_c, " = ", round(1 / pt$gbif_ndop_rate * 100), "% | adj: ", pt$bvn_gbif, " | r.breadth.B2: ", round(pt$ndop.breadth.B2, digits = 2), "/", round(pt$gbif_crop.breadth.B2, digits = 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "darkgray", pch = 0)
    par()
    points(points, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), pch = 16, cex = 0.5)



    # GBIF
    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack <- raster(gsub("_ndop", "_gbif", png_list[[pt$path]]))
    raster_stack_crop <- crop(raster_stack, extent(czechia))
    r.gbif <- raster_stack_mask_czechia <- normalize(raster.standardize(mask(raster_stack_crop, czechia)))
    plot(raster_stack_mask_czechia,
        legend.width = 1, col = palU(20),
        main = paste0(pt$species, " | GBIF (centr. Europe) bias corrected predictions to CZ, (", (pt$px_size_item / 1000), "km)"),
        sub = paste0("geo.cor (Spearman): ", round(pt$gbif_ndop.geo.cor, digits = 2), " | geo.D (Schoener): ", round(pt$gbif_ndop.geo.D, digits = 2), " | RMSE: ", round(pt$gbif_ndop.geo.rmse, digits = 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "darkgray", pch = 0)
    par()
    points(points, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), pch = 16, cex = 0.5)


    # DIFF
    # pal <- colorRampPalette(c("red", "orange", "white", "lightblue", "blue"))
    # pal <- colorRampPalette(c("red", "orange", "forestgreen", "lightblue", "blue"))
    # pal <- colorRampPalette(c("red", "orange", "limegreen", "darkgreen", "limegreen", "deepskyblue3", "blue"))
    pal <- colorRampPalette(c("red", "orange", "gray98", "gray98", "deepskyblue3", "blue"))
    ## plot(r.ndop.n - r.gbif.n,  horizontal = TRUE, col = pal(50))
    pal_n <- 20


    r.n.diff <- r.ndop - r.gbif

    # r.max <- abs(maxValue(r.n.diff))
    # r.min <- abs(minValue(r.n.diff))
    # if(r.max < r.min){
    # r.max <- r.min
    # }

    r.max <- 1.0

    plot(r.n.diff,
        col = pal(pal_n), breaks = round(seq(-r.max, r.max, length.out = pal_n + 1), 2), legend.width = 1,
        main = paste0("difference (NDOP - GBIF)")
        # sub = paste0("geo.cor (Spearman): ", round(pt$gbif_ndop.geo.cor, digits = 2), " | geo.D (Schoener): ", round(pt$gbif_ndop.geo.D, digits = 2), " | RMSE: ", round(pt$gbif_ndop.geo.rmse, digits = 2), " | EPS: ", round(pt$gbif_ndop.geo.eps, digits = 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "darkgray", pch = 0)
}

dev.off()


png_list <- unname(unlist(png_list))


pxs_size <- tibble_grains %>% distinct(px_size_item)
