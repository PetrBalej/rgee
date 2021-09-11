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

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/LV"

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

if (file.exists(paste0(export_path, "/outputs/tibble_grains-", prefix, ".rds"))) {
    tibble_grains <- readRDS(file = paste0(export_path, "/outputs/tibble_grains-", prefix, ".rds"))
} else {
    tibble_grains <- join_outputs_rds(export_path, prefix)

    ### uložit do rds a csv
    # saveRDS(tibble_grains, file = paste0(export_path, "/outputs/tibble_grains-",prefix,".rds"))
    # write_csv(tibble_grains %>% select_if(~ is.numeric(.) | is.character(.)), paste0(export_path, "/outputs/tibble_grains-",prefix,".csv"))
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

pdf(paste0(export_path, "/pdf/check-predictions-", prefix, "_10-0.5_-7728.pdf"), width = 18, height = 5) # -cor-0.75 -rmse-0.17 -D-0.85 -7728
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
    r.n.diff.s <- raster.standardize(r.ndop) - raster.standardize(r.gbif)

    # r.max <- abs(maxValue(r.n.diff))
    # r.min <- abs(minValue(r.n.diff))
    # if(r.max < r.min){
    # r.max <- r.min
    # }

    r.max <- 1.0

    # r.n.diff.s.na <- !is.na(r.n.diff.s[])
    # r.n.diff.na <- !is.na(r.n.diff[])

    # total_over_under_prediction.s.sum <- cellStats(abs(r.n.diff.s), stat = sum)
    # total_over_under_prediction.sum <- cellStats(abs(r.n.diff), stat = sum)

    # total_over_under_prediction.s <- round(total_over_under_prediction.s.sum / ncell(r.n.diff.s.na), 8)
    # total_over_under_prediction <- round(total_over_under_prediction.sum / ncell(r.n.diff.na), 2)

    plot(r.n.diff,
        # col = rev(topo.colors(20)),
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
