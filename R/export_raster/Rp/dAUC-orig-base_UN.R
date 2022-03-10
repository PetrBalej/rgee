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

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/UN"

# pomocné funkce
source(paste0(wd, "/R/export_raster/functions.R"))
source(paste0(wd, "/R/export_raster/multiply-metrics-selection-cycle.R"))

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
cci_all_3035_czechia <- readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom-minimum/inputs/occurrences/cci_all_3035_czechia.rds")

prefix <- "glm_UN_" # "OWNPFr" "maxent_thr" "glm_GB_"

if (file.exists(paste0(export_path, "/outputs/tibble_grains-", prefix, ".rds"))) {
    tibble_grains <- readRDS(file = paste0(export_path, "/outputs/tibble_grains-", prefix, ".rds"))
} else {
    tibble_grains <- join_outputs_rds(export_path, prefix)

    ### uložit do rds a csv
    # saveRDS(tibble_grains, file = paste0(export_path, "/outputs/tibble_grains-",prefix,".rds"))
    # write_csv(tibble_grains %>% select_if(~ is.numeric(.) | is.character(.)), paste0(export_path, "/outputs/tibble_grains-",prefix,".csv"))
}


# tibble_grains_c <- tibble_grains  %>%  select(px_size_item, bvn_gbif, auc.ndop.te, gbif_ndop.geo.cor, gbif_ndop.geo.rmse, gbif_ndop.geo.D)
# tibble_grains_c  %<>%
#     filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.70 & gbif_ndop.geo.rmse <= 0.20 & gbif_ndop.geo.D >= 0.80 & px_size_item == 2000)

# png(file="optimal_kernel.png")
# hist(as.numeric(tibble_grains_c$bvn_gbif), breaks=5, xlab = "kernel bandwidth (adjust 0.5 = 6.5 km; 1.0 = 13km)", ylab="species", main="optimal kernel smoothing bandwidth (sigma) for bias raster")
# dev.off()


tibble_grains_numeric <- tibble_grains %>% select_if(., is.numeric) # pro základní deskriptivní statistiku


mm <- mm(wd, export_path, pxs = c(500, 1000, 2000, 5000, 10000))

# výběr "ideální" predikce podle maximální hodnoty metriky pro příslušnou kernel width
mtype <- "md" # md, cor, I, rmse

tibble_grains.reduced1 <- mm %>%
    filter(ndop.auc >= 0.70 & md.max > 0.53)

tibble_grains.reduced2 <- tibble_grains %>%
    select_if(~ is.numeric(.) | is.character(.))

tibble_grains.reduced <- tibble_grains.reduced1 %>%
    left_join(tibble_grains.reduced2, by = c("species" = "species", "px" = "px_size_item"), suffix = c("", "_nj")) %>%
    arrange(desc(md.max))



# # NDOP závislost AUC na r.breadth
# plot(tibble_grains$ndop.breadth.B2, tibble_grains$auc.ndop.te)
# plot(tibble_grains.reduced$ndop.breadth.B2, tibble_grains.reduced$auc.ndop.te)



png_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/UN/outputs/r"
# Acanthis cabaret_500_glm_BFitA_5003_4_all.png   glm_BFitA_5001_Aix sponsa_500_4_all.tif
tibble_grains.reduced.path <- tibble_grains.reduced %>%
    mutate(path = paste0(prefix, px, "[0-9]{1,2}_", species, "_", px, "_4_ndop.tif"))
# tibble_grains.reduced.path <-tibble_grains.reduced %>%  mutate(path = paste0(species, "_", px_size_item, "_","glm_BFitA", "_", px_size_item, "[0-9]{1,2}_4_gbif.png")) %>% select(path)
png_list <- list()



#  source("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/Rp/dAUC-orig-base.R", encoding = "UTF-8")

#
# generování map do PDF
#
pdf(paste0(export_path, "/pdf/check-predictions-", prefix, "_10-0.5_-auc0.7-", mtype, ".pdf"), width = 30, height = 5) # -cor-0.75 -rmse-0.17 -D-0.85 -7728
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

    # dofiltrované 100m
    points <- per_pixel_df(as.data.frame(st_coordinates(cci_all_3035_czechia %>% filter(species == as.character(pt$species)))), 100)

    par(mfrow = c(1, 5))
    palU <- colorRampPalette(c("gray98", "limegreen", "darkgreen"))
    # NDOP
    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack <- raster(png_list[[pt$path]])
    raster_stack_crop <- crop(raster_stack, extent(czechia))
    r.ndop <- raster_stack_mask_czechia <- normalize(raster.standardize(mask(raster_stack_crop, czechia)))
    plot(raster_stack_mask_czechia,
        legend.width = 1, col = palU(20),
        main = paste0(pt$species, " | NDOP, AUC=", round(pt$ndop.auc, digits = 2), " (", (pt$px / 1000), "km)"),
        sub = paste0(
            "NDOP/NDOP100/GBIF: ", pt$ndop_c.f, "/", pt$ndop_c_thin, "/", pt$gbif_c.f, " \nadj. kernel width/max (mm|cor|I|RMSE): ",
            pt$md, "/", round(pt$md.max, digits = 2), " | ",
            pt$cor, "/", round(pt$cor.max, digits = 2), " | ",
            pt$i, "/", round(pt$i.max, digits = 2), " | ",
            pt$rmse, "/", round(pt$rmse.max, digits = 2)
        )
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


    r <-
        list.files(
            path = paste0(export_path, "/outputs/r"),
            # původní: "^glm_fmt_", d, "_.*\\.rds$"
            # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
            pattern = paste0("^", tail(strsplit(gsub("_ndop.tif", paste0("_gbif_ideal-", mtype, "_[0-2]\\.[0-9][0-9]\\.tif"), png_list[[pt$path]]), split = "/")[[1]], n = 1), "$"),
            ignore.case = TRUE,
            full.names = TRUE
        )


    raster_stack <- raster(r)
    raster_stack_crop <- crop(raster_stack, extent(czechia))
    r.gbif <- raster_stack_mask_czechia <- normalize(raster.standardize(mask(raster_stack_crop, czechia)))
    plot(raster_stack_mask_czechia,
        legend.width = 1, col = palU(20),
        main = paste0(pt$species, " | GBIF (centr. Europe) bias corrected predictions to CZ, (", (pt$px / 1000), "km)"),
        sub = paste0("(mm maxs) geo.cor (Spearman): ", round(pt$md.cor.max, digits = 2), " | geo.I (Warren): ", round(pt$md.i.max, digits = 2), " | RMSE: ", round(pt$md.rmse.max, digits = 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "darkgray", pch = 0)
    par()
    points(points, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5), pch = 16, cex = 0.5)


    # confusion matrix pro páry výsledných rasterů
    # beru 75 % kvartil
    threshold.ndop <- quantile(r.ndop)[4]
    threshold.gbif <- quantile(r.gbif)[4]
    r.ndop.pa <- r.ndop
    r.gbif.pa <- r.gbif

    r.ndop.pa[r.ndop.pa < threshold.ndop] <- 0
    r.ndop.pa[r.ndop.pa >= threshold.ndop] <- 1

    r.gbif.pa[r.gbif.pa < threshold.gbif] <- 0
    r.gbif.pa[r.gbif.pa >= threshold.gbif] <- 1

    cm <- rasters_confusion(r.ndop.pa, r.gbif.pa)

    # DIFF
    # pal <- colorRampPalette(c("red", "orange", "white", "lightblue", "blue"))
    # pal <- colorRampPalette(c("red", "orange", "forestgreen", "lightblue", "blue"))
    # pal <- colorRampPalette(c("red", "orange", "limegreen", "darkgreen", "limegreen", "deepskyblue3", "blue"))
    pal <- colorRampPalette(rev(c("red", "orange", "gray98", "gray98", "deepskyblue3", "blue")))
    ## plot(r.ndop.n - r.gbif.n,  horizontal = TRUE, col = pal(50))
    pal_n <- 20


    r.n.diff <- r.gbif - r.ndop
    r.n.diff.s <- raster.standardize(r.gbif) - raster.standardize(r.ndop)

    # r.max <- abs(maxValue(r.n.diff))
    # r.min <- abs(minValue(r.n.diff))
    # if(r.max < r.min){
    # r.max <- r.min
    # }

    r.max <- 1.0

    r.n.diff.s.na <- !is.na(r.n.diff.s[])
    r.n.diff.na <- !is.na(r.n.diff[])

    total_over_under_prediction.s.sum <- cellStats(abs(r.n.diff.s), stat = sum)
    total_over_under_prediction.sum <- cellStats(abs(r.n.diff), stat = sum)

    total_over_under_prediction.s <- round(total_over_under_prediction.s.sum / ncell(r.n.diff.s.na), 8)
    total_over_under_prediction <- round(total_over_under_prediction.sum / ncell(r.n.diff.na), 3)

    plot(r.n.diff,
        # col = rev(topo.colors(20)),
        col = pal(pal_n), breaks = round(seq(-r.max, r.max, length.out = pal_n + 1), 2), legend.width = 1,
        main = paste0("difference (GBIF - NDOP) ", round(total_over_under_prediction.s.sum, 3), " | ", total_over_under_prediction),
        sub = paste0("geo.cor (Spearman): ", round(pt$cor.max, digits = 2), " | geo.I (Warren): ", round(pt$i.max, digits = 2), " | RMSE: ", round(pt$rmse.max, digits = 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "darkgray", pch = 0)


    # PA NDOP
    plot(r.ndop.pa,
        # col = rev(topo.colors(20)),
        # col = pal(pal_n), breaks = round(seq(-r.max, r.max, length.out = pal_n + 1), 2),
        legend.width = 1,
        main = paste0("presence/absence NDOP"),
        sub = paste0("75 % kvartil: thrNDOP=", round(threshold.ndop, 2), ", thrGBIF=", round(threshold.gbif, 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "darkgray", pch = 0)

    # PA GBIF
    plot(r.gbif.pa,
        # col = rev(topo.colors(20)),
        # col = pal(pal_n), breaks = round(seq(-r.max, r.max, length.out = pal_n + 1), 2),
        legend.width = 1,
        main = paste0("presence/absence GBIF"),
        sub = paste0("Sorensen=", round(cm$Sorensen, 3), "; TSS=", round(cm$TSS, 3), "; Sens=", round(cm$Sensitivity, 3), "; Spec=", round(cm$Specificity, 3), "; OPR=", round(cm$OPR, 3), ", UPR=", round(cm$UPR, 3))
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