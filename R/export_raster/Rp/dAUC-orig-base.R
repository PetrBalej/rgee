# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra", "viridis")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# vede na git rgee
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/pdf_statsIGAprelim2"

pairs <- list(

    # # auc
    c("auc.all.te", "auc.gbif.te", "auc.ndop.te"),
    c("TSS.all", "TSS.gbif", "TSS.ndop"),
    c("F_measure.all", "F_measure.gbif", "F_measure.ndop"),
    c("Jaccard.all", "Jaccard.gbif", "Jaccard.ndop"),
    c("sedi.all", "sedi.gbif", "sedi.ndop"),
    c("TSS.ndop_gbif", "TSS.gbif_ndop", "TSS.ndop_all"),
    c("TSS.all_gbif", "TSS.all_gbif_erase", "TSS.all_gbif_erase"),
    c("F_measure.gbif_ndop", "F_measure.ndop_all", "F_measure.ndop_all"),
    c("F_measure.all_gbif", "F_measure.all_gbif_erase", "F_measure.all_gbif_erase"),
    c("Jaccard.ndop_gbif", "Jaccard.gbif_ndop", "Jaccard.ndop_all"),
    c("Jaccard.all_gbif", "Jaccard.all_gbif_erase", "Jaccard.all_gbif_erase"),

    # https://borea.mnhn.fr/sites/default/files/pdfs/2018%20Leroy%20et%20al%20-%20Journal%20of%20Biogeography.pdf
    c("Sorensen.ndop_gbif", "Sorensen.gbif_ndop", "Sorensen.ndop_all"),
    c("OPR.ndop_gbif", "OPR.gbif_ndop", "OPR.ndop_all"),
    c("UPR.ndop_gbif", "UPR.gbif_ndop", "UPR.ndop_all"),

    c("Sensitivity.ndop_gbif", "Sensitivity.gbif_ndop", "Sensitivity.ndop_all"),
    c("Sensitivity.all_gbif", "Sensitivity.all_gbif_erase", "Sensitivity.all_gbif_erase"),
    c("Specificity.ndop_gbif", "Specificity.gbif_ndop", "Specificity.ndop_all"),
    c("Specificity.all_gbif", "Specificity.all_gbif_erase", "Specificity.all_gbif_erase"),
    #  c("auc.all.boyce", "auc.gbif.boyce", "auc.ndop.boyce"), # boyce je k ničemu, kritizovali hi jako neschopný rozlišit rozdíly v jednom článku - yru3it enmtools.calibrate? nebo udělat recalibrate?
    # # geogr. overlap
    c("gbif_all.geo.D", "gbif_all_erase.geo.D", "gbif_ndop.geo.D"),
    c("gbif_all.geo.I", "gbif_all_erase.geo.I", "gbif_ndop.geo.I"),
    c("gbif_all.geo.cor", "gbif_all_erase.geo.cor", "gbif_ndop.geo.cor"),
    # # env. overlap
    c("gbif_all.env.D", "gbif_ndop.env.D", "all_ndop.env.D"),
    c("gbif_all.env.I", "gbif_ndop.env.I", "all_ndop.env.I"),
    c("gbif_all.env.cor", "gbif_ndop.env.cor", "all_ndop.env.cor"),
    # # geogr šířka niky
    c("gbif.breadth.B1", "all.breadth.B1", "ndop.breadth.B1"),
    c("gbif.breadth.B2", "all.breadth.B2", "ndop.breadth.B2"),

    c("gbif.breadth.B1", "all.breadth.B1", "all.breadth.B1"),
    c("gbif.breadth.B2", "all.breadth.B2", "all.breadth.B2"),

    c("gbif_erase.breadth.B1", "all_erase.breadth.B1", "all_erase.breadth.B1"),
    c("gbif_erase.breadth.B2", "all_erase.breadth.B2", "all_erase.breadth.B2"),

    c("gbif_crop.breadth.B1", "all_crop.breadth.B1", "ndop.breadth.B1"),
    c("gbif_crop.breadth.B2", "all_crop.breadth.B2", "ndop.breadth.B2"),

    # # env šířka niky

    c("gbif.ebreadth.B2", "ndop.ebreadth.B2", "all.ebreadth.B2"),

    # # permut. performance prediktorů - poměr WB : L8
    c("wcl8.gbif", "wcl8.all", "wcl8.ndop")
    # c("wcl8_perc.gbif", "wcl8_perc.all", "wcl8_perc.ndop") # cca 30% prišpívají, ale je třeba to rozdělit na samostatné 3 modely (stejný treshold): WC, L8 a WC+L8
)

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

prefix <- "glm_BFitA_" # "OWNPFr" "maxent_thr" "glm_GB_"

rds_list <-
    list.files(
        path = paste0(export_path, "/../vse-v-jednom/glm-prelim2/outputs/rds"), # vse-v-jednom/outputs/rds/
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
        filter(species %notin% nepuvodni) %>%
        filter(species %notin% problematicke)


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

tibble_grains %<>% mutate(wc.gbif = rowSums(dplyr::select(., matches("^wc_.*\\.Imp\\.gbif$"))))
tibble_grains %<>% mutate(wc.all = rowSums(dplyr::select(., matches("^wc_.*\\.Imp\\.all$"))))
tibble_grains %<>% mutate(wc.ndop = rowSums(dplyr::select(., matches("^wc_.*\\.Imp\\.ndop$"))))

tibble_grains %<>% mutate(l8.gbif = rowSums(dplyr::select(., matches("^l8_.*\\.Imp\\.gbif$"))))
tibble_grains %<>% mutate(l8.all = rowSums(dplyr::select(., matches("^l8_.*\\.Imp\\.all$"))))
tibble_grains %<>% mutate(l8.ndop = rowSums(dplyr::select(., matches("^l8_.*\\.Imp\\.ndop$"))))


tibble_grains %<>% mutate(wcl8.gbif = (wc.gbif / l8.gbif))
tibble_grains %<>% mutate(wcl8.all = (wc.all / l8.all))
tibble_grains %<>% mutate(wcl8.ndop = (wc.ndop / l8.ndop))

tibble_grains %<>% mutate(wcl8_perc.gbif = ((l8.gbif * 100) / wc.gbif))
tibble_grains %<>% mutate(wcl8_perc.all = ((l8.all * 100) / wc.all))
tibble_grains %<>% mutate(wcl8_perc.ndop = ((l8.ndop * 100) / wc.ndop))


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

tibble_grains.reduced <- tibble_grains %>%
    select_if(~ is.numeric(.) | is.character(.)) %>%
    filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75)
# tibble_grains %>%  filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.D >= 0.80)
# write_csv(tibble_grains %>% select_if(~ is.numeric(.) | is.character(.)), "tibble_grains.glm.csv")
tibble_grains.reduced %>% filter(px_size_item == 10000)

# NDOP závislost AUC na r.breadth
plot(tibble_grains$ndop.breadth.B2, tibble_grains$auc.ndop.te)
plot(tibble_grains.reduced$ndop.breadth.B2, tibble_grains.reduced$auc.ndop.te)


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








#
# generování mapky
#
png_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/glm-prelim2/r"
# Acanthis cabaret_500_glm_BFitA_5003_4_all.png   glm_BFitA_5001_Aix sponsa_500_4_all.tif
tibble_grains.reduced.path <- tibble_grains.reduced %>%
    mutate(path = paste0("glm_BFitA", "_", px_size_item, "[0-9]{1,2}_", species, "_", px_size_item, "_4_ndop.tif"))
# tibble_grains.reduced.path <-tibble_grains.reduced %>%  mutate(path = paste0(species, "_", px_size_item, "_","glm_BFitA", "_", px_size_item, "[0-9]{1,2}_4_gbif.png")) %>% select(path)
png_list <- list()


pdf(paste0(export_path, "/check-predictions-NDOP_GBIF-cor-0.75.pdf"), width = 15, height = 7)
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

    par(mfrow = c(1, 2))

    # NDOP
    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack <- raster(png_list[[p]])
    raster_stack_crop <- crop(raster_stack, extent(czechia))
    raster_stack_mask_czechia <- mask(raster_stack_crop, czechia)
    plot(raster_stack_mask_czechia,
        main = paste0(pt$species, " | NDOP, AUC=", round(pt$auc.ndop.te, digits = 2), " (", (pt$px_size_item / 1000), "km)"),
        sub = paste0("NDOP/GBIF: ", pt$ndop_c, "/", pt$gbif_c, " = ", round(1 / pt$gbif_ndop_rate * 100), "% | adj: ", pt$bvn_ndop, "/", pt$bvn_gbif, " | r.breadth.B2: ", round(pt$ndop.breadth.B2, digits = 2), "/", round(pt$gbif.breadth.B2, digits = 2), " (crop: ", round(pt$gbif_crop.breadth.B2, digits = 2), ")")
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "red", pch = 0)
    par()
    points(points, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3), cex = 0.5)



    # GBIF
    # ořez původního raster_stack na ČR pro lokální SDM
    raster_stack <- raster(gsub("_ndop", "_gbif", png_list[[p]]))
    raster_stack_crop <- crop(raster_stack, extent(czechia))
    raster_stack_mask_czechia <- mask(raster_stack_crop, czechia)
    plot(raster_stack_mask_czechia,
        main = paste0(pt$species, " | GBIF, (AUC=", round(pt$auc.gbif.te, digits = 2), ") (", (pt$px_size_item / 1000), "km)"),
        sub = paste0("geo.cor (Spearman): ", round(pt$gbif_ndop.geo.cor, digits = 2), " | geo.D (Schoener): ", round(pt$gbif_ndop.geo.D, digits = 2))
    )
    par(bg = NA)
    plot(rivers_cz$geometry, add = TRUE, col = "blue")
    par()
    plot(czechia$geometry, add = TRUE)
    par()
    plot(places_cz$geometry, add = TRUE, col = "red", pch = 0)
    par()
    points(points, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3), cex = 0.5)
}
dev.off()




png_list <- unname(unlist(png_list))


stop()

pxs_size <- tibble_grains %>% distinct(px_size_item)


for (pxs in pxs_size$px_size_item) {

}
# stop()

# # kontrola
# tibble  %>% dplyr::select(species, vip1.gbif)
# tibble_result %>% dplyr::select(species,  l8_3.5_10000_MNDWI.Importance.gbif) # vip1.gbif    l8_6.8_10000_EVI.Importance_gbif   enm_mxt_gbif.vip_gbif   vip3.gbif_gbif



for (p in pairs) {
    title <- paste0(p[1], "+", p[2], "+", p[3])
    appendix <- paste0(p[1], "+", p[2], "+", p[3])

    # title <- paste0(p[1], "/", p[2])
    # appendix <- paste0(p[1], "-", p[2])



    # read AUC table
    # dfAUCorig <- read.csv2("AUCall100.csv", header = TRUE)

    # dfAUCorig <- read_csv(paste0(wd, "/../export/schuzka2-total-gbif-ndop6/topX-final.csv"))
    # dfAUCorig_10000 <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/gOverlap_10000.csv")
    # dfAUCorig_1000 <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/gOverlap_1000.csv")

    # dfAUCorig <- dfAUCorig_10000 %>% add_row(dfAUCorig_1000)

    dfAUCorig <- tibble_grains


    # dfAUCorig_count <- as.integer(count(dfAUCorig) / 4)
    dfAUCorig_count <- as.integer(count(dfAUCorig) / 4) * 4


    auc_limit <- 0.75
    typ_filtrace <- "NDOP" # "GBIF_ALL" "GBIF_NDOP" "ALL_NDOP" "NDOP" "GBIF"

    if (typ_filtrace == "GBIF_ALL") {
        dfAUCfiltr <- dfAUCorig %>%
            filter(auc.all.te >= auc_limit) %>%
            filter(auc.gbif.te >= auc_limit)
    }

    if (typ_filtrace == "GBIF_NDOP") {
        dfAUCfiltr <- dfAUCorig %>%
            filter(auc.ndop.te >= auc_limit) %>%
            filter(auc.gbif.te >= auc_limit)
    }

    if (typ_filtrace == "ALL_NDOP") {
        dfAUCfiltr <- dfAUCorig %>%
            filter(auc.ndop.te >= auc_limit) %>%
            filter(auc.all.te >= auc_limit)
    }

    if (typ_filtrace == "NDOP") {
        dfAUCfiltr <- dfAUCorig %>%
            filter(auc.ndop.te >= auc_limit)
    }

    if (typ_filtrace == "GBIF") {
        dfAUCfiltr <- dfAUCorig %>%
            filter(auc.gbif.te >= auc_limit)
    }



    # read bird traits table (traits according to Kolecek et al 2010)
    dftraits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")
    glimpse(dftraits) # check data

    # join bird traits to dfAUC
    joined_traits <- dfAUCfiltr %>%
        left_join(dftraits, by = c("species" = "species")) %>%
        filter(!is.na(Habitat))


    c_orig <- as_tibble(dfAUCorig %>% group_by(px_size_item) %>% count(px_size_item))
    c_filter <- as_tibble(dfAUCfiltr %>% group_by(px_size_item) %>% count(px_size_item))
    reduction <- as_tibble(c_orig %>% mutate(n_filter = c_filter$n) %>% mutate(perc = (n_filter * 100 / n)))


    ### stačí předat jen vektor - nemusím pokaždé specifikovat počet!
    # df %>% gather("key", "value", x, y, z) is equivalent to df %>% pivot_longer(c(x, y, z), names_to = "key", values_to = "value")

    joined_traits_question <- joined_traits %>% gather(p[1], p[2], p[3], key = "Question", value = "dAUC")

    cnt.all <- count(dfAUCorig)
    cnt.filtr <- count(dfAUCfiltr)
    cnt.perc <- (cnt.filtr * 100) / cnt.all
    caption <- paste0("AUC > ", auc_limit)


    # df <- df1 %>% add_row(df2)
    df <- joined_traits_question
    glimpse(df) # check data





    # , "auc.all.te", "auc.gbif.te", "auc.ndop.te","gbif_all.geo.D", "gbif_all_erase.geo.D", "gbif_ndop.geo.D","gbif_all.env.D", "gbif_all.env.I", "gbif_all.env.I"
    frequency <- c("ndop_c", "gbif_c", "gbif_ndop_rate", "gbif_ndop_perc")
    for (f in frequency) {

    }
}