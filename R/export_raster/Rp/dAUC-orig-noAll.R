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

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra", "viridis")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# vede na git rgee
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/pdf_statsIGA-BFitA"

pairs <- list(

    # # auc
    c("auc.ndop.te", "auc.gbif.te", "auc.ndop.te"),
    c("TSS.ndop", "TSS.gbif", "TSS.ndop"),
    c("F_measure.ndop", "F_measure.gbif", "F_measure.ndop"),
    c("Jaccard.ndop", "Jaccard.gbif", "Jaccard.ndop"),
    c("sedi.ndop", "sedi.gbif", "sedi.ndop"),
    c("TSS.ndop_gbif", "TSS.gbif_ndop", "TSS.gbif_ndop"),

    c("F_measure.gbif_ndop", "F_measure.gbif_ndop", "F_measure.gbif_ndop"),

    c("Jaccard.ndop_gbif", "Jaccard.gbif_ndop", "Jaccard.gbif_ndop"),


    # https://borea.mnhn.fr/sites/default/files/pdfs/2018%20Leroy%20et%20al%20-%20Journal%20of%20Biogeography",suffix_pdf,".pdf
    c("Sorensen.ndop_gbif", "Sorensen.gbif_ndop", "Sorensen.gbif_ndop"),
    c("OPR.ndop_gbif", "OPR.gbif_ndop", "OPR.gbif_ndop"),
    c("UPR.ndop_gbif", "UPR.gbif_ndop", "UPR.gbif_ndop"),

    c("Sensitivity.ndop_gbif", "Sensitivity.gbif_ndop", "Sensitivity.gbif_ndop"),

    c("Specificity.ndop_gbif", "Specificity.gbif_ndop", "Specificity.gbif_ndop"),

    #  c("auc.gbif.boyce", "auc.gbif.boyce", "auc.ndop.boyce"), # boyce je k ničemu, kritizovali hi jako neschopný rozlišit rozdíly v jednom článku - yru3it enmtools.calibrate? nebo udělat recalibrate?
    # # geogr. overlap
    c("gbif_ndop.geo.D", "gbif_ndop.geo.D", "gbif_ndop.geo.D"),
    c("gbif_ndop.geo.I", "gbif_ndop.geo.I", "gbif_ndop.geo.I"),
    c("gbif_ndop.geo.cor", "gbif_ndop.geo.cor", "gbif_ndop.geo.cor"),
    # # env. overlap


    # # geogr šířka niky
    c("gbif.breadth.B1", "gbif.breadth.B1", "ndop.breadth.B1"),
    c("gbif.breadth.B2", "gbif.breadth.B2", "ndop.breadth.B2"),

    c("gbif.breadth.B1", "gbif.breadth.B1", "gbif.breadth.B1"),
    c("gbif.breadth.B2", "gbif.breadth.B2", "gbif.breadth.B2"),

    c("gbif_erase.breadth.B1", "ndop.breadth.B1", "ndop.breadth.B1"),
    c("gbif_erase.breadth.B2", "ndop.breadth.B2", "ndop.breadth.B2"),

    c("gbif_crop.breadth.B1", "ndop.breadth.B1", "ndop.breadth.B1"),
    c("gbif_crop.breadth.B2", "ndop.breadth.B2", "ndop.breadth.B2"),
    # # geogr šířka niky - cropnutý GBIF
    c("gbif.breadth.B2", "gbif_crop.breadth.B2", "ndop.breadth.B2"),
    # # env šířka niky

    c("gbif.ebreadth.B2", "ndop.ebreadth.B2", "gbif.ebreadth.B2"),

    # # permut. performance prediktorů - poměr WB : L8
    c("wcl8.gbif", "wcl8.ndop", "wcl8.ndop")
    # c("wcl8_perc.gbif", "wcl8_perc.ndop", "wcl8_perc.ndop") # cca 30% prišpívají, ale je třeba to rozdělit na samostatné 3 modely (stejný treshold): WC, L8 a WC+L8
)

# # # průměrna diverzita habitat suitability
#  all_species_list <-
#   list.files(
#     path = "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/",
#     pattern = paste0("^glmE[0-9]_.+_5000_3_ndop\\.tif$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"
#     ignore.case = TRUE,
#     full.names = TRUE
#   )
#         all_species <- stack(sapply(all_species_list, function(x) raster(x)))
#         all_species_mean <- calc(all_species, fun = mean, na.rm=TRUE)
#   writeRaster(all_species_mean, paste0(export_path, "habitat-suitability-diversity.tif"), format = "GTiff", overwrite = TRUE)

# stop()


# dfe500 <- append(append(
#   append(
#     readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_glm4Qr_500_1_1621157991.74236.rds"),
#     readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_glm4Qr_500_1_1621152134.96594.rds")
#   ),
#   readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_glm4Qr_500_1_1621152060.20922.rds")
# ), readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_glm4Qr_500_1_1621150566.15917.rds"))

# dfe0 <- append(
#   append(
#     readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_5000_1_1621012083.67113.rds"),
#     readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_10000_3_1621016560.79906.rds")
#   ),
#   readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_1000_1_1621022574.0087.rds")
# )


# enmsr_glmE1_1000_1_1621239440.03447.rds

prefix <- "glm_BFitA_" # "OWNPFr" "maxent_thr" "glm_GB_"

rds_list <-
    list.files(
        path = paste0(export_path, "/../vse-v-jednom/BFitA/outputs/rds"), # vse-v-jednom/outputs/rds/
        pattern = paste0("^enmsr_", prefix, ".+\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
        ignore.case = TRUE,
        full.names = TRUE
    )


rds_append <- readRDS(rds_list[[1]])
rds_list <- rds_list[-1]
for (i in seq_along(rds_list)) {
    rds_append <- append(rds_append, readRDS(rds_list[[i]]))
}



# dfe <- list(
#  "5000" = readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_5000_1_1621012083.67113.rds"),
#  "10000" = readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_10000_3_1621016560.79906.rds"),
#  "1000" = readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_1000_1_1621022574.0087.rds"))
# dfe <-
# readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/rds/enmsr_1000_1_1621022574.0087.rds")

# dfAUCorig_10000 <- dfe[["10000"]]

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



    ##### nejdříve seřadit podle abecedy a až pak tomu dát klíč - pomůže to?
    # tibble_ordered <- tibble %>%  arrange(desc(species))  %>% mutate(id = row_number(), .before= species)
    # tibble_gbif <- tibble_ordered %>% expand(vip1.gbif) %>% rename_all(paste0, "_gbif") %>% mutate(id = row_number(), .after = 0)
    # tibble_all <- tibble_ordered %>% expand(vip1.ndop) %>% rename_all(paste0, "_all") %>% mutate(id = row_number(), .after = 0)
    # tibble_ndop <- tibble_ordered %>% expand(vip1.ndop) %>% rename_all(paste0, "_ndop") %>% mutate(id = row_number(), .after = 0)

    # tibble_result  <- tibble_ordered %>% inner_join(tibble_gbif, by = "id") %>% inner_join(tibble_all, by = "id") %>% inner_join( tibble_ndop, by = "id")

    # tibble_ordered  %>% dplyr::select(id, species, vip1.ndop)
    # tibble_result %>% dplyr::select(species,  l8_6.8_10000_EVI.Importance_gbif) # vip1.gbif    l8_6.8_10000_EVI.Importance_gbif   enm_mxt_gbif.vip_gbif   vip3.gbif_gbif



    ## # # # # #  možná bych ani nemusel hodnotu průměrovat už v modelu, ale až tady!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # tibble_ordered %>% summarize(vip1.ndop)
    tibble %<>% tibble %>% group_by(species, px_size_item) # sp = paste(species, px_size_item)

    tibble_gbif <- tibble %>%
        summarize(vip1.gbif, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".gbif")

    tibble_ndop <- tibble %>%
        summarize(vip1.ndop, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".ndop")




    tibble_gbif_p <- tibble %>%
        summarize(perf.gbif, .groups = "keep") %>%
        rename_all(gsub, pattern = "_\\d+_", replacement = "_") %>%
        rename_all(paste0, ".gbif")
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



    tibble_result <- tibble %>%
        left_join(tibble_gbif, by = c("species" = "species.gbif")) %>%
        left_join(tibble_ndop, by = c("species" = "species.ndop")) %>%
        left_join(tibble_gbif_p, by = c("species" = "species.gbif")) %>%
        left_join(tibble_ndop_p, by = c("species" = "species.ndop")) %>%
        left_join(tibble.gbif_ndop.pa, by = c("species" = "species.gbif_ndop")) %>%
        left_join(tibble.ndop_gbif.pa, by = c("species" = "species.ndop_gbif")) %>%
        ungroup() %>%
        dplyr::select(-contains("vip")) %>%
        dplyr::select(-contains("perf.")) %>%
        dplyr::select(-contains(".pa"))



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



auc_limit <- 0.70
# auc_limit - nové limity obecně na všechno
tibble_grains %<>%
    filter(auc.ndop.te >= auc_limit & gbif_ndop.geo.cor >= 0.75) # auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75

suffix_pdf <- paste0("-cor075-", prefix, "-10-1")

give.n <- function(x) {
    # pro zobrazení počtu nálezů nad boxploty
    return(c(y = median(x) * 1.05, label = length(x)))
}


# glimpse(tibble_grains %>% dplyr::select(contains(".ebreadth.")))

pdf(paste0(export_path, "/", prefix, "auc_variable_permutation_importance", suffix_pdf, ".pdf"))

### AUC
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
auc <- tibble_grains %>% pivot_longer(
    cols = c("auc.ndop.te", "auc.gbif.te", "auc.ndop.te"),
    names_to = "auc_type",
    names_prefix = "p",
    values_to = "auc_value",
    values_drop_na = TRUE
)

pic_box <- ggplot(auc, aes(x = auc_type, y = auc_value, fill = auc_type)) +
    ylim(0.3, 1) + # porovnatelnost mezi glm a maxent +
    geom_violin() +
    facet_grid(~px_size_item)
print(pic_box)

# Plot
pic_box <- auc %>%
    ggplot(aes(x = auc_type, y = auc_value, fill = auc_type)) +
    ylim(0.3, 1) + # porovnatelnost mezi glm a maxent
    geom_boxplot() +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    geom_jitter(color = "red", size = 0.3, alpha = 0.3) +
    ggtitle("AUC") +
    xlab("") +
    facet_grid(~px_size_item) # _wrap

print(pic_box)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #




### Jaccard (F_measuse je totéž jen 2*Jaccard), Jaccard se lépe interpretuje, podobnost je v procentech!
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Jaccard <- tibble_grains %>% pivot_longer(
    cols = c("Jaccard.ndop", "Jaccard.gbif", "Jaccard.ndop"),
    names_to = "Jaccard_type",
    names_prefix = "F",
    values_to = "Jaccard_value",
    values_drop_na = TRUE
)

pic_box <- ggplot(Jaccard, aes(x = Jaccard_type, y = Jaccard_value, fill = Jaccard_type)) +
    ylim(0, 1) + # porovnatelnost mezi glm a maxent +
    geom_violin() +
    facet_grid(~px_size_item)
print(pic_box)

# Plot
pic_box <- Jaccard %>%
    ggplot(aes(x = Jaccard_type, y = Jaccard_value, fill = Jaccard_type)) +
    ylim(0, 1) + # porovnatelnost mezi glm a maxent
    geom_boxplot() +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    geom_jitter(color = "red", size = 0.3, alpha = 0.3) +
    ggtitle("Jaccard") +
    xlab("") +
    facet_grid(~px_size_item) # _wrap

print(pic_box)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


### Sorensen
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
Sorensen <- tibble_grains %>% pivot_longer(
    cols = c("Sorensen.ndop", "Sorensen.gbif", "Sorensen.ndop"),
    names_to = "Sorensen_type",
    names_prefix = "F",
    values_to = "Sorensen_value",
    values_drop_na = TRUE
)

pic_box <- ggplot(Sorensen, aes(x = Sorensen_type, y = Sorensen_value, fill = Sorensen_type)) +
    ylim(0, 1) + # porovnatelnost mezi glm a maxent +
    geom_violin() +
    facet_grid(~px_size_item)
print(pic_box)

# Plot
pic_box <- Sorensen %>%
    ggplot(aes(x = Sorensen_type, y = Sorensen_value, fill = Sorensen_type)) +
    ylim(0, 1) + # porovnatelnost mezi glm a maxent
    geom_boxplot() +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    geom_jitter(color = "red", size = 0.3, alpha = 0.3) +
    ggtitle("Sorensen") +
    xlab("") +
    facet_grid(~px_size_item) # _wrap

print(pic_box)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Plot NDOP

# Plot GBIF


# Plot ALL


dev.off()
# # statistika jednotlivé layery
# p.stats <- tibble_grains %>%  dplyr::select(ends_with(c(".Imp.ndop", ".Imp.gbif")))  %>%
# get_summary_stats(., show = c("mean", "sd", "median", "iqr"))  %>% filter(median >= 0.03) %>%  arrange(variable)
# print(p.stats, n = 100)




pxs_size <- tibble_grains %>% distinct(px_size_item)

pdf(paste0(export_path, "/", prefix, "overall", suffix_pdf, ".pdf"))

# generovat per (filter): px_size, species,

boxplot(x = as.list(as.data.frame(tibble_grains %>% filter(px_size_item == pxs_size$px_size_item[1]) %>% select_if(., is.numeric) %>% dplyr::select(contains("_c")))))

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains("auc.")) %>% dplyr::select(contains(".te")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 2), rep("green", 2), rep("cyan3", 2)))

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains("TSS.")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 2), rep("green", 2), rep("cyan3", 2)))

# boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".boyce")))), las = 2, cex.axis = 0.7)


boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".geo.")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".env.")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".breadth.B1")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".breadth.B2")))), las = 2, cex.axis = 0.7)

# boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".ebreadth.B1")))), las = 2, cex.axis = 0.7)

# boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".ebreadth.B2")))), las = 2, cex.axis = 0.7)
dev.off()
##  souhrn - hlavně celkový počet nálezů ndop/gbif 1489274/3304575  - mám to filtrované 2010-2020???
# tibble_grains  %>% filter(px_size_item == 1000) %>% select_if(., is.numeric) %>% summarise(across(everything(), ~ sum(., is.na(.), 0)))





for (pxs in pxs_size$px_size_item) {
    pdf(paste0(export_path, "/", prefix, "overall_", pxs, "", suffix_pdf, ".pdf"))

    # generovat per (filter): px_size, species,
    print("jedna")
    print(pxs)
    boxplot(x = as.list(as.data.frame(tibble_grains %>% filter(px_size_item == pxs) %>% select_if(., is.numeric) %>% dplyr::select(contains("_c")))))

    boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains("auc.")) %>% dplyr::select(contains(".te")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 2), rep("green", 2), rep("cyan3", 2)))

    boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains("TSS.")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 2), rep("green", 2), rep("cyan3", 2)))

    # boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".boyce")))), las = 2, cex.axis = 0.7)

    boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".geo.")))), las = 2, cex.axis = 0.7)

    boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".env.")))), las = 2, cex.axis = 0.7)

    boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".breadth.B1")))), las = 2, cex.axis = 0.7)

    boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".breadth.B2")))), las = 2, cex.axis = 0.7)

    # boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".ebreadth.B1")))), las = 2, cex.axis = 0.7)

    # boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".ebreadth.B2")))), las = 2, cex.axis = 0.7)

    dev.off()

    ##  souhrn - hlavně celkový počet nálezů ndop/gbif 1489274/3304575  - mám to filtrované 2010-2020???
    # tibble_grains  %>% filter(px_size_item == 1000) %>% select_if(., is.numeric) %>% summarise(across(everything(), ~ sum(., is.na(.), 0)))
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



    typ_filtrace <- "NDOP" # "GBIF_ALL" "GBIF_NDOP" "ALL_NDOP" "NDOP" "GBIF"
    dfAUCfiltr <- dfAUCorig
    # if (typ_filtrace == "GBIF_ALL") {
    #   dfAUCfiltr <- dfAUCorig %>%
    #     filter(auc.all.te >= auc_limit) %>%
    #     filter(auc.gbif.te >= auc_limit)
    # }

    # if (typ_filtrace == "GBIF_NDOP") {
    #   dfAUCfiltr <- dfAUCorig %>%
    #     filter(auc.ndop.te >= auc_limit) %>%
    #     filter(auc.gbif.te >= auc_limit)
    # }

    # if (typ_filtrace == "ALL_NDOP") {
    #   dfAUCfiltr <- dfAUCorig %>%
    #     filter(auc.ndop.te >= auc_limit) %>%
    #     filter(auc.all.te >= auc_limit)
    # }

    # if (typ_filtrace == "NDOP") {
    #   dfAUCfiltr <- dfAUCorig %>%
    #     filter(auc.ndop.te >= auc_limit & gbif_ndop.geo.cor >= 0.75)
    # }

    # if (typ_filtrace == "GBIF") {
    #   dfAUCfiltr <- dfAUCorig %>%
    #     filter(auc.gbif.te >= auc_limit)
    # }


    #
    # neměl bych podle auc filtrovat individuálně podle !!!typu datasetu a velikosti zrna!!! a ne odstraňovat úplně všechny druhy jen když nevyšel jen jeden model?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!§
    # Zohlednit to jaké páry porovnávám, u GBIFxALL mě nezajímá přesnost NDOP a naopak!
    #
    # nutná křížová filtrace per podle AUC per pixel i dataset!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! - lze filtrovat podle group_by?



    # glimpse(dfAUC) # check data

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


    # dfAUC <- dfAUCorig %>%
    #   filter(Training.samples > 30) %>%
    #   filter(Test.samples > 20) %>%
    #   dplyr::select(species1_, species1, species1_short, recnum, px_size_item, Q1, Q2, species2, Training.samples, Training.samples.all) %>%
    #   gather("Training.samples", "Training.samples.all", key = "Question", value = "dAUC")



    # glimpse(dfAUC) # check data

    # # read bird traits table (traits according to Kolecek et al 2010)
    # dftraits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")
    # glimpse(dftraits) # check data

    # # join bird traits to dfAUC
    # df1 <- dfAUC %>%
    #   left_join(dftraits, by = c("species" = "species")) %>%
    #   filter(!is.na(Habitat))




    # df1n <- dfAUC %>%
    #   left_join(dftraits, by = c("species1" = "species")) %>%
    #   filter(is.na(Habitat)) %>%
    #   dplyr::select(-species_old, -Habitat, -Migration, -Distribution, -Protection)
    # df2 <- df1n %>% left_join(dftraits, by = c("species2" = "species"))

    # df <- df1 %>% add_row(df2)
    df <- joined_traits_question
    glimpse(df) # check data

    # anti1 <- dfAUC %>% anti_join(dftraits, by = c("species1" = "species")) %>% distinct(species1)
    # anti <- dftraits %>% anti_join(dfAUC, by = c("species" = "species1")) %>% distinct(species)
    # glimpse(anti1) # check data
    # glimpse(anti) # check data



    # print(anti1, n = 100)
    # print(anti, n = 300)

    # dfAUC1 <- dfAUC %>%
    #   inner_join(dftraits, by = c("species1" = "species")) %>%
    #   drop_na(Habitat)
    # anti <- dfAUC %>% anti_join(dftraits, by = c("species1" = "species"))

    # dfAUC2 <- anti %>%
    #   inner_join(dftraits, by = c("species2" = "species")) %>%
    #   drop_na(Habitat)

    # dfAUC <- bind_rows(dfAUC1, dfAUC2)

    # df <- dfAUC
    # dfAUC %>% distinct(species1)


    # head(df, 100)
    # head(df[90:120, ], 100) # check data



    # labels
    titfreqpx_size_item <- paste0("Dependence of ", title, " on species frequency level (according to grain)")
    titQ1 <- "Q1: difference between global GIF and GIF model with local (NDOP) validation"
    titQ2 <- "Q2: difference between global GIF and adjusted GBIF+NDOP model"
    titQ1Q2 <- "Q1: global (GIF) model with local (NDOP) validation; Q2: GBIF + NDOP model"
    labelfreq <- "species frequency level"
    labely <- title # "AUC difference"

    tithab <- paste0("Dependence of ", title, " on habitat and grain")
    labelhab <- "Habitat type: A - farmland, open sites, F - forests, trees, U - urban, W - wetland"

    titprot <- paste0("Dependence of ", title, " on protection statut and grain")
    labelprot <- "Protection statut: N—non-protected, E—endangered, H — highly endangered, C — critically endangered"

    titdist <- paste0("Dependence of ", title, " on European distribution and grain")
    labeldist <- "European distribution: C — central, N — northern, S — southern, W — widespread"

    titmigr <- paste0("Dependence of ", title, " on migration type and grain")
    labelmigr <- "Migration type: L: long-distance, P: partial, R: resident, S: short-distance"


    pdf(paste0(export_path, "/", prefix, "_", appendix, "_aucLim-", auc_limit, "_filtr-", typ_filtrace, "", suffix_pdf, ".pdf"))

    grid.arrange(top = caption, tableGrob(reduction))
    # grid.table(reduction)


    pic_lm <- ggplot(df, aes(x = Question, y = as.numeric(dAUC), group = Question)) +
        geom_boxplot(aes(fill = Question)) +
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
        facet_wrap(~px_size_item, ncol = 5) + # !!! XXX - pokud chci v jednom řádku tak: ncol = 5
        labs(title = appendix, subtitle = "st", x = paste0(" grain size"), y = appendix, caption = caption) # + scale_y_continuous(trans = 'log2')
    print(pic_lm)



    # , "auc.all.te", "auc.gbif.te", "auc.ndop.te","gbif_all.geo.D", "gbif_all_erase.geo.D", "gbif_ndop.geo.D","gbif_all.env.D", "gbif_all.env.I", "gbif_all.env.I"
    frequency <- c("ndop_c", "gbif_c", "gbif_ndop_rate", "gbif_ndop_perc")
    for (f in frequency) {
        #----------------------------------------------------------------------------------------------------------------
        # check if there is a dependence of AUC diff on species frequency level
        pic_lm <- ggplot(df, aes(x = eval(parse(text = f)), y = as.numeric(dAUC))) +
            geom_point() +
            geom_smooth(method = loess) +
            facet_grid(Question ~ px_size_item) +
            labs(title = titfreqpx_size_item, subtitle = titQ1Q2, x = paste0(labelfreq, " (normalized by \'", f, "\')"), y = labely, caption = caption) # + scale_y_continuous(trans = 'log2')
        print(pic_lm)
        # .... spíš žádná závislost není, což je fajn; u Q2 možná rozdíl v AUC trochu klesá s vyšším počtem záznamů o druhu,
        # .... ale asi spíš neprůkazně (někdy otestovat, ale teď není čas)
        # .... zároveň je na grafech vidět, že
        # .... u Q1 jsou dost velké rozdíly v dosažených AUC - model validovaný na NDOP výrazně horší (nižší AUC)
        # .... tj. kvalita dlobálních modelů je na national level špatná
        # .... tj. global data nepostihují dostatečně national niche ?
        # .... u Q2 jsou rozdíly mezi globálním modelem z GBIF a modelem z GBIF+NDOP menší,
        # .... přičemž u většiny druhů se přidáním NDOP AUC zmenšilo (což nevím, jak interpretovat, nevím, jak jsi to validoval),
        # .... asi zatím tak, že přidání national dat nic modely nezlepší, spíš naopak? Má to vysvětlení?
    }

    #----------------------------------------------------------------------------------------------------------------
    # "Dependence of dAUC on habitat and grain"
    pic_box <- ggplot(df, aes(x = Habitat, y = as.numeric(dAUC), fill = Habitat)) +
        geom_boxplot() +
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
        facet_grid(Question ~ px_size_item) +
        labs(title = tithab, subtitle = titQ1Q2, x = labelhab, y = labely) #+ scale_y_continuous(trans = 'log2')
    print(pic_box)
    # .... rozdíly v habitatech u Q1 budou: nejhorší jsou globální modely oproti national validaci pro farmland a lesní ptáky
    # .... o něco lepší pro birds of urban environment, nejmenší pro wetland birds
    # .... zdá se, že s vyšším grain:
    # .... se national modely jeví spíš horší
    # .... a zároveň se rozdíly mezi ptáky různých habitatů spíš stírají (100 a 200 vypadá podobně, u 1000 změna - všechny habitaty podobně blbé)
    # .... ověřit trend ještě na 2000 a/nebo 5000?

    #----------------------------------------------------------------------------------------------------------------
    # "Dependence of dAUC on protection level and grain"

    pic_box <- ggplot(df, aes(x = factor(Protection, level = c("C", "H", "E", "N")), y = as.numeric(dAUC), fill = Protection)) +
        geom_boxplot() +
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
        facet_grid(Question ~ px_size_item) +
        labs(title = titprot, subtitle = titQ1Q2, x = labelprot, y = labely) #+ scale_y_continuous(trans = 'log2')
    print(pic_box)
    # .... žádný jasný trend, že modely pro chráněné a nechráněné byly lepší či horší?
    # .... obdobně jako u Habitat se zdá, že s vyšším grain spíš horší national a stírají se rozdíly mezi skupinami?

    #----------------------------------------------------------------------------------------------------------------
    # "Dependence of dAUC on European distribution and grain"
    pic_box <- ggplot(df, aes(x = Distribution, y = as.numeric(dAUC), fill = Distribution)) +
        geom_boxplot() +
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
        facet_grid(Question ~ px_size_item) +
        labs(title = titdist, subtitle = titQ1Q2, x = labeldist, y = labely) #+ scale_y_continuous(trans = 'log2')
    print(pic_box)
    # ... tady pokud nějaké rozdíly, tak spíš ve větším grain?



    #----------------------------------------------------------------------------------------------------------------
    # to samé se dá udělat pro typ migrace, ale to asi nemá smysl ukazovat, protože není důvod, proč by na tom mělo něco záležet
    pic_box <- ggplot(df, aes(x = factor(Migration, level = c("R", "P", "S", "L")), y = as.numeric(dAUC), fill = Migration)) +
        geom_boxplot() +
        stat_summary(fun.data = give.n, geom = "text", fun.y = median, colour = "red", size = 2) +
        facet_grid(Question ~ px_size_item) +
        labs(title = titmigr, subtitle = titQ1Q2, x = labelmigr, y = labely) #+ scale_y_continuous(trans = 'log2')
    print(pic_box)

    dev.off()
}




###
# Test významnosti rozdílů mezi daty
###

# # https://www.datanovia.com/en/lessons/anova-in-r/#one-way-independent-measures

# adata <- df %>%
#   filter(pixel_size == 100) %>%
#   dplyr::select(species1_, Habitat, Q1) %>%
#   mutate_at(vars(Habitat), factor)

# print(summary(adata))

# print(adata %>%
#   group_by(Habitat) %>%
#   get_summary_stats(Q1, type = "mean_sd"))

# print(ggboxplot(adata, x = "Habitat", y = "Q1"))



# # print(adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1))

# # # outliers - odebrání1
# # outliers <- adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1) %>%
# #   dplyr::select(species1_) %>%
# #   pull(species1_)

# # adata %<>% filter(!species1_ %in% as.vector(outliers))
# # # outliers - odebrání1



# # print(adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1))

# # # outliers - odebrání2
# # outliers <- adata %>%
# #   group_by(Habitat) %>%
# #   identify_outliers(Q1) %>%
# #   dplyr::select(species1_) %>%
# #   pull(species1_)

# # adata %<>% filter(!species1_ %in% as.vector(outliers))
# # # outliers - odebrání2




# print(adata %>%
#   group_by(Habitat) %>%
#   identify_outliers(Q1))



# print(ggboxplot(adata, x = "Habitat", y = "Q1"))


# # Build the linear model
# model <- lm(Q1 ~ Habitat, data = adata)
# # Create a QQ plot of residuals
# print(ggqqplot(residuals(model)))

# # Compute Shapiro-Wilk test of normality
# print("Compute Shapiro-Wilk test of normality")
# print(shapiro_test(residuals(model)))

# print("Check normality assumption by groups. Computing Shapiro-Wilk test for each group level. If the data is normally distributed, the p-value should be greater than 0.05.")
# print(adata %>%
#   group_by(Habitat) %>%
#   shapiro_test(Q1))


# print(ggqqplot(adata, "Q1", facet.by = "Habitat"))

# print(plot(model, 1))

# print(adata %>% levene_test(Q1 ~ Habitat))

# # nemůžu klasickou ANOVA
# welch_anova <- adata %>% welch_anova_test(Q1 ~ Habitat)
# print(welch_anova)

# # můžu Tukey? - ne (jen pro normální data), jinak musím použít Games-Howell
# pwc <- adata %>% tukey_hsd(Q1 ~ Habitat)
# print(pwc)



# # Pairwise comparisons (Games-Howell)
# pwc2 <- adata %>% games_howell_test(Q1 ~ Habitat)
# # Visualization: box plots with p-values
# pwc2 <- pwc2 %>% add_xy_position(x = "Habitat", step.increase = 1)
# print(ggboxplot(adata, x = "Habitat", y = "Q1") +
#   stat_pvalue_manual(pwc2, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(welch_anova, detailed = TRUE),
#     caption = get_pwc_label(pwc2)
#   ))

# print(pwc2)


# ###
# # If you have doubt about the normality of the data, you can use the Kruskal-Wallis test, which is the non-parametric alternative to one-way ANOVA test.
# # https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# # print(adata %>% kruskal_test(Q1 ~ Habitat))
# # print(adata %>% kruskal_effsize(Q1 ~ Habitat))