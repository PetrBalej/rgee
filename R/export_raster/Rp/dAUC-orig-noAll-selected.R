# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra", "viridis")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# vede na git rgee
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/LV"


prefix <- "glm_LV_" # "OWNPFr" "maxent_thr" "glm_GB_"

rds_list <-
    list.files(
        path = paste0(export_path, "/outputs/rds"), # vse-v-jednom/outputs/rds/
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


# 2km mediany: .cor 0.71; .eps 0.45; rmse 0.19
auc_limit <- 0.70
# auc_limit - nové limity obecně na všechno
tibble_grains %<>%
    filter(auc.ndop.te >= auc_limit) # auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75 & gbif_ndop.geo.rmse <= 0.17 & gbif_ndop.geo.eps >= 0.50

suffix_pdf <- paste0("-all-", prefix, "-10-0.5-") # -cor075- -rmse017- -eps-05-

give.m <- function(x) {
    # pro zobrazení počtu nálezů nad boxploty
    return(c(y = median(x), label = round(median(x), 2))) # label = median(x) length(x)
}

# glimpse(tibble_grains %>% dplyr::select(contains(".ebreadth.")))

pdf(paste0(export_path, "/pdf/", prefix, "-final-charts-", suffix_pdf, ".pdf"), width = 5, height = 4)


### geo.X overlap metriky
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


metric_names <- c(
    "gbif_ndop.geo.D" = "Schoener\'s D",
    "gbif_ndop.geo.cor" = "Spearman\'s rank correlation",
    "gbif_ndop.geo.eps" = "EPS",
    "gbif_ndop.geo.rmse" = "RMSD"
)
tibble_grains_overlap <- tibble_grains %>% select(px_size_item, gbif_ndop.geo.D, gbif_ndop.geo.cor, gbif_ndop.geo.eps, gbif_ndop.geo.rmse)
tibble_grains_overlap %<>%
    rename_at(names(metric_names), ~metric_names)

auc <- tibble_grains_overlap %>% pivot_longer(
    cols = unname(metric_names),
    names_to = "auc_type", # "px size",
    names_prefix = "p",
    values_to = "auc_value", # "overlap by metrics",
    values_drop_na = TRUE
)

# pic_box <- ggplot(auc, aes(x = auc_type, y = auc_value, fill = auc_type)) +
#     ylim(0.0, 1) + # porovnatelnost mezi glm a maxent +
#     geom_violin() +
#     facet_grid(~px_size_item) +
#     ggtitle("geographical niche overlap (GLM: NDOP CZ vs GBIF corrected prediction to CZ)") +
#     xlab("px size") +
#     ylab("overlap by metrics")
# print(pic_box)

# Plot
pic_box <-
    ggplot(auc, aes(x = factor(auc_type, levels = unname(metric_names)), y = auc_value, fill = factor(auc_type, levels = unname(metric_names)))) +
    guides(fill = guide_legend(title = "overlap metrics:")) +
    theme_light() +
    theme(
        text = element_text(size = 8),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 5),
        plot.caption = element_text(hjust = 0.5, size = 5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(size = 0.1)
    ) +
    scale_fill_brewer(palette = "Set1") +
    ylim(0.0, 1) + # porovnatelnost mezi glm a maxent
    # scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    # geom_jitter(color = "red", size = 0.2, alpha = 0.2) +
    ggtitle("Geographical niche overlap") +
    xlab("px size") +
    ylab("overlap by metrics") +
    labs(
        caption = "EPS (Godsoe\'s Expected fraction of Shared Presences); RMSD (=RMSE, root-mean-square deviation/error)",
        subtitle = "Performance of regional (GBIF, centr. Europe) birds bias corrected SDM predictions to Czechia (locally validated by NDOP, AUC > 0.7)"
    ) +
    # facet_wrap(~px_size_item) # _wrap, labeller = as_labeller(auc_type = metric_names)
    facet_grid(. ~ px_size_item, switch = "x") # _wrap, labeller = as_labeller(auc_type = metric_names)




print(pic_box + geom_boxplot(size = 0.1, outlier.size = 0.01) + stat_summary(fun.data = give.m, geom = "text", fun.y = median, colour = "white", size = 1.5))
print(pic_box + geom_violin(size = 0.2) + stat_summary(fun.data = mean_sd, geom = "pointrange", color = "white", size = 0.1))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


dev.off()