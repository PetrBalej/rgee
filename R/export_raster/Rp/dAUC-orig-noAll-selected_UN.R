# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra", "viridis")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

########
## vygeneruje základní grafy niche overlap
########

# vede na git rgee
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)
source(paste0(wd, "/R/export_raster/functions.R"))
source(paste0(wd, "/R/export_raster/multiply-metrics-selection-cycle.R"))


export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/UN3"

prefix <- "glm_UN_" # "OWNPFr" "maxent_thr" "glm_GB_"

if (file.exists(paste0(export_path, "/outputs/tibble_grains-", prefix, ".rds"))) {
    tibble_grains <- readRDS(file = paste0(export_path, "/outputs/tibble_grains-", prefix, ".rds"))
} else {
    tibble_grains <- join_outputs_rds(export_path, prefix)

    ### uložit do rds a csv
    # saveRDS(tibble_grains, file = paste0(export_path, "/outputs/tibble_grains-",prefix,".rds"))
    # write_csv(tibble_grains %>% select_if(~ is.numeric(.) | is.character(.)), paste0(export_path, "/outputs/tibble_grains-",prefix,".csv"))
}


# stop()
# summary(unique(tibble_grains %>% dplyr::select(species)))
# summary(unique(tibble_grains %>% filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.60 & gbif_ndop.geo.rmse <= 0.20 & gbif_ndop.geo.D >= 0.70) %>% dplyr::select(species)))
# summary(unique(tibble_grains %>% filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75) %>% dplyr::select(species)))

# tibble_grains %<>%
#     filter(auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.70 & gbif_ndop.geo.rmse <= 0.20 & gbif_ndop.geo.D >= 0.80)



mm <- mm(wd, export_path, pxs = c(1000, 2000, 5000, 10000))


tibble_grains.reduced1 <- mm # %>% filter(ndop.auc >= 0.85 & md.max > 0.63)


# # nutné prozatimně, kvůli získání boyce indexu z NDOP, který nemám vypočtený (až v novější verzi enmtoolsN)
# tibble_boyce.ndop <- readRDS(file = paste0(export_path, "/outputs/boyce.ndop.rds"))
# tibble_boyce.ndop <- as_tibble(tibble_boyce.ndop) %>% type_convert(
#     col_types = cols(
#         species = "f", px = "i", species = "f"
#     )
# )

tibble_grains.reduced2 <- tibble_grains %>%
    select_if(~ is.numeric(.) | is.character(.))

tibble_grains.reduced3 <- tibble_grains.reduced1 %>%
    left_join(tibble_grains.reduced2, by = c("species" = "species", "px" = "px_size_item"), suffix = c("", "_nj")) %>%
    arrange(desc(md.max))

# tibble_grains.reduced4 <- tibble_grains.reduced3 %>%
#     left_join(tibble_boyce.ndop, by = c("species" = "species", "px" = "px"), suffix = c("", "_ndop")) %>%
#     arrange(desc(md.cor.max))

tibble_grains.reduced <- tibble_grains.reduced4 <- tibble_grains.reduced3 # %>% filter(species == "Certhia familiaris") # boyce.ndop >= 0.990 & cor.max > 0.90


tibble_grains <- tibble_grains.reduced


tibble_grains %<>% add_column(d0 = .$md0.boyce.max - .$boyce.ndop, d1 = .$md1.boyce.max - .$boyce.ndop, di = .$boyce.max - .$boyce.ndop) # !!! di: .$md.boyce.max  nebo .$boyce.max




# 2km mediany: .cor 0.71; .eps 0.45; rmse 0.19
auc_limit <- 0.80
# auc_limit - nové limity obecně na všechno
tibble_grains %<>%
    # filter(auc.ndop.te >= auc_limit) # auc.ndop.te >= 0.70 & gbif_ndop.geo.cor >= 0.75 & gbif_ndop.geo.rmse <= 0.17 & gbif_ndop.geo.eps >= 0.50
    filter(boyce.ndop >= auc_limit)
suffix_pdf <- paste0("-all-", prefix, "-bez500-") # -cor075- -rmse017- -eps-05-

tibble_grains$px <- as.factor(tibble_grains$px)
tg <- summary(tibble_grains$px)


give.m <- function(x) {
    # pro zobrazení počtu nálezů nad boxploty
    return(c(y = median(x), label = round(median(x), 3))) # label = median(x) length(x)
}

# glimpse(tibble_grains %>% dplyr::select(contains(".ebreadth.")))

pdf(paste0(export_path, "/pdf/", prefix, "-final-charts-boyce", auc_limit, "-", suffix_pdf, ".pdf"), width = 4, height = 4)


### geo.X overlap metriky
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# metric_names <- c(
#     # "gbif_ndop.geo.I" = "Warren\'s I",
#     "md.cor.max" = "Spearman\'s rank correlation (md)",
#     "cor.max" = "Spearman\'s rank correlation (max)",
#     # "gbif_ndop.geo.eps" = "EPS",
#     "md.rmse.max" = "RMSE (md)"
# )
# tibble_grains_overlap <- tibble_grains %>% select(px, md.cor.max, cor.max, md.rmse.max)


# metric_names <- c(
#     # "gbif_ndop.geo.I" = "Warren\'s I",
#     "d0" = "d0 (Boyce)",
#     "d1" = "d1 (Boyce)",
#     "di" = "di/max (Boyce)"
# )
# tibble_grains_overlap <- tibble_grains %>% select(px, d0, d1, di)


metric_names <- c(
    # "gbif_ndop.geo.I" = "Warren\'s I",
    "md0.cor.max" = "d0 (cor)",
    "md1.cor.max" = "d1 (cor)",
    "cor.max" = "di/max (cor)" # !!! md.cor.max nebo cor.max
)
tibble_grains_overlap <- tibble_grains %>% select(px, md0.cor.max, md1.cor.max, cor.max) # !!! md.cor.max nebo cor.max






tibble_grains_overlap %<>%
    rename_at(names(metric_names), ~metric_names)



tibble_grains_overlap$px <- as.factor(tibble_grains_overlap$px)

mydata.long <- tibble_grains_overlap %>%
    pivot_longer(-px, names_to = "variables", values_to = "value")

mydata.long$variables <- as.factor(mydata.long$variables)

# mydata.long %>%
#  group_by(variables, px) %>%
#  summarise(
#    n = n(),
#    mean = mean(value),
#    sd = sd(value)
#  ) %>%
#  ungroup()

stat.test <- mydata.long %>%
    group_by(px) %>%
    t_test(value ~ variables, p.adjust.method = "bonferroni")
# Remove unnecessary columns and display the outputs
stat.test %>% select(-.y., -statistic, -df)


# # Create the plot
# myplot <- ggboxplot(
#   mydata.long, x = "variables", y = "value",
#   fill = "variables", palette = "npg", legend = "none",
#   ggtheme = theme_pubr(border = TRUE)
#   ) +
#   facet_wrap(~px)







pic_box <-
    ggboxplot(mydata.long, x = "variables", y = "value", fill = "variables", outlier.shape = 20, outlier.size = 0.1, size = 0.1) +
    guides(fill = guide_legend(title = "")) + # overlap metrics:
    theme_light() +
    theme(
        text = element_text(size = 4),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 4),
        plot.caption = element_text(hjust = 0.5, size = 4),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(size = 0.1)
    ) +
    scale_fill_brewer(palette = "Set1") +
    # ylim(0.0, 1) + # porovnatelnost mezi glm a maxent
    # scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    # geom_jitter(color = "red", size = 0.2, alpha = 0.2) +
    ggtitle("Geographical niche overlap") +
    xlab("px size") +
    ylab("overlap by metrics") +
    labs(
        caption = paste0("species per px size: ", paste(as.character(tg), collapse = ", ")),
        subtitle = paste0("Performance of regional (GBIF, centr. Europe) birds bias corrected SDM (GLM) predictions to Czechia (locally validated by NDOP, Boyce>", auc_limit, ")")
    ) +
    # facet_wrap(~px) # _wrap, labeller = as_labeller(auc_type = metric_names)
    facet_grid(. ~ px, switch = "x") # _wrap, labeller = as_labeller(auc_type = metric_names)


# Add statistical test p-values
stat.test <- stat.test %>% add_xy_position(x = "variables", step.increase = 0.6)


print(pic_box + stat_summary(fun.data = give.m, geom = "text", fun.y = median, colour = "white", size = 1.5) + stat_pvalue_manual(stat.test, label = "p.adj.signif", size = 1.5, bracket.size = 0.1, tip.length = 0.01, vjust = 0.5))













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
#     facet_grid(~px) +
#     ggtitle("geographical niche overlap (GLM: NDOP CZ vs GBIF corrected prediction to CZ)") +
#     xlab("px size") +
#     ylab("overlap by metrics")
# print(pic_box)

# Plot
pic_box <-
    ggplot(auc, aes(x = factor(auc_type, levels = unname(metric_names)), y = auc_value, fill = factor(auc_type, levels = unname(metric_names)))) +
    guides(fill = guide_legend(title = "")) + # overlap metrics:
    theme_light() +
    theme(
        text = element_text(size = 8),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 4),
        plot.caption = element_text(hjust = 0.5, size = 4),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(size = 0.1)
    ) +
    scale_fill_brewer(palette = "Set1") +
    # ylim(0.0, 1) + # porovnatelnost mezi glm a maxent
    # scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    # geom_jitter(color = "red", size = 0.2, alpha = 0.2) +
    ggtitle("Geographical niche overlap") +
    xlab("px size") +
    ylab("overlap by metrics") +
    labs(
        caption = "RMSE (=RMSD, root-mean-square deviation/error). Warren's I and EPS don't sufficiently visualizes discrimination ability.", # EPS (Godsoe\'s Expected fraction of Shared Presences);
        subtitle = paste0("Performance of regional (GBIF, centr. Europe) birds bias corrected SDM (GLM) predictions to Czechia (locally validated by NDOP, Boyce>", auc_limit, ")")
    ) +
    # facet_wrap(~px) # _wrap, labeller = as_labeller(auc_type = metric_names)
    facet_grid(. ~ px, switch = "x") # _wrap, labeller = as_labeller(auc_type = metric_names)




print(pic_box + geom_boxplot(size = 0.1, outlier.size = 0.01) + stat_summary(fun.data = give.m, geom = "text", fun.y = median, colour = "white", size = 1.5))
print(pic_box + geom_violin(size = 0.2) + stat_summary(fun.data = mean_sd, geom = "pointrange", color = "white", size = 0.1))


# kumulativní performance - "logistic function"
pic_box2 <-
    ggplot(auc, aes(auc_value, colour = as.factor(px))) +
    theme_light() +
    theme(
        text = element_text(size = 8),
        # legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 5),
        plot.subtitle = element_text(hjust = 0.5, size = 4),
        plot.caption = element_text(hjust = 0.5, size = 4),
        panel.grid.minor = element_blank(),
        # panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(size = 0.1)
    ) +
    # scale_color_manual(values=wes_palette(n=5, name="Royal2")) +
    # scale_color_brewer(palette = "Spectral") +
    scale_color_manual(values = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")) +
    xlim(0.0, 1) + # porovnatelnost mezi glm a maxent
    # scale_fill_viridis(discrete = TRUE, alpha = 0.9) +
    # geom_jitter(color = "red", size = 0.2, alpha = 0.2) +
    ggtitle("Geographical niche overlap") +
    xlab("overlap by metric") +
    ylab("cumulative probability") +
    labs(
        colour = "px size",
        caption = "RMSE (=RMSD, root-mean-square deviation/error). Warren's I and EPS don't sufficiently visualizes discrimination ability.", # EPS (Godsoe\'s Expected fraction of Shared Presences);
        subtitle = "Performance of regional (GBIF, centr. Europe) birds bias corrected SDM (GLM) predictions to Czechia (locally validated by NDOP, AUC>0.7)"
    ) +
    stat_ecdf(size = 0.3, geom = "line") +
    facet_grid(vars(auc_type)) # _wrap, labeller = as_labeller(auc_type = metric_names)

print(pic_box2)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


dev.off()