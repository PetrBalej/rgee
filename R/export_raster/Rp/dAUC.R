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

export_path <-  "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/pdf_stats"
# pairs <- list(

#   # # auc
#   c("auc.gbif.te", "auc.ndop.te"),
#   c("auc.gbif.te", "auc.all.te"),
#   # # geogr. overlap
#   c("gbif_all.geo.D", "gbif_ndop.geo.D"),
#   c("gbif_all.geo.I", "gbif_ndop.geo.I"),
#   c("gbif_all_erase.geo.D", "gbif_ndop.geo.D"),
#   c("gbif_all_erase.geo.I", "gbif_ndop.geo.I"),
#   # je nějaký rozdíl v globálních po vymazání čr?
#   c("gbif_all_erase.geo.D", "gbif_all.geo.D"),
#   c("gbif_all_erase.geo.I", "gbif_all.geo.I"),
#   # # env. overlap - není s čím do páru porovnávat...
#   c("gbif_all.env.D", "gbif_all.env.D")
# )



pairs <- list(

  # # auc
  c("auc.all.te", "auc.gbif.te", "auc.ndop.te"),
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
  c("gbif.rbreath.B1", "ndop.rbreath.B1", "all.rbreath.B1"),
  c("gbif.rbreath.B2", "ndop.rbreath.B2", "all.rbreath.B2"),
  # # env šířka niky
  # c("gbif.ebreath.B1", "ndop.ebreath.B1", "all.ebreath.B1"),
  # c("gbif.ebreath.B2", "ndop.ebreath.B2", "all.ebreath.B2"),
  # # permut. performance prediktorů - poměr WB : L8
  c("wcl8.gbif", "wcl8.all", "wcl8.ndop")
  # c("wcl8_perc.gbif", "wcl8_perc.all", "wcl8_perc.ndop") # cca 30% prišpívají, ale je třeba to rozdělit na samostatné 3 modely (stejný treshold): WC, L8 a WC+L8
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
rds_list <-
  list.files(
    path = paste0(export_path,"/../tmp3/rds/"), # vse-v-jesdnom/outputs/rds/
    pattern = paste0("^enmsr_glmE[0-9]_.+\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"
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


for (i in seq_along(names(rds_append))) {
  tibble <- rds_append[[i]] %>% # ttemp
    map_depth(1, na.omit) %>%
    map(as_tibble) %>%
    bind_rows(.id = "species") %>%
    # group_by(species) %>% dplyr::select(-r)
    distinct(species, .keep_all = TRUE)



  ##### nejdříve seřadit podle abecedy a až pak tomu dát klíč - pomůže to?
  # tibble_ordered <- tibble %>%  arrange(desc(species))  %>% mutate(id = row_number(), .before= species)
  # tibble_gbif <- tibble_ordered %>% expand(vip1.gbif) %>% rename_all(paste0, "_gbif") %>% mutate(id = row_number(), .after = 0)
  # tibble_all <- tibble_ordered %>% expand(vip1.all) %>% rename_all(paste0, "_all") %>% mutate(id = row_number(), .after = 0)
  # tibble_ndop <- tibble_ordered %>% expand(vip1.ndop) %>% rename_all(paste0, "_ndop") %>% mutate(id = row_number(), .after = 0)

  # tibble_result  <- tibble_ordered %>% inner_join(tibble_gbif, by = "id") %>% inner_join(tibble_all, by = "id") %>% inner_join( tibble_ndop, by = "id")

  # tibble_ordered  %>% dplyr::select(id, species, vip1.all)
  # tibble_result %>% dplyr::select(species,  l8_6.8_10000_EVI.Importance_gbif) # vip1.gbif    l8_6.8_10000_EVI.Importance_gbif   enm_mxt_gbif.vip_gbif   vip3.gbif_gbif



  ## # # # # #  možná bych ani nemusel hodnotu průměrovat už v modelu, ale až tady!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

  tibble_result <- tibble %>%
    left_join(tibble_gbif, by = c("species" = "species.gbif")) %>%
    left_join(tibble_all, by = c("species" = "species.all")) %>%
    left_join(tibble_ndop, by = c("species" = "species.ndop")) %>%
    ungroup() %>%
    dplyr::select(-contains("vip")) %>%
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

tibble_grains %<>% mutate(wc.gbif = rowSums(select(., matches("^wc_.*\\.Imp\\.gbif$"))))
tibble_grains %<>% mutate(wc.all = rowSums(select(., matches("^wc_.*\\.Imp\\.all$"))))
tibble_grains %<>% mutate(wc.ndop = rowSums(select(., matches("^wc_.*\\.Imp\\.ndop$"))))

tibble_grains %<>% mutate(l8.gbif = rowSums(select(., matches("^l8_.*\\.Imp\\.gbif$"))))
tibble_grains %<>% mutate(l8.all = rowSums(select(., matches("^l8_.*\\.Imp\\.all$"))))
tibble_grains %<>% mutate(l8.ndop = rowSums(select(., matches("^l8_.*\\.Imp\\.ndop$"))))


tibble_grains %<>% mutate(wcl8.gbif = (wc.gbif / l8.gbif))
tibble_grains %<>% mutate(wcl8.all = (wc.all / l8.all))
tibble_grains %<>% mutate(wcl8.ndop = (wc.ndop / l8.ndop))

tibble_grains %<>% mutate(wcl8_perc.gbif = ((l8.gbif * 100) / wc.gbif))
tibble_grains %<>% mutate(wcl8_perc.all = ((l8.all * 100) / wc.all))
tibble_grains %<>% mutate(wcl8_perc.ndop = ((l8.ndop * 100) / wc.ndop))



pdf(paste0(export_path,"/auc_variable_permutation_importance.pdf"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
auc <- tibble_grains %>%  pivot_longer(
   cols = c("auc.ndop.te", "auc.gbif.te", "auc.all.te"),
   names_to = "auc_type",
   names_prefix = "p",
   values_to = "auc_value",
   values_drop_na = TRUE
 )

pic_box <- ggplot(auc, aes(x=auc_type, y=auc_value, fill=auc_type)) +  geom_violin()  +
    facet_grid(~ px_size_item)
print(pic_box)

# Plot
pic_box <- auc %>%
  ggplot( aes(x=auc_type, y=auc_value, fill=auc_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.9) +
    geom_jitter(color="red", size=0.3, alpha=0.3) +
    ggtitle("AUC") +
    xlab("") + facet_wrap(~ px_size_item)

print(pic_box)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# Plot NDOP
perm.ndop <- tibble_grains %>%  pivot_longer(
   cols = contains(".Imp.ndop"),
   names_to = "imp_type",
   names_prefix = "p",
   values_to = "imp_value",
   values_drop_na = TRUE
 )
pic_box <- perm.ndop %>%
  ggplot( aes(x=imp_type, y=imp_value, fill=imp_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.9) +

    ggtitle("permutation Importance NDOP") +
    xlab("") + facet_wrap(~ px_size_item)  +
  theme(text = element_text(size=5),axis.text.x=element_text(angle =- 90, vjust = 0.5))

print(pic_box)

# Plot GBIF

perm.gbif <- tibble_grains %>%  pivot_longer(
   cols = contains(".Imp.gbif"),
   names_to = "imp_type",
   names_prefix = "p",
   values_to = "imp_value",
   values_drop_na = TRUE
 ) 
pic_box <- perm.gbif %>%
  ggplot( aes(x=imp_type, y=imp_value, fill=imp_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.9) +

    ggtitle("permutation Importance gbif") +
    xlab("") + facet_wrap(~ px_size_item)  +
  theme(text = element_text(size=5),axis.text.x=element_text(angle =- 90, vjust = 0.5))

print(pic_box)

# Plot ALL

perm.all <- tibble_grains %>%  pivot_longer(
   cols = contains(".Imp.all"),
   names_to = "imp_type",
   names_prefix = "p",
   values_to = "imp_value",
   values_drop_na = TRUE
 )
pic_box <- perm.all %>%
  ggplot( aes(x=imp_type, y=imp_value, fill=imp_type)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.9) +

    ggtitle("permutation ImportanceALL") +
    xlab("") + facet_wrap(~ px_size_item)  +
  theme(text = element_text(size=5),axis.text.x=element_text(angle =- 90, vjust = 0.5))

print(pic_box)

dev.off()

pxs_size  <- tibble_grains %>% distinct(px_size_item )

pdf(paste0(export_path,"/overall.pdf"))

# generovat per (filter): px_size, species,

boxplot(x = as.list(as.data.frame(tibble_grains %>% filter(px_size_item == pxs_size$px_size_item[1]) %>% select_if(., is.numeric) %>% dplyr::select(contains("_c")))))

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains("auc.")) %>% dplyr::select(contains(".te")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 2), rep("green", 2), rep("cyan3", 2)))

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".boyce")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".Imp")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 11), rep("cyan3", 11), rep("green", 11)))

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".geo.")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".env.")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".rbreath.B1")))), las = 2, cex.axis = 0.7)

boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".rbreath.B2")))), las = 2, cex.axis = 0.7)

# boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".ebreath.B1")))), las = 2, cex.axis = 0.7)

# boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% dplyr::select(contains(".ebreath.B2")))), las = 2, cex.axis = 0.7)
dev.off()
##  souhrn - hlavně celkový počet nálezů ndop/gbif 1489274/3304575  - mám to filtrované 2010-2020???
# tibble_grains  %>% filter(px_size_item == 1000) %>% select_if(., is.numeric) %>% summarise(across(everything(), ~ sum(., is.na(.), 0)))





for (pxs in pxs_size$px_size_item) {
  pdf(paste0(export_path,"/overall_", pxs, ".pdf"))

  # generovat per (filter): px_size, species,
  print("jedna")
  print(pxs)
  boxplot(x = as.list(as.data.frame(tibble_grains %>% filter(px_size_item == pxs) %>% select_if(., is.numeric) %>% dplyr::select(contains("_c")))))

  boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains("auc.")) %>% dplyr::select(contains(".te")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 2), rep("green", 2), rep("cyan3", 2)))

  # boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".boyce")))), las = 2, cex.axis = 0.7)

  boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".Imp")))), las = 2, cex.axis = 0.7, col = c(rep("cyan", 11), rep("cyan3", 11), rep("green", 11)))

  boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".geo.")))), las = 2, cex.axis = 0.7)

  boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".env.")))), las = 2, cex.axis = 0.7)

  boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".rbreath.B1")))), las = 2, cex.axis = 0.7)

  boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".rbreath.B2")))), las = 2, cex.axis = 0.7)

  # boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".ebreath.B1")))), las = 2, cex.axis = 0.7)

  # boxplot(x = as.list(as.data.frame(tibble_grains_numeric %>% filter(px_size_item == pxs) %>% dplyr::select(contains(".ebreath.B2")))), las = 2, cex.axis = 0.7)

  dev.off()

  ##  souhrn - hlavně celkový počet nálezů ndop/gbif 1489274/3304575  - mám to filtrované 2010-2020???
  # tibble_grains  %>% filter(px_size_item == 1000) %>% select_if(., is.numeric) %>% summarise(across(everything(), ~ sum(., is.na(.), 0)))
}
# stop()

# # kontrola
# tibble  %>% dplyr::select(species, vip1.gbif)
# tibble_result %>% dplyr::select(species,  l8_3.5_10000_MNDWI.Importance.gbif) # vip1.gbif    l8_6.8_10000_EVI.Importance_gbif   enm_mxt_gbif.vip_gbif   vip3.gbif_gbif







#
#
# pokusy, smazat později
#
#

# tibble_ordered %>% dplyr::select(id, species, vip1.all)  %>% arrange(vip1.gbif)

# tibble_ordered %>%dplyr::select(id, species, reps,  vip3.gbif, ndop_c, gbif_c)

# tibble_ordered  %>%  pivot_longer(vip1.all)  %>%  dplyr::select(id, species, name, value$EVI.Importance_all )


# tibble_ordered %>% mutate(by_continent = map(by_continent, ~.x %>% group_by(country) %>% nest(.key = by_country)))

# tibble_ordered  %>% flatten_df(tibble_ordered )


# tibble_ordered %>%  separate(vip1.all, c("a","b","c","d","e","f","g","h","i","j","k"), convert = TRUE) %>% dplyr::select(species,  a, b, c, d, e, f, g, h, i, j, k)
# tibble_ordered %>%  separate(vip1.all, c("a","b","c","d","e","f","g","h","i","j","k")) %>% dplyr::select(species,  a, b, c, d, e, f, g, h, i, j, k)

# tibble_ordered %>% summarize(vip1.all)

# #%>%  pivot_longer(vip.all)

# dplyr::select(species, vip.all, ndop_c)
#   unnest_wider(vip.all)

# # $wc_10000_bio13.Importance
# gbif_vip <- tibble %>% dplyr::select(vip.all)
# # takhle se dostanu až na úroveň zanořené tabulky - pak řádky napárovat
# gbif_vip$vip.all # totéž jen s tibble %>% expand(vip.all)


# tibble %<>% mutate(id = row_number())
# tibble_vip <- tibble %>% expand(vip.all) %>% mutate(id = row_number())

# tibble %>% inner_join(tibble_vip, by = "id")





# data_zones <- add_row_numbers(tibble)


# tibble_vip <- tibble %>% expand(vip.all) %>% mutate(id = row_number())

# tibble %>% inner_join(tibble_vip, by = "id")


# tibble %>% mutate(xxx = expand(vip.all)$wc_10000_bio13.Importance)

# tibble %>% expand(vip.all, species)%>%  dplyr::select(species, wc_10000_bio13.Importance)



# gbif_vip$vip.all %>%  unnest_wider(l8_3.5_10000_MNDWI.Importance)

# tibble %>% pivot_wider(vip.all)
# tibble %>% pivot_longer(vip.all) %>% dplyr::select(name)



# ########
# tibble %>%  separate(vip.all, c("a","b","c","d","e","f","g","h","i","j","k"), convert = TRUE) %>% dplyr::select(species,  a, b, c, d, e, f, g, h, i, j, k)

# tibble %>%  separate(vip.all, c("a","b","c","d"), convert = TRUE)

# tibble %>% unnest(vip.auc)



# separate(vip.all, c("a","b","c","d","e","f","g","h","i","j","k"), convert = TRUE) %>% dplyr::select(species,  a, b, c, d, e, f, g, h, i, j, k) %>%
#   mutate_if(is.numeric, round, 5)

# dplyr::select(vip.all)

# tibble %>% separate(vip.all , c("a","b","c","d","e","f","g","h","i","j","k"), convert = TRUE) %>% dplyr::select(species,  a, b, c, d, e, f, g, h, i, j, k)


# tibble %>% mutate(vipN = unlist(vip.all))%>% dplyr::select(vipN)


# tibble %>% separate(vip.all, c("a","b","c","d","e","f","g","h","i","j","k"), convert = TRUE) %>%  separate(vip.all, c("a","b","c","d","e","f","g","h","i","j","k"), convert = TRUE)  %>% dplyr::select(species,  a, b, c, d, e, f, g, h, i, j, k)
# #  %>%  separate(vip.all, c("a","b","c","d","e","f","g","h","i","j","k"))

# dplyr::select(-r) %>%  mutate(auc_csv = map_chr(auc, toString)) %>% dplyr::select(auc_csv)
#   #filter(species == "Cinclus cinclus") %>%
#  # dplyr::select(auc)# %>%  unnest_wider(auc )  #%>% extract2(2)   # %>% hoist(auc ) # %>%  unnest_wider(auc )  %>% add_column()# %>%  unnest_wider( auc ) # %>% unnest_wider( auc ) #  %>% unnest(auc) %>% unnest(auc)

# tibble%>%dplyr::select(species, a, b, c, d, e, f, g, h, i, j, k)

# tibble  %>% separate(vip.all, c("a","b","c","d","e","f","g","h","i","j","k"))

# separate(data = tibble, col = vip.all, into = c("a","b","c","d","e","f","g","h","i","j","k"), sep = "_")
# print(tibble  %>% dplyr::select(vip.all ), width = Inf)
# tibble <- dfAUCorig_10000[[1]] %>%
#   map_depth(2, na.omit) %>%
#   map(as_tibble) %>%
#   unnest(auc) %>%
#   group_by(species) %>%
#   mutate(col = paste0('col', row_number())) %>%
#   pivot_wider(names_from = col, values_from = auc) %>%
#   ungroup -> df1


# df1 %>% dplyr::select(species, col1, col2, col3, col4, col5)



# as.data.frame(unlist(tibble))


# stop()
#   # mutate_if(is.list, simplify_all) %>%
#   # unnest()


#   #  mutate(aucTEST = dfAUCorig_10000[[1]][["Cinclus cinclus"]]$auc$ndop[[1]] ) %>% dplyr::select(aucTEST)
# # flatten_dfr(.id= '', auc)
#   #  mutate(nauc = map(auc, as_tibble), .name_repair = "minimal") %>%
# # mutate(aucTEST = unlist(auc))


# #unnest_wider(auc, simplify = TRUE, names_sep = "_") %>%
# #unnest_wider(auc) %>% unnest_wider( values )




#   #mutate(auct = 123) %>%
#    #unnest_wider(dfAUCorig_10000[[1]] %>% dplyr::select(auc), simplify = TRUE, names_sep = "_")
# # hoist(auc,first_AUC = list("gbif", 1L))



# # %>% mutate(
# #   a = map_dbl(x, "a"),
# #   b = map_dbl(x, "b", .null = NA_real_)
# # )

# # dfAUCorig_10000_ <- dfAUCorig_10000
# # names(dfAUCorig_10000[[1]])

# # for(sp in names(dfAUCorig_10000_[[1]])){
# #  # nutné odstranit rastery
# # dfAUCorig_10000_[[1]][[sp]]$r <- NULL
# # }

# # df <- as.data.frame(do.call(rbind, lapply(dfAUCorig_10000_, as.data.frame)))

# # dfAUCorig_10000_u <- unlist(dfAUCorig_10000_)
# # stop()



# rds <- dfAUCorig_10000

# for (pxs in names(rds)) {
#   # nutné odstranit ra
#   for (sp in names(rds[[pxs]])) {
#     print(paste0(pxs, " - ", sp, " **********************************************************************************************"))

# # print(str(rds[[pxs]][[sp]]$vip[[1]]))

# # cnames <- rds[[pxs]][[sp]]$vip[[1]]$Variable
# # print(purrr::transpose(rds[[pxs]][[sp]]$vip[[1]][,2], cnames))

# # vip1 <- as.data.frame(t(as.matrix(unlist(purrr::transpose(rds[[pxs]][[sp]]$vip[[1]][,2], cnames))))) # .names = rds[[pxs]][[sp]]$vip[[1]]$Variable

# # write_csv(vip1 , paste0(export_path, "t4a.csv"), append = TRUE)

# # vip <- as.data.frame(t(as.matrix(unlist(rds[[pxs]][[sp]]$vip))))

# # # as.data.frame(transpose(rds[[pxs]][[sp]]$vip[[1]][, 2]))
# # write_csv(vip  , paste0(export_path, "t4b.csv"), append = TRUE)
# # stop()

# # AUC 15

# auc <- as.data.frame(t(as.matrix(unlist(rds[[pxs]][[sp]]$auc))))
# print(names(auc))
# print(as_tibble(auc))

# write_csv(auc  , paste0(export_path, "auc.csv"), append = TRUE) #bind_cols(csvgee_1[1, ], csvgee_2[1, ])

# stop()
#     # dfAUCorig_10000_[[1]][[sp]]$r <- NULL

#     # str(dfAUCorig_10000[[1]][["Cinclus cinclus"]]$auc, max.level = 2)
#   }
# }



# dfAUCorig_10000

# for (res in dfAUCorig_10000 ) {
# print(class(res))

# }


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


  auc_limit <- 0.5
  typ_filtrace <- "GBIF_NDOP" # "GBIF_ALL" "GBIF_NDOP" "ALL_NDOP" "NDOP" "GBIF"

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


  #
  # neměl bych podle auc filtrovat individuálně podle !!!typu datasetu a velikosti zrna!!! a ne odstraňovat úplně všechny druhy jen když nevyšel jen jeden model?!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!§
  # Zohlednit to jaké páry porovnávám, u GBIFxALL mě nezajímá přesnost NDOP a naopak!
  #
  # nutná křížová filtrace per podle AUC per pixel i dataset!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! - lze filtrovat podle group_by?




  c_orig <- as_tibble(dfAUCorig %>% group_by(px_size_item) %>% count(px_size_item))
  c_filter <- as_tibble(dfAUCfiltr %>% group_by(px_size_item) %>% count(px_size_item))
  reduction <- as_tibble(c_orig %>% mutate(n_filter = c_filter$n) %>% mutate(perc = (n_filter * 100 / n)))


  ### stačí předat jen vektor - nemusím pokaždé specifikovat počet!
  # df %>% gather("key", "value", x, y, z) is equivalent to df %>% pivot_longer(c(x, y, z), names_to = "key", values_to = "value")

  dfAUC <- dfAUCfiltr %>% gather(p[1], p[2], p[3], key = "Question", value = "dAUC")

  cnt.all <- count(dfAUCorig)
  cnt.filtr <- count(dfAUCfiltr)
  cnt.perc <- (cnt.filtr * 100) / cnt.all
  caption <- paste0("AUC > ", auc_limit)


  # dfAUC <- dfAUCorig %>%
  #   filter(Training.samples > 30) %>%
  #   filter(Test.samples > 20) %>%
  #   dplyr::select(species1_, species1, species1_short, recnum, px_size_item, Q1, Q2, species2, Training.samples, Training.samples.all) %>%
  #   gather("Training.samples", "Training.samples.all", key = "Question", value = "dAUC")



  glimpse(dfAUC) # check data

  # read bird traits table (traits according to Kolecek et al 2010)
  dftraits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")
  glimpse(dftraits) # check data

  # join bird traits to dfAUC
  df1 <- dfAUC %>%
    left_join(dftraits, by = c("species" = "species")) %>%
    filter(!is.na(Habitat))
  # df1n <- dfAUC %>%
  #   left_join(dftraits, by = c("species1" = "species")) %>%
  #   filter(is.na(Habitat)) %>%
  #   dplyr::select(-species_old, -Habitat, -Migration, -Distribution, -Protection)
  # df2 <- df1n %>% left_join(dftraits, by = c("species2" = "species"))

  # df <- df1 %>% add_row(df2)
  df <- df1
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


  pdf(paste0(export_path,"/", appendix, "_aucLim-", auc_limit, "_filtr-", typ_filtrace, ".pdf"))

  grid.arrange(top = caption, tableGrob(reduction))
  # grid.table(reduction)



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
    facet_grid(Question ~ px_size_item) +
    labs(title = titprot, subtitle = titQ1Q2, x = labelprot, y = labely) #+ scale_y_continuous(trans = 'log2')
  print(pic_box)
  # .... žádný jasný trend, že modely pro chráněné a nechráněné byly lepší či horší?
  # .... obdobně jako u Habitat se zdá, že s vyšším grain spíš horší national a stírají se rozdíly mezi skupinami?

  #----------------------------------------------------------------------------------------------------------------
  # "Dependence of dAUC on European distribution and grain"
  pic_box <- ggplot(df, aes(x = Distribution, y = as.numeric(dAUC), fill = Distribution)) +
    geom_boxplot() +
    facet_grid(Question ~ px_size_item) +
    labs(title = titdist, subtitle = titQ1Q2, x = labeldist, y = labely) #+ scale_y_continuous(trans = 'log2')
  print(pic_box)
  # ... tady pokud nějaké rozdíly, tak spíš ve větším grain?



  #----------------------------------------------------------------------------------------------------------------
  # to samé se dá udělat pro typ migrace, ale to asi nemá smysl ukazovat, protože není důvod, proč by na tom mělo něco záležet
  pic_box <- ggplot(df, aes(x = factor(Migration, level = c("R", "P", "S", "L")), y = as.numeric(dAUC), fill = Migration)) +
    geom_boxplot() +
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







# > names(tibble_result )
#   [1] "reps"
#   [2] "px_size_item"
#   [3] "species"
#   [4] "ndop_c"
#   [5] "gbif_c"
#   [6] "auc.gbif.tr"
#   [7] "auc.gbif.te"
#   [8] "auc.gbif.tr.env"
#   [9] "auc.gbif.te.env"
#  [10] "auc.gbif.boyce"
#  [11] "auc.ndop.tr"
#  [12] "auc.ndop.te"
#  [13] "auc.ndop.tr.env"
#  [14] "auc.ndop.te.env"
#  [15] "auc.ndop.boyce"
#  [16] "auc.all.tr"
#  [17] "auc.all.te"
#  [18] "auc.all.tr.env"
#  [19] "auc.all.te.env"
#  [20] "auc.all.boyce"
#  [21] "vip1.gbif"
#  [22] "vip1.ndop"
#  [23] "vip1.all"
#  [24] "vip2.gbif"
#  [25] "vip2.ndop"
#  [26] "vip2.all"
#  [27] "vip3.gbif"
#  [28] "vip3.ndop"
#  [29] "vip3.all"
#  [30] "gbif_ndop.geo.D"
#  [31] "gbif_ndop.geo.I"
#  [32] "gbif_ndop.geo.cor"
#  [33] "gbif_all.geo.D"
#  [34] "gbif_all.geo.I"
#  [35] "gbif_all.geo.cor"
#  [36] "gbif_all_erase.geo.D"
#  [37] "gbif_all_erase.geo.I"
#  [38] "gbif_all_erase.geo.cor"
#  [39] "gbif_all.env.D"
#  [40] "gbif_all.env.I"
#  [41] "gbif_all.env.cor"
#  [42] "sp"
#  [43] "l8_3.5_5000_MNDWI.Importance.gbif"
#  [44] "l8_6.8_5000_EVI.Importance.gbif"
#  [45] "l8_6.8_5000_MNDWI.Importance.gbif"
#  [46] "l8_9.11_5000_B5.Importance.gbif"
#  [47] "l8_9.11_5000_EVI.Importance.gbif"
#  [48] "l8_9.11_5000_MNDWI.Importance.gbif"
#  [49] "wc_5000_bio02.Importance.gbif"
#  [50] "wc_5000_bio03.Importance.gbif"
#  [51] "wc_5000_bio09.Importance.gbif"
#  [52] "wc_5000_bio13.Importance.gbif"
#  [53] "wc_5000_bio15.Importance.gbif"
#  [54] "l8_3.5_10000_MNDWI.Importance.gbif"
#  [55] "l8_6.8_10000_EVI.Importance.gbif"
#  [56] "l8_6.8_10000_MNDWI.Importance.gbif"
#  [57] "l8_9.11_10000_B5.Importance.gbif"
#  [58] "l8_9.11_10000_EVI.Importance.gbif"
#  [59] "l8_9.11_10000_MNDWI.Importance.gbif"
#  [60] "wc_10000_bio02.Importance.gbif"
#  [61] "wc_10000_bio03.Importance.gbif"
#  [62] "wc_10000_bio09.Importance.gbif"
#  [63] "wc_10000_bio13.Importance.gbif"
#  [64] "wc_10000_bio15.Importance.gbif"
#  [65] "l8_3.5_1000_MNDWI.Importance.gbif"
#  [66] "l8_6.8_1000_EVI.Importance.gbif"
#  [67] "l8_6.8_1000_MNDWI.Importance.gbif"
#  [68] "l8_9.11_1000_B5.Importance.gbif"
#  [69] "l8_9.11_1000_EVI.Importance.gbif"
#  [70] "l8_9.11_1000_MNDWI.Importance.gbif"
#  [71] "wc_1000_bio02.Importance.gbif"
#  [72] "wc_1000_bio03.Importance.gbif"
#  [73] "wc_1000_bio09.Importance.gbif"
#  [74] "wc_1000_bio13.Importance.gbif"
#  [75] "wc_1000_bio15.Importance.gbif"
#  [76] "l8_3.5_5000_MNDWI.Importance.all"
#  [77] "l8_6.8_5000_EVI.Importance.all"
#  [78] "l8_6.8_5000_MNDWI.Importance.all"
#  [79] "l8_9.11_5000_B5.Importance.all"
#  [80] "l8_9.11_5000_EVI.Importance.all"
#  [81] "l8_9.11_5000_MNDWI.Importance.all"
#  [82] "wc_5000_bio02.Importance.all"
#  [83] "wc_5000_bio03.Importance.all"
#  [84] "wc_5000_bio09.Importance.all"
#  [85] "wc_5000_bio13.Importance.all"
#  [86] "wc_5000_bio15.Importance.all"
#  [87] "l8_3.5_10000_MNDWI.Importance.all"
#  [88] "l8_6.8_10000_EVI.Importance.all"
#  [89] "l8_6.8_10000_MNDWI.Importance.all"
#  [90] "l8_9.11_10000_B5.Importance.all"
#  [91] "l8_9.11_10000_EVI.Importance.all"
#  [92] "l8_9.11_10000_MNDWI.Importance.all"
#  [93] "wc_10000_bio02.Importance.all"
#  [94] "wc_10000_bio03.Importance.all"
#  [95] "wc_10000_bio09.Importance.all"
#  [96] "wc_10000_bio13.Importance.all"
#  [97] "wc_10000_bio15.Importance.all"
#  [98] "l8_3.5_1000_MNDWI.Importance.all"
#  [99] "l8_6.8_1000_EVI.Importance.all"
# [100] "l8_6.8_1000_MNDWI.Importance.all"
# [101] "l8_9.11_1000_B5.Importance.all"
# [102] "l8_9.11_1000_EVI.Importance.all"
# [103] "l8_9.11_1000_MNDWI.Importance.all"
# [104] "wc_1000_bio02.Importance.all"
# [105] "wc_1000_bio03.Importance.all"
# [106] "wc_1000_bio09.Importance.all"
# [107] "wc_1000_bio13.Importance.all"
# [108] "wc_1000_bio15.Importance.all"
# [109] "l8_3.5_5000_MNDWI.Importance.ndop"
# [110] "l8_6.8_5000_EVI.Importance.ndop"
# [111] "l8_6.8_5000_MNDWI.Importance.ndop"
# [112] "l8_9.11_5000_B5.Importance.ndop"
# [113] "l8_9.11_5000_EVI.Importance.ndop"
# [114] "l8_9.11_5000_MNDWI.Importance.ndop"
# [115] "wc_5000_bio02.Importance.ndop"
# [116] "wc_5000_bio03.Importance.ndop"
# [117] "wc_5000_bio09.Importance.ndop"
# [118] "wc_5000_bio13.Importance.ndop"
# [119] "wc_5000_bio15.Importance.ndop"
# [120] "l8_3.5_10000_MNDWI.Importance.ndop"
# [121] "l8_6.8_10000_EVI.Importance.ndop"
# [122] "l8_6.8_10000_MNDWI.Importance.ndop"
# [123] "l8_9.11_10000_B5.Importance.ndop"
# [124] "l8_9.11_10000_EVI.Importance.ndop"
# [125] "l8_9.11_10000_MNDWI.Importance.ndop"
# [126] "wc_10000_bio02.Importance.ndop"
# [127] "wc_10000_bio03.Importance.ndop"
# [128] "wc_10000_bio09.Importance.ndop"
# [129] "wc_10000_bio13.Importance.ndop"
# [130] "wc_10000_bio15.Importance.ndop"
# [131] "l8_3.5_1000_MNDWI.Importance.ndop"
# [132] "l8_6.8_1000_EVI.Importance.ndop"
# [133] "l8_6.8_1000_MNDWI.Importance.ndop"
# [134] "l8_9.11_1000_B5.Importance.ndop"
# [135] "l8_9.11_1000_EVI.Importance.ndop"
# [136] "l8_9.11_1000_MNDWI.Importance.ndop"
# [137] "wc_1000_bio02.Importance.ndop"
# [138] "wc_1000_bio03.Importance.ndop"
# [139] "wc_1000_bio09.Importance.ndop"
# [140] "wc_1000_bio13.Importance.ndop"
# [141] "wc_1000_bio15.Importance.ndop"