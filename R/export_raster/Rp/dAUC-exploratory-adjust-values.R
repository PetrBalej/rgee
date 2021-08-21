# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "magrittr", "dplyr", "ggplot2", "rstatix", "ggpubr", "gridExtra", "viridis")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# vede na git rgee
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/pdf_statsIGA"

datasets <- c("ndop", "all", "gbif")

tibbleT <- list()
for (d in datasets) {
  rds_list <-
    list.files(
      path = "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/BF0/inputs/occurrences", # vse-v-jesdnom/outputs/rds/ , /mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/prelim/occurrences
      # původní: "^glm_fmt_", d, "_.*\\.rds$"
      # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
      pattern = paste0("^glm_fmt_", d, "_.*\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
      ignore.case = TRUE,
      full.names = TRUE
    )


  rds_append <- list()

  rds <- readRDS(rds_list[[1]])
  rds_names <- names(rds)
  rds_append[[rds_names]] <- rds[[rds_names]]


  rds_list <- rds_list[-1]
  for (i in seq_along(rds_list)) {
    rds <- readRDS(rds_list[[i]])
    rds_names <- names(rds)

    rds_append[[rds_names]] <- append(rds_append[[rds_names]], rds[[rds_names]])
    # rds_append <- append(rds_append, readRDS(rds_list[[i]]))
  }

  tibble <- list()
  for (i in names(rds_append)) {
    print("-----------------------------------------------------------------------------------------------------")
    print(i)
    tibble[[i]] <- rds_append[[i]] %>% # ttemp
      map_depth(1, na.omit) %>%
      map(as_tibble) %>%
      bind_rows(.id = "species") %>%
      mutate(px_size = i)
    # # %>%
    # # #group_by(species) %>%
    # distinct(species, .keep_all = TRUE)

    #      print(tibble)
    # tibble %<>% tibble %>% bind_rows(.id = "species")

    # tibble %<>% tibble#  %>% group_by(species, px_size) # sp = paste(species, px_size_item)
  }

  tibbleT[[d]] <- bind_rows(tibble) %>% mutate(dataset = d)
}

tibbleB <- bind_rows(tibbleT)

tibbleB$px_size %<>% as.integer
tibbleB$species %<>% as.factor
tibbleB$dataset %<>% as.factor
tibbleB %<>% group_by(species, px_size, dataset)

# # následující dává smysl jen v případě vybrané JEDNÉ ideální hodnoty adjustu

# # sumarizace podle px_size
# tibbleB.px_size <- tibbleB %>% group_by(px_size)
# tibbleB.px_size %>% summarise(across(where(is.numeric), mean))

# # sumarizace podle druhu
# tibbleB.species <- tibbleB %>% group_by(species)
# tibbleB.species %>% summarise(across(where(is.numeric), mean))

# # sumarizace zgroupovaná zároveň podle druhu a px_size
# tibbleB %>% filter(species == "Podiceps grisegena") %>% summarise(across(where(is.numeric), max))
# odstraním nejvyšší V2 (možný extrém) - nedělat - může být fakt nejvyšší


t4 <- tibbleB %>%
  filter(species == "Podiceps grisegena" & dataset == "gbif" & px_size == 5000) %>%
  top_n(5, V2) %>%
  top_n(-4, V2) %>%
  arrange(V2)

# odstraním nejodlehlejší nms
t4.m <- mean(t4$nms)
t4 %<>% arrange(nms)

if (abs(t4[1, ]$nms - t4.m) < abs(t4[4, ]$nms - t4.m)) {
  t3 <- t4[-4, ]
} else {
  t3 <- t4[-1, ]
}


t5 <- tibbleB %>%
  filter(species == "Podiceps grisegena" & dataset == "gbif" & px_size == 2000) %>%
  top_n(5, V2) %>%
  arrange(nms)
t3 <- t5[-c(1, 5), ]
median(t3$nms)

#

tibbleB %>%
  filter(species == "Podiceps grisegena") %>%
  summarise(max(V2))


tibbleB %>%
  top_n(5, V2) %>%
  summarise(median(V2))

pg.g <- tibbleB %>% filter(species == "Podiceps grisegena" & dataset == "gbif" & px_size == 10000)
pg.a <- tibbleB %>% filter(species == "Podiceps grisegena" & dataset == "all" & px_size == 10000)


tibbleB %>%
  filter(species == "Periparus ater") %>%
  top_n(5, V2) %>%
  summarise_at(vars(V2), median)

tibbleB %>%
  filter(species == "Periparus ater") %>%
  slice_max(V2, n = 5) %>%
  summarise_at(vars(nms), median)

tibbleB %>%
  filter(species == "Periparus ater") %>%
  slice_max(V2, n = 5) %>%
  summarise_at(vars(nms), median)
print(tibbleB %>% filter(species == "Periparus ater") %>% slice_max(V2, n = 5) %>% summarise_at(vars(nms), median), n = 100)


tibbleB %>%
  filter(species == "Periparus ater") %>%
  slice_max(V2, n = 5) %>%
  filter(nms > quantile(nms, 0.10), nms < quantile(nms, 0.90)) %>%
  summarise_at(vars(nms), median)

t5 <- tibbleB %>%
  slice_max(V2, n = 5) %>%
  summarise_at(vars(nms), median) # %>% filter(species == "Periparus ater")

t5f <- tibbleB %>%
  slice_max(V2, n = 5) %>%
  mutate(maxNms = max(nms), minNms = min(nms)) %>%
  mutate(nmsRange = maxNms - minNms) %>%
  summarise_at(vars(nms, minNms, maxNms, nmsRange), median)
t5f3mmr <- tibbleB %>%
  filter(nms < 3) %>%
  slice_max(V2, n = 5) %>%
  mutate(maxNms = max(nms), minNms = min(nms)) %>%
  mutate(nmsRange = maxNms - minNms) %>%
  summarise_at(vars(nms, minNms, maxNms, nmsRange), median)
t3f3mmr <- tibbleB %>%
  filter(nms < 3) %>%
  slice_max(V2, n = 3) %>%
  mutate(maxNms = max(nms), minNms = min(nms)) %>%
  mutate(nmsRange = maxNms - minNms) %>%
  summarise_at(vars(nms, minNms, maxNms, nmsRange), median)

# write_csv(t5f, "adjust-top5-median.csv")
# write_csv(t5f3mmr, "adjust-ltNms3-top5-median.csv")
# write_csv(t3f3mmr, "adjust-ltNms3-top3-median.csv")


# tibbleB.glm <- tibbleB
# tibbleB.maxent <- tibbleB


# t3f3mmr.maxent <- tibbleB.maxent %>%
#   filter(nms < 3) %>%
#   slice_max(V2, n = 3) %>%
#   mutate(maxNms = max(nms), minNms = min(nms)) %>%
#   mutate(nmsRange = maxNms - minNms) %>%
#   summarise_at(vars(nms, minNms, maxNms, nmsRange), median)

#   # write_csv(t3f3mmr.maxent, "adjust-ltNms3-top3-median.maxent.csv")

# t3f3mmr.glm <- tibbleB.glm %>%
#   filter(nms < 3) %>%
#   slice_max(V2, n = 3) %>%
#   mutate(maxNms = max(nms), minNms = min(nms)) %>%
#   mutate(nmsRange = maxNms - minNms) %>%
#   summarise_at(vars(nms, minNms, maxNms, nmsRange), median)

#   # write_csv(t3f3mmr.glm, "adjust-ltNms3-top3-median.glm.csv")


# rozsahy raster.breadth

RBRange <- tibbleB %>%
  mutate(maxRB = max(V2), minRB = min(V2)) %>%
  mutate(RBRange = maxRB - minRB) %>%
  summarise_at(vars(RBRange, maxRB), mean)

# library
library(ggplot2)
# # # - musím všude použít filtr na NDOP AUC a napárovat na GBIF, jinak nemůžu tvrdit, že ty hodnoty jsou správně...

dataRB <- data.frame(px_size = RBRange$px_size, dataset = RBRange$dataset, maxRB = RBRange$maxRB, RBRange = RBRange$RBRange)


# # #
# # # grouped boxplot - porovnání max (ideál) šířky nik per pixel a dataset
# # #
ggplot(dataRB, aes(x = factor(px_size), y = maxRB, fill = dataset)) +
  geom_boxplot() +
  labs(
    title = "porovnání maximální (ideální, peak) šířky nik per pixel a dataset",
    subtitle = "GBIF data vykazují větší šířku niky napříč px size"
  )

# # #
# # # grouped boxplot - porovnání rozpětí šířky nik během adjustace per pixel a dataset = jak moc se r.breadth zlepší (zvětší) během adjustace,
# # # aneb jak moc velký vliv má můj bias fitting
# # #

ggplot(data, aes(x = factor(px_size), y = RBRange, fill = dataset)) +
  geom_boxplot() +
  labs(
    title = "porovnání změny rozpětí šířky nik během bias fittingu per pixel a dataset",
    subtitle = "jak moc se r.breadth zlepší (zvětší) během fittingu, jak velký je vliv bias rasteru na nálezy druhů",
    caption = "NDOP data nejsou fittingem příliš ovlivněná (šířka niky se moc nemění, totéž GBIF 10km)"
  )



topAdjust <- tibbleB %>% slice_max(V2, n = 1)
dataAdj <- data.frame(px_size = topAdjust$px_size, dataset = topAdjust$dataset, adjust = topAdjust$nms)
# # #
# # # grouped boxplot - porovnání rozpětí šířky nik během adjustace per pixel a dataset = jak moc se r.breadth zlepší (zvětší) během adjustace,
# # # aneb jak moc velký vliv má můj bias fitting
# # #

ggplot(dataAdj, aes(x = factor(px_size), y = adjust, fill = dataset)) +
  geom_boxplot() +
  labs(
    title = "adjust (kernel size) s největší r.breadth (ideál, peak)",
    subtitle = "(vyšší adjust hodnota ~ větší šířka kernelu)",
    caption = "srovnatelné asi jen v rámci jedné px_size, napříč asi ne, každý rozměr má předem vypočítaný ideální adjust"
  )