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

# výběr datasetu
prefix <- "glm_fmt_ndop_" # _gbif_ _all_

rds_list <-
  list.files(
    path = "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/prelim/occurrences", # vse-v-jesdnom/outputs/rds/
    pattern = paste0("^", prefix, ".*\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
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

tibbleB <- bind_rows(tibble)
tibbleB$px_size %<>% as.integer
tibbleB$species %<>% as.factor
tibbleB %<>% group_by(species, px_size)


# # následující dává smysl jen v případě vybrané JEDNÉ ideální hodnoty adjustu

# # sumarizace podle px_size
# tibbleB.px_size <- tibbleB %>% group_by(px_size)
# tibbleB.px_size %>% summarise(across(where(is.numeric), mean))

# # sumarizace podle druhu
# tibbleB.species <- tibbleB %>% group_by(species)
# tibbleB.species %>% summarise(across(where(is.numeric), mean))

# # sumarizace zgroupovaná zároveň podle druhu a px_size
# tibbleB %>% summarise(across(where(is.numeric), mean))
