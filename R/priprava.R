# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)




# wd <- "D:/PERSONAL_DATA/pb/rgee"
wd <- "C:/Users/petr/Downloads/igaD/rgeeDP/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

path.igaD <- "C:/Users/petr/Downloads/igaD/igaD/"

# #
# # vytažení dat z průběžných výsledků jednotlivých druhů a uložení rds
# #
#
"%notin%" <- Negate("%in%")
# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor03-vidstep15.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor05-vidstep2.grd"))


# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vify/kfme16-vify/kfme16-vifcor05-vidstep2-cv.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vify/kfme16-vify/kfme16-vifcor05-vidstep2-mean.grd"))

names(env.sentinel_bio) <- str_replace_all(names(env.sentinel_bio), c("kfme16." = "", "_30" = "", "\\.nd" = ""))
env.sentinel_bio.names <- names(env.sentinel_bio)



# připojit traits!!!!
traits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")

# nahrazení názvů traits druhů novějšími názvy z NDOPu
traits <- synonyms_unite(traits)




# načtení RDS jednotlivých druhů, spojení do jednoho listu
rds_list <-
  list.files(
    path = paste0(path.igaD, "../v3/all-mm2m3-unfiltered-rds"), # ../t--1  ws2-glm-meanKomplet-cv4kola  C:\Users\petr\Downloads\igaD\igaD\all-unfiltered-rds-v4
    # původní: "^glm_fmt_", d, "_.*\\.rds$"
    # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
    pattern = paste0("[A-z]+ [a-z]+\\.rds$"), #    ws2-glm-cv--[0-9]{1,2}-[A-z]+ [a-z]+         [A-z]+ [a-z]+\\.rds$     "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
    ignore.case = TRUE,
    full.names = TRUE
  )

# rds_list <- str_replace(rds_list, "-cv-", "-mean-")
# rds_list <- str_replace(rds_list, "-CV-", "-mean-")

species.names <- regmatches(rds_list, regexpr("([A-z]+ [A-z]+)", rds_list))
rds_append <- list()
# rds_append <- sapply(species.names, function(x) NA)
for (i in seq_along(rds_list)) {
  species.name <- regmatches(rds_list[[i]], regexpr("([A-z]+ [A-z]+)", rds_list[[i]]))
  print(species.name)
  rds_append[[species.name]] <- readRDS(rds_list[[i]])
}

# saveRDS(rds_append, file = paste0(path.igaD, "../t--1/all-merged.rds"))
# saveRDS(rds_append, file = paste0(path.igaD, "../rds-all/all-merged.rds"))

# name.export <- "all-merged-cvXmean-mean-72"
name.export <- "all-merged-maxent-104-v3"
# saveRDS(rds_append, file = paste0(path.igaD, name.export, ".rds"))







# soubor s výsledky modelů
# rds_append <- readRDS(paste0(path.igaD, "../rds-all/all-merged.rds"))
rds_append <- readRDS(paste0(path.igaD, name.export, ".rds"))
species.names <- names(rds_append)

# k získání prevalence z NDOP dat - porovnat s p/a LSD prevalencí - shoduje se?
czechia.kfme16.total.count <- 9845 # všechny kvadráty které mají průnik, pokud bych bral čistě vnitřní nebo třeba 50% uvnitř, bude jich ještě méně (ve 131 chybí Sněžka?!?)

scu <- c() # unikátní kombinace, sběrač

metrics <- c("auc", "sorensen") # jen změna indexu 1/2
select.by <- metrics[2] # jen změna indexu 1/2

select.by.alt <- ifelse(select.by == "auc", metrics[2], metrics[1])

cn <- c(
  "species",
  paste0(select.by, ".max.total"), # nejlepší auc/sorensen celkově
  paste0(select.by, ".max"), # nejlepší auc/sorensen v rámci "pt.min"
  "tss.max", paste0(select.by.alt, ".max"), # nejlepší tss auc/sorensen v rámci "pt.min"
  "pt.min", # treshold minimálního počtu prediktorů
  "pt", # celkem unikátních prediktorů z pt.min, výčet uveden v "ptnu"
  "ptnu", # názvy unikátních prediktorů
  "species.p", "species.a", "species.ndop.p", env.sentinel_bio.names
)


create.tibble <- TRUE
for (species.name in species.names) {
  print(species.name)
  data <- rds_append[[species.name]]
  data.aucs <- sapply(data, function(x) as.numeric(x[[select.by]])) # x$sorensen x$auc

  first <- data[[env.sentinel_bio.names[[1]]]]


  # vyberu nejlepší procento (jednoprocentní rozsah = 0.01, nebo jiný) nejlepších modelů, které považuji za rovnocenné
  # auc.treshold <- max(data.aucs) - 0.01
  auc.treshold <- 0.75

  # data.aucs.treshold <- which(data.aucs >= auc.treshold)

  data.aucs.treshold <- which.max(data.aucs)



  data.predictors <- sapply(data, function(x) as.numeric(x$predictors.c))
  # vyberu počet použitých prediktorů pro modely s nejlepším AUC
  data.predictors.auc.treshold <- data.predictors[data.aucs.treshold]


  data.predictors.min.treshold <- min(data.predictors.auc.treshold)
  print(data.predictors.min.treshold)
  # kombinace prediktorů s největším AUC a nejmenším počtem prediktorů
  data.predictors.min <- which(data.predictors.auc.treshold == data.predictors.min.treshold)

  data.predictors.v <- sapply(data, function(x) x$predictors.v)
  selected.combs <- data.predictors.v[names(data.predictors.min)] # data.predictors.min / data.predictors.auc.treshold -  vybere všechny kombinace nad 0.75 (nejen s min tresholdem)
  # vybrané kombinace prediktor
  print(selected.combs)
  # vybrané unikátní prediktory
  selected.combs.unique <- unique(unlist(selected.combs))
  scu <- c(scu, selected.combs.unique) # unlist(selected.combs) nebo selected.combs.unique


  # vybírám tif s nejlepším auc/Sorensen a vykopíruju do nové složky
  str.preds <- str_replace_all(names(data.aucs.treshold), c("kfme16." = "", "_30" = "", "mean" = "me", "__" = "+")) # "\\.nd" = "",
  # rf <- list.files("C:/Users/petr/Downloads/igaD/preds/preds2/", pattern=paste0(species.name, ".*--", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", str.preds),"\\.tif") , full.names = TRUE)
  # print(rf)
  #  if(max(data.aucs) >= auc.treshold) file.copy( rf, "C:/Users/petr/Downloads/igaD/best-auc/")




  env.sentinel_bio.names.true <- which(env.sentinel_bio.names %in% selected.combs.unique)
  env.sentinel_bio.names.false <- which(env.sentinel_bio.names %notin% selected.combs.unique)

  env.sentinel_bio.names.tf <- env.sentinel_bio.names

  env.sentinel_bio.names.tf[env.sentinel_bio.names.true] <- 1
  env.sentinel_bio.names.tf[env.sentinel_bio.names.false] <- 0



  metrics.tss <- sapply(data, function(x) x[[paste0(select.by, ".metrics")]]$TSS)
  metrics.tss.selected <- metrics.tss[names(data.predictors.min)]

  metrics.main <- sapply(data, function(x) x[[select.by]])
  metrics.main.selected <- metrics.main[names(data.predictors.min)]

  if (select.by == "auc") {
    metrics.alt <- sapply(data, function(x) x[[paste0(select.by, ".metrics")]]$Sorensen)
    metrics.alt.selected <- metrics.alt[names(data.predictors.min)]
  } else {
    metrics.alt <- sapply(data, function(x) x$auc)
    metrics.alt.selected <- metrics.alt[names(data.predictors.min)]
  }



  row <- c(
    species.name,
    round(max(data.aucs), 3),
    round(max(metrics.main.selected), 3),
    round(max(metrics.tss.selected), 3),
    round(max(metrics.alt.selected), 3),
    data.predictors.min.treshold,
    length(env.sentinel_bio.names.tf[env.sentinel_bio.names.tf == 1]),
    str_replace_all(paste(selected.combs.unique, collapse = " | "), c("kfme16." = "", "_30" = "", "\\.nd" = "")),
    first$species.p,
    first$species.a,
    first$species.ndop.p,
    env.sentinel_bio.names.tf
  )
  names(row) <- cn
  if (create.tibble) {
    tbl <- bind_rows(row)
    create.tibble <- FALSE
    next
  } else {
    tbl %<>% add_row(bind_rows(row))
  }
}

tbl[[paste0(select.by, ".max.total")]] %<>% as.numeric
tbl[[paste0(select.by, ".max")]] %<>% as.numeric
tbl[[paste0(select.by.alt, ".max")]] %<>% as.numeric
tbl$tss.max %<>% as.numeric
tbl$pt %<>% as.integer
tbl$pt.min %<>% as.integer
tbl$species.p %<>% as.integer
tbl$species.ndop.p %<>% as.integer

# tbl %>% filter(auc > 0.75) %>% arrange(desc(auc)) %>% dplyr::select(species, auc, pt, ptnu)
print(tbl %>% filter(!!as.name(paste0(select.by, ".max")) >= 0.75 & !!as.name(paste0(select.by.alt, ".max")) >= 0.75) %>% arrange(pt.min, pt) %>% dplyr::select(species, !!as.name(paste0(select.by, ".max.total")), !!as.name(paste0(select.by, ".max")), tss.max, !!as.name(paste0(select.by.alt, ".max")), pt.min, pt, species.p, species.ndop.p, ptnu), n = 100)
print(tbl %>% filter(!!as.name(paste0(select.by, ".max")) >= 0.75) %>% arrange(pt.min, pt) %>% dplyr::select(species, !!as.name(paste0(select.by, ".max.total")), !!as.name(paste0(select.by, ".max")), tss.max, !!as.name(paste0(select.by.alt, ".max")), pt.min, pt, species.p, species.ndop.p, ptnu), n = 100)
# auc_pt <- tbl %>% filter(auc > 0.75) %>% arrange( pt) %>% dplyr::select(auc, pt)
# cor(auc_pt)

unique(scu)
table(scu)




joined_traits <- tbl %>%
  left_join(traits, by = c("species" = "species")) %>%
  filter(!is.na(Habitat))

joined_traits <- as_tibble(joined_traits)



# write.csv(tbl, paste0(path.igaD, "metrics.auc.based.csv"), row.names = FALSE)
write.csv(joined_traits, paste0(path.igaD, name.export, "--", select.by, ".based.csv"), row.names = FALSE)








tbl.sor <- tbl %>%
  filter(sorensen.max > 0.75 & auc.max > 0.75) %>%
  dplyr::select(species)
tbl.sor.top <- as.vector(unlist(tbl.sor))

tbl.auc <- tbl %>%
  filter(sorensen.max > 0.75 & auc.max > 0.75) %>%
  dplyr::select(species)
tbl.auc.top <- as.vector(unlist(tbl.auc))

setdiff(tbl.sor.top, tbl.auc.top)
setdiff(tbl.auc.top, tbl.sor.top)
match(tbl.auc.top, tbl.sor.top)
match(tbl.sor.top, tbl.auc.top)


raster_stack_best <- rasters_dir_stack(paste0(path.igaD, "../best-auc"), "tif")
# writeRaster(calc(raster_stack_best, median), paste0(path.igaD,"../best-auc-median.tif"))



####################### puvodni bias vyhodnoceni
# rds_append <- readRDS(paste0(path.igaD, "../rds-all/all-merged.rds"))
# lc <- readRDS("C:/Users/petr/Downloads/igaD/rds-prvni/0-10-Loxia curvirostra.rds")
lc <- rds_append[["Alcedo atthis"]]
# str(lc$kfme16.l8_30_4_ndvi_cv.nd, max.level=1)
# str(lc$kfme16.l8_30_5_raw_mean.B10__kfme16.l8_30_6_raw_cv.B1__kfme16.l8_30_4_mndwi_cv.nd__kfme16.l8_30_4_ndvi_cv.nd__kfme16.l8_30_5_mndwi_cv.nd__kfme16.l8_30_6_mndwi_cv.nd__kfme16.wc_30_6_cv.bio06__kfme16.wc_30_6_cv.bio11__kfme16.wc_30_6_mean.bio15$bias.metrics[[2]], max.level=1)

#  table(as.numeric(sapply(lc, function(x){x$adj.selected})))
aucs <- sapply(lc, function(x) {
  as.numeric(x$auc)
})
summary(aucs)
aucs.thr <- which(aucs > 0.75)



adj.selected <- as.numeric(sapply(lc, function(x) {
  as.numeric(x$adj.selected)
}))

adj.selected.thr <- adj.selected[aucs.thr]
#  table(as.numeric(sapply(lc, function(x){x$adj.selected})))
length(adj.selected.thr)
table(adj.selected.thr)

# získám četnost použití jednotlivých prediktorů nad daný treshold AUC
preds <- sapply(lc, function(x) {
  x$predictors.v
})
preds.thr <- unlist(preds[aucs.thr])
table(preds.thr)

plot(table(preds.thr))

# lc764 <- lc[764] # max AUC

bias.aucs <- sapply(lc, function(x) {
  unlist(x$bias.auc)
})


bias.aucs.t <- as_tibble(t(bias.aucs), rownames = "preds")
bias.aucs <- as_tibble(bias.aucs, rownames = "correction")


bias.aucs.t.thr <- bias.aucs.t %>% filter(preds %in% names(aucs.thr))
bias.aucs.t.thr.diff <- bias.aucs.t.thr %>% mutate(diff = `0` - pmax(`1`, `3`, `5`, `7`, `9`))
summary(bias.aucs.t.thr.diff$diff)
boxplot(bias.aucs.t.thr.diff$diff)








####################### nove bias vyhodnoceni


species.names <- names(rds_append)
create.tibble <- TRUE
for (species.name in species.names) {
  print(species.name)
  lc <- rds_append[[species.name]]


  # lc <- rds_append[["Alcedo atthis"]]
  # str(lc$kfme16.l8_30_4_ndvi_cv.nd, max.level=1)

  #  table(as.numeric(sapply(lc, function(x){x$adj.selected})))
  aucs <- sapply(lc, function(x) {
    as.numeric(x$auc)
  })
  summary(aucs)
  # aucs.thr <- which(aucs > 0.75)
  aucs.thr <- which.max(aucs)


  adj.selected <- as.numeric(sapply(lc, function(x) {
    as.numeric(x$adj.selected)
  }))

  adj.selected.thr <- adj.selected[aucs.thr]
  #  table(as.numeric(sapply(lc, function(x){x$adj.selected})))
  length(adj.selected.thr)
  table(adj.selected.thr)

  # získám četnost použití jednotlivých prediktorů nad daný treshold AUC
  preds <- sapply(lc, function(x) {
    x$predictors.v
  })
  preds.thr <- unlist(preds[aucs.thr])
  table(preds.thr)

  plot(table(preds.thr))

  # lc764 <- lc[764] # max AUC

  bias.aucs <- sapply(lc, function(x) {
    unlist(x$bias.auc)
  })


  bias.aucs.t <- as_tibble(t(bias.aucs), rownames = "preds")
  bias.aucs <- as_tibble(bias.aucs, rownames = "correction")


  bias.aucs.t.thr <- bias.aucs.t %>% filter(preds %in% names(aucs.thr))
  nc <- bias.aucs.t.thr %>% dplyr::select(matches("[0-9]"))
  bias.aucs.t.thr.diff <- bias.aucs.t.thr %>%
    mutate(auc_0 = `0`) %>%
    mutate(auc_biasCorrected = pmax(`1`, `3`, `5`, `7`, `9`)) %>%
    mutate(diff = `0` - pmax(`1`, `3`, `5`, `7`, `9`)) %>%
    mutate(auc_max = pmax(`0`, `1`, `3`, `5`, `7`, `9`)) %>%
    mutate(species = species.name) %>%
    mutate(which_max = names(nc)[max.col(nc)]) %>%
    mutate(auc_min = auc_max - abs(diff))


  if (create.tibble) {
    tbl <- bias.aucs.t.thr.diff
    create.tibble <- FALSE
    next
  } else {
    tbl %<>% add_row(bias.aucs.t.thr.diff)
  }
}

tbl$preds %<>% as.factor
tbl$species %<>% as.factor
tbl$which_max %<>% as.factor


write.csv(tbl, paste0(path.igaD, name.export, "--", ".auc.csv"), row.names = FALSE)


smr <- summary(tbl %>% filter(auc_max >= 0.75) %>% filter(abs(diff) >= 0.01))


summary(tbl %>% filter(auc_max >= 0.75) %>% filter(auc_0 >= 0.75))



library(broom)


# načte do RasterStack-u všechny rastry ze zadaného adresáře a dané přípony
rasters_dir_stack <- function(path_dir, raster_extension) {
  rasters_list <-
    list.files(
      path = path_dir,
      pattern = paste0("\\.", raster_extension, "$"),
      ignore.case = TRUE,
      full.names = TRUE
    )
  raster_stack <- stack(lapply(rasters_list, raster::raster))
  return(raster_stack)
}


raster_stack <- rasters_dir_stack(paste0(path.igaD, "kfme16_prediktory/"), "tif")

raster_stack <- dropLayer(raster_stack, grep("stdev|bio03|bio08|bio09", names(raster_stack)))
names(raster_stack) <- str_replace_all(names(raster_stack), c("kfme16." = "", "_30" = "", "\\.nd" = ""))




library(ENMToolsPB)
library(corrplot)

# cm <- raster.cor.matrix(raster_stack)
## uložit, jinak se dlouho počítá...
# saveRDS(cm, file = paste0(path.igaD, "correlation-matrix-92.rds"))

cm <- readRDS(paste0(path.igaD, "correlation-matrix-92.rds"))

### start dendro



dissimilarity <- 1 - cm
distance <- as.dist(dissimilarity)



library(stats)

pdf(paste0(path.igaD, "preds92-hclust.pdf"), width = 20, height = 14)
print(plot(hclust(distance), main = "CZ Dissimilarity = 1 - Correlation"))
dev.off()
### end dendro


#
# cp <- raster.cor.plot(raster_stack)
# saveRDS(cp, file = paste0(path.igaD, "correlation-plot-92.rds"))

cp <- readRDS(paste0(path.igaD, "correlation-plot-92.rds"))
cmc <- cor(cm)

# print(corrplot.mixed(cmc))

# corrplot
# M <- cor(cm)
# corrplot(M, method = "circle")


# http://evomics.org/learning/population-and-speciation-genomics/2018-population-and-speciation-genomics/environmental-correlation-analysis/

pdf(paste0(path.igaD, "preds92-corplots.pdf"), width = 20, height = 20)
print(cp$cor.mds.plot)
print(cp$cor.heatmap)
print(corrplot(cmc, order = "hclust", main = "hclust"))
print(corrplot(cmc, order = "AOE", main = "AOE"))
print(corrplot(cmc, order = "FPC", main = "FPC"))
dev.off()


#
# paste(str_replace_all(env.sentinel_bio.names, c("kfme16." = "", "_30" = "", "\\.nd" = "")), collapse = " ")








library(usdm)
vif.vif <- vif(stack.all[[-2]])
vif.vifcor <- vifcor(raster_stack[[1:3]], th = 0.3)
# vif.c <- vifstep(stack.all[[-2]], th = 10)






# vytvoří všechny kombinace bez opakování z vloženého vektoru (včetně možných jednotlivých tříd)
comb_all <- function(vector) {
  comb_list <- list()
  total <- 2 # length(vector)
  for (i in 1:total) {
    comb_list <- append(comb_list, combn(vector, i, simplify = FALSE))
  }
  return(comb_list)
}

length(comb_all(1:18))