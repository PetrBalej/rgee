# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "ENMToolsPB", "abind")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
# wd <- "D:/PERSONAL_DATA/pb/rgee"
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

# path.igaD <- "D:/PERSONAL_DATA/pb/igaD/"
path.igaD <- "/home/petr/Documents/igaD/"

stop()

# shp.grid <- shapefile(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
# shp.grid <- shapefile(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
# shp.grid <- shapefile(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_3035.shp"))

# excel.files <- list.files(paste0(path.igaD, "3km_prediktory"), full.names = TRUE, pattern = ".xls")


rcrs <- "+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

print("start:")
print(Sys.time())

# library(foreign)
# dbf <- read.dbf(paste0(path.igaD, "OneDrive_1_9-16-2022/linie_1km_sum.dbf"))
# write.csv(dbf, paste0(path.igaD, "OneDrive_1_9-16-2022/linie_1km_sum.csv"))

# library(openxlsx)
# write.xlsx(dbf, paste0(path.igaD, "OneDrive_1_9-16-2022/linie_1km_sum.xlsx"))



# ###
# ### raster z excelů s indexy
# ###
# for (excel.file.item in excel.files) {
#   print("***********************************************************")
#   excel.file <- read_excel(excel.file.item)
#   print(str(excel.file))

#   filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(excel.file.item))
#   print(filename)

#   # merge gridu shapefile a exceli podle ID
#   # m <- merge(shp.grid, excel.file, by='ID')
#   # m <- merge(shp.grid, excel.file, by = "FID_EEA_gr")
#   m <- merge(shp.grid, excel.file, by = "POLE")
#   # print(str(m))
#   print("jmena sloupcu mergnute")
#   print(names(m@data))


#   # náhoda, takto to cca vychází?! Ale to nejsou KFME gridy?
#   # ext <- extent(4470000, 4970000, 2830000, 3120000)

# ext <- extent(4455000, 4995000, 2800000, 3190000)
#   gridsize <- 1000
#   r <- raster(ext, res = gridsize)


#   ## field - výběr názvu sloupce s indexy obsahující slovo sentinel
#   rr <- rasterize(m, r, field = m@data[which(grepl("sentinel|variation|mean|cv", names(m@data)))])

#   crs(rr) <- rcrs

#   # raster::crs(r) <- "EPSG:3035"
#   # crs(r) <- CRS('+init=EPSG:3035')

#   print(paste0(path.igaD, filename, ".tif"))
#   writeRaster(rr, paste0(path.igaD, filename, "-delete1.tif"), format = "GTiff", overwrite = TRUE)
# }


# # # jen doplnění nul (0) místo NA
# # rr <- raster(paste0(path.igaD, "3km_prediktory/sitmap_linie.tif")) # prechod_pole_les_delka_1km.tif
# # rr[is.na(rr[])] <- 0
# # writeRaster(rr, paste0(path.igaD, "3km_prediktory/sitmap_linie.tif"), format = "GTiff", overwrite = TRUE)



####### nové načítání L8 a WC
kfme16.N <- readRDS(paste0(path.igaD, "kfme16-N_czechia_wc_l8_2018-2021_4-6.rds"))
kfme16.S <- readRDS(paste0(path.igaD, "kfme16-S_czechia_wc_l8_2018-2021_4-6.rds"))

# sloučit N a S
for (l1 in names(kfme16.N)) {
  kfme16.N[[l1]] %<>% add_row(kfme16.S[[l1]])
}

kfme16 <- kfme16.N

sf.grid <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326_czechia.shp"))


# vzorový KFME raster pro 16 subkvadrátů
r <- raster(
  xmn = 12.0834157383896326, # set minimum x coordinate
  xmx = 18.8750299183168408, # set maximum x coordinate
  ymn = 48.5500817521775048, # set minimum y coordinate
  ymx = 51.0750755551042701, # set maximum y coordinate
  res = c((1 / 6) / 4, 0.1 / 4), # resolution in c(x,y) direction
  vals = runif(16463, 0, 1)
)
# writeRaster(r, paste0(path.igaD, "kfme16-test.tif"))




for (l1 in names(kfme16)) {
  kfme16.t <- as_tibble(kfme16[[l1]])

  print(l1)

  names(kfme16[[l1]])[-1]

  for (band in names(kfme16[[l1]])[-1]) {
    print(band)
    kfme16.t.select <- kfme16.t %>% dplyr::select(all_of(c("POLE", band)))

    m <- merge(sf.grid, kfme16.t.select, by = "POLE")

    ## field - výběr názvu sloupce s indexy obsahující slovo sentinel
    rr <- rasterize(m, r, field = band)
    crs(rr) <- 4326

    writeRaster(rr, paste0(path.igaD, "kfme16-", l1, "-", band, ".tif"), format = "GTiff", overwrite = TRUE)

    # dodělávka CV
    if (grepl("stdev", l1, fixed = TRUE)) {
      l1.mean <- gsub("stdev", "mean", l1)
      print(l1.mean)
      kfme16.t.mean <- as_tibble(kfme16[[l1.mean]])
      kfme16.t.mean.select <- kfme16.t.mean %>% dplyr::select(all_of(c("POLE", band)))
      m.mean <- merge(sf.grid, kfme16.t.mean.select, by = "POLE")
      ## field - výběr názvu sloupce s indexy obsahující slovo sentinel
      rr.mean <- rasterize(m.mean, r, field = band)
      crs(rr.mean) <- 4326


      writeRaster(rr / rr.mean, paste0(path.igaD, "kfme16-", gsub("stdev", "cv", l1), "-", band, ".tif"), format = "GTiff", overwrite = TRUE)
    }
  }
}





































##############################
### hclust čistě s lokalitami
##############################

library(cluster)
library(factoextra)
library(dendextend)

csv.name <- "test-PA-lsdHod-binary.csv"
"%notin%" <- Negate("%in%")
res <- readRDS(paste0(path.igaD, "export-avif-lsd-2018-2022_utf8_3-6.rds"))
res.hodinovky <- readRDS(paste0(path.igaD, "exportavif--hodinovky-2018-2022_utf_3-6.rds"))

res$species %<>% as.character
res.hodinovky$species %<>% as.character

# sjednocení synonym
syns <- synonyms()

for (s in names(syns)) {
  matched <- res %>% filter(species == syns[[s]])
  if (nrow(matched) > 0) {
    res[res$species == syns[[s]], "species"] <- s
  }
}

for (s in names(syns)) {
  matched <- res.hodinovky %>% filter(species == syns[[s]])
  if (nrow(matched) > 0) {
    res.hodinovky[res.hodinovky$species == syns[[s]], "species"] <- s
  }
}



res %<>% dplyr::select(POLE, species, ObsListsID)
res.hodinovky %<>% dplyr::select(POLE, species, ObsListsID)
res %<>% add_row(res.hodinovky)
res %<>% st_transform(st_crs(4326))


# počet ObsListsID (jednotlivých LSD akcí) na POLE, alespoň 10 má 113 subkvadrátů
ObsListsID_per_sq <- res %>%
  group_by(POLE) %>%
  summarise(count = n_distinct(ObsListsID)) %>%
  arrange(desc(count)) %>%
  filter(count >= 10) %>%
  filter(!is.na(POLE)) %>%
  ungroup()

POLE.unique <- sort(as.vector(unique(ObsListsID_per_sq$POLE)))

res %<>% filter(POLE %in% POLE.unique)

# počet POLE na species
species <- res %>%
  filter(POLE %in% POLE.unique) %>%
  group_by(species) %>%
  summarise(count = n_distinct(POLE)) %>%
  arrange(desc(count)) %>%
  filter(count >= 30) %>%
  filter(count < 110) %>% # celkem je (183) (113) 141 subkvadrátů, potřebuju nějaké absence na ověření...
  filter(!is.na(species))

species.unique <- sort(as.vector(unique(species$species)))

res %<>% filter(species %in% species.unique)
cn <- TRUE

for (sp in unique(res$species)) {
  res.species <- res %>% filter(species == sp)
  res.species.presence <- as.vector(unique(res.species$POLE))
  res.species.absence <- setdiff(POLE.unique, res.species.presence)

  res.species.presence.v <- rep(1, length(res.species.presence))
  res.species.absence.v <- rep(0, length(res.species.absence))

  names(res.species.presence.v) <- res.species.presence
  names(res.species.absence.v) <- res.species.absence

  res.species.row <- append(
    append(res.species.presence.v, res.species.absence.v),
    list(species = sp, p = length(res.species.presence), a = length(res.species.absence))
  )
  res.species.row <- res.species.row[sort(names(res.species.row))]

  write.table(res.species.row,
    file = paste0(path.igaD, csv.name), sep = ",", append = TRUE, quote = TRUE,
    col.names = cn, row.names = FALSE
  )
  if (cn == TRUE) {
    # col.names jen poprvé
    cn <- FALSE
  }
}

# permImp <- read_delim(paste0(path.igaD, "/result_AUCs_sentinel.all.csv"), delim=",")
# permImp <- read_delim(paste0(path.igaD, "/lsdHod-result_AUCs_kfme16-glm-131-remove-lessPreds-nsim100.csv"), delim = ",")
#  permImp <- read_delim(paste0(path.igaD, "/lsdHod-result_AUCs_kfme16-glm-131-test-delete.csv"), delim = ",")
#  permImp <- read_delim(paste0(path.igaD, "/ndop-kfme16-test.csv"), delim = ",")
permImp <- read_delim(paste0(path.igaD, csv.name), delim = ",")


permImp.test <- permImp %>%
  relocate(species) %>%
  dplyr::select(-c(p, a)) %>%
  arrange(species)



### read bird traits table (traits according to Kolecek et al 2010)
traits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")

for (s in names(syns)) {
  matched <- traits %>% filter(species == syns[[s]])
  if (nrow(matched) > 0) {
    traits[traits$species == syns[[s]], "species"] <- s
  }
}
###
# join bird traits
permImp.test.traits <- permImp.test %>%
  dplyr::select(species) %>%
  left_join(traits, by = c("species" = "species")) %>%
  filter(!is.na(Habitat))


### read bird traits table 2
traits2 <- read_excel(paste0(path.igaD, "/functional_traits_GEB_Pearman.xlsx"), skip = 6)
traits2 %<>% mutate(species = gsub("_", " ", sp.names))

for (s in names(syns)) {
  matched <- traits2 %>% filter(species == syns[[s]])
  if (nrow(matched) > 0) {
    traits2[traits2$species == syns[[s]], "species"] <- s
  }
}
###
# join bird traits2
permImp.test.traits <- permImp.test.traits %>%
  left_join(traits2, by = c("species" = "species"))


# výběr sloupců s traits
traits.all <- permImp.test.traits %>%
  filter(species %in% permImp.test.traits$species) %>%
  dplyr::select(starts_with(c("Habitat", "Migration", "Distribution", "Protection", "e.", "f.", "b.", "3.")))



# dodatečná selekce zmizelých druhů po připojení traits
permImp.test.names <- permImp.test %>%
  filter(species %in% permImp.test.traits$species) %>%
  column_to_rownames(var = "species")
permImp.test.names.dist <- dist(permImp.test.names, method = "binary")
# dissimilarity  <- 1 - permImp.test.names.dist
# permImp.test.names.dist <- distance  <- as.dist(dissimilarity)
# plot(hclust(as.dist(1 - permImp.test.names.dist) ), main="permImp.test.names.dist")
hc <- hclust(permImp.test.names.dist)
plot(hc, main = "permImp.test.names.dist")
# hclust do formátu dendrogramu, aby šel upravit
dend <- as.dendrogram(hc)



pdf(paste0(path.igaD, csv.name, ".pdf"), width = 14, height = 6)
for (t in names(traits.all)) {
  # nutné seřadit druhy (s vazbou na habitat) tak aby odpovídaly řazení v dendrogramu a podbarvily se podle habitatu
  # labels_colors(dend) <- as.numeric(as.factor(permImp.auc75$Habitat[order.dendrogram(dend)]))

  labels_colors(dend) <- as.numeric(as.factor(permImp.test.traits[[`t`]][order.dendrogram(dend)]))
  # as.numeric(as.factor(permImp.auc75[["Habitat"]][order.dendrogram(dend)]))
  # permImp.auc75$Habitat
  par(cex = 0.7, mar = c(10, 2, 2, 2))
  plot(dend, main = t)
}

dev.off()










####################################################################
## landsat - 4-10 měsíc
##################################


raster_stack <-
  rasters_dir_stack(
    paste0(
      "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/l8-12/1000/"
    ),
    "tif"
  )
raster_stack <- dropLayer(raster_stack, grep("srtm|clc|count|bio03|bio08|bio09", names(raster_stack), ignore.case = TRUE))


rcrs <- crs(raster_stack)
raster_stack <- raster::mask(raster_stack, sum(raster_stack))

raster_stack <- stack_NA_repair(raster_stack)


















############################################################################################################
# tvorba bias rasteru pro ptáky 2018-2021
library(spatstat)

raster_stack <- stack(paste0(path.igaD, "kfme16-vifcor03-vidstep15.grd"))[[1]]
rcrs <- crs(raster_stack)

res.ndop <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))

res.ndop %<>% filter(precision <= 1000) %>% st_as_sf(coords = c("X", "Y"), crs = 3035)

res.ndop %<>% st_transform(st_crs(4326))

ext <- extent(raster_stack)
ow <- owin(xrange = c(ext@xmin, ext@xmax), yrange = c(ext@ymin, ext@ymax))

res.ndop.coords <- st_coordinates(res.ndop)
res.ndop.coords.ppp <- ppp(res.ndop.coords[, 1], res.ndop.coords[, 2], window = ow)



for (adj in 1:9) {
  raster_stack.bias <- resample(raster(density.ppp(res.ndop.coords.ppp, sigma = bw.scott.iso(res.ndop.coords.ppp), adjust = adj / 10)), raster_stack, method = "bilinear")
  r.min <- minValue(raster_stack.bias)
  r.max <- maxValue(raster_stack.bias)
  raster_stack.bias <- ((raster_stack.bias - r.min) / (r.max - r.min))
  crs(raster_stack.bias) <- rcrs

  raster_stack.bias <- mask(crop(raster_stack.bias, extent(raster_stack)), raster_stack)
  raster_stack.bias <- setMinMax(raster_stack.bias)

  writeRaster(raster_stack.bias, paste0(path.igaD, "bias-ptaci-adj-0.", as.character(adj), "-kfme16.tif"), format = "GTiff", overwrite = TRUE)
}
# # raster s 1, nulová varianta jako bez biasu
# raster_stack.bias[raster_stack.bias > 0] <- 1
# writeRaster(raster_stack.bias, paste0(path.igaD, "bias-ptaci-adj-0.0-kfme16.tif"), format = "GTiff", overwrite = TRUE)



























# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#################################################################################################################################################
# NDOP data evaluována lsdHod nad kfme16 - použití fitování adjust bias rasteru + fitování prediktorů
#################################################################################################################################################

results_name <- "test"
# kolik druhů má být v jedné skupině
speciesPerGroup <- 6
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- 1

data <- list()
data.species <- list()
data.species.best.model <- list()


# obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků
"%notin%" <- Negate("%in%")

sf.grid <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326.shp"))
#  env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor05-vidstep2.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor03-vidstep15.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor05-vidstep2.grd"))
# env.sentinel_bio <- dropLayer(env.sentinel_bio, which(names(env.sentinel_bio) == "kfme16.l8_30_5_mndwi_cv.nd") )
# env.sentinel_bio <- dropLayer(env.sentinel_bio, which(names(env.sentinel_bio) == c("kfme16.l8_30_6_mndwi_cv.nd", "kfme16.wc_30_6_cv.bio06")) )
# env.sentinel_bio <- env.sentinel_bio[[1:3]]
# env.sentinel_bio <- env.sentinel_bio[[c(3,5)]]


# raster_stack <- rasters_dir_stack(paste0(path.igaD,"kfme16_prediktory/"),"tif")
# env.sentinel_bio <- dropLayer(env.sentinel_bio,  grep("stdev|bio03|bio08|bio09", names(env.sentinel_bio)))
names(env.sentinel_bio) <- str_replace_all(names(env.sentinel_bio), c("kfme16." = "", "_30" = "", "\\.nd" = ""))


# sjednocení LSD s hodinovkama****************************************************************************
res <- readRDS(paste0(path.igaD, "export-avif-lsd-2018-2022_utf8_3-6.rds"))
res.hodinovky <- readRDS(paste0(path.igaD, "exportavif--hodinovky-2018-2022_utf_3-6.rds"))

res$species %<>% as.character
res.hodinovky$species %<>% as.character

# sjednocení synonym
res <- synonyms_unite(res)
res.hodinovky <- synonyms_unite(res.hodinovky)


res %<>% dplyr::select(POLE, species, ObsListsID)
res.hodinovky %<>% dplyr::select(POLE, species, ObsListsID)
res %<>% add_row(res.hodinovky)
res %<>% st_transform(st_crs(4326))


# počet ObsListsID (jednotlivých LSD akcí) na POLE, alespoň 10 má 113 subkvadrátů
ObsListsID_per_sq <- res %>%
  group_by(POLE) %>%
  summarise(count = n_distinct(ObsListsID)) %>%
  arrange(desc(count)) %>%
  filter(count >= 10) %>%
  filter(!is.na(POLE)) %>%
  ungroup()


# počet POLE na species
species <- res %>%
  filter(POLE %in% ObsListsID_per_sq$POLE) %>%
  group_by(species) %>%
  summarise(count = n_distinct(POLE)) %>%
  arrange(desc(count)) %>%
  filter(count >= 10) %>%
  filter(count < 120) %>% # celkem je (183) (113) 141 subkvadrátů, potřebuju nějaké absence na ověření...
  filter(!is.na(species))


res %<>% filter(POLE %in% ObsListsID_per_sq$POLE)

# st_write(
#   # res %>% dplyr::distinct(geometry),
#   res %>% filter(POLE %in% ObsListsID_per_sq$POLE),
#  paste0(path.igaD, "delete-distinct-geom-lds-hodinovky.shp"))



#####################################################################
# doplnění nálezů z NDOPu do použitých kvadrátů s cílem omezení false absence


res.ndop <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))

res.ndop %<>% filter(precision <= 1000) %>%
  st_as_sf(coords = c("X", "Y"), crs = 3035)

res.ndop$species %<>% as.character
res.ndop <- synonyms_unite(res.ndop)
res.ndop %<>% st_transform(st_crs(4326))
res.ndop %<>% st_intersection(sf.grid, res.ndop)



# počet POLE na species
species.ndop <- res.ndop %>%
  group_by(species) %>%
  summarise(count = n_distinct(key)) %>%
  arrange(desc(count)) %>%
  filter(count >= 30) %>%
  filter(!is.na(species))
# %>% st_transform(st_crs(4326))


species$species %<>% as.character



### Jen hnízdící druhy: http://fkcso.cz/fk/ptacicr.html
fkcso_csv <- read_csv(paste0(wd, "/species/ndop/ptaci-hnizdeni-do-2020-fkcso.csv"))
fkcso <- fkcso_csv %>%
  filter(cat == "A") %>%
  filter(grepl("h,|H,", type))

### read bird traits table (traits according to Kolecek et al 2010)
traits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")




fkcso <- synonyms_unite(fkcso)
# nahrazení názvů traits druhů novějšími názvy z NDOPu
traits <- synonyms_unite(traits)
# fkcso %>% anti_join(species, by = c("species" = "species"))

species.filtry <- species %>%
  filter(species %in% fkcso$species) %>%
  filter(species %notin% nepuvodni_problematicke()$nepuvodni) %>%
  filter(species %notin% nepuvodni_problematicke()$problematicke) %>%
  filter(species %notin% nepuvodni_problematicke()$nevhodne) %>%
  arrange(species)


###
# join bird traits
species.filtry.joined_traits <- species.filtry %>%
  left_join(traits, by = c("species" = "species")) %>%
  filter(!is.na(Habitat))
species.filtry.joined_traits <- as_tibble(species.filtry.joined_traits)
# joined_traits_anti <- species.filtry %>%
#    anti_join(traits, by = c("species" = "species"))

species.filtry.joined_traits$species %<>% as.character
species.filtry.joined_traits$Habitat %<>% as.factor
species.filtry.joined_traits$Migration %<>% as.factor
species.filtry.joined_traits$Distribution %<>% as.factor
species.filtry.joined_traits$Protection %<>% as.factor
# saveRDS(permImp.auc75.filtry.joined_traits$species, paste0(path.igaD,"vybrane-druhy.rds"))

# rozdělení druhů do skupin
species.parts <- split(species.filtry.joined_traits$species, ceiling(seq_along(species.filtry.joined_traits$species) / speciesPerGroup))
results_name <- paste0(results_name, "-", species.part)

for (sp in species.parts[[species.part]]) { # species.filtry.joined_traits$species # (species.filtry.joined_traits %>% filter(species == "Certhia familiaris" | species == "Ardea cinerea"))$species
  # (species.filtry.joined_traits %>% filter(species == "Periparus ater"))$species
  species.name <- sp #  Ciconia nigra, Coturnix coturnix, Mergus merganser, Crex crex, Falco subbuteo, Jynx torquilla, Asio otus, "Vanellus vanellus" Certhia familiaris Ardea cinerea

  # presence druhu lsdHod
  res.species.p <- res %>% filter(species == species.name)
  res.species.p.intersects <- st_intersects(sf.grid, res.species.p)
  res.species.p.intersects.true <- lengths(res.species.p.intersects) > 0 # where intersection is TRUE
  res.species.p.intersects.presence <- sf.grid[res.species.p.intersects.true, ]
  res.species.presence <- st_centroid(res.species.p.intersects.presence) %>% dplyr::distinct(POLE, .keep_all = TRUE)



  # absence druhu lsdHod (kvadráty kde nejsou presence)
  res.species.a <- res %>% filter(POLE %notin% res.species.p$POLE)
  res.species.a.intersects <- st_intersects(sf.grid, res.species.a)
  res.species.a.intersects.true <- lengths(res.species.a.intersects) > 0 # where intersection is TRUE
  res.species.a.intersects.absence <- sf.grid[res.species.a.intersects.true, ]
  res.species.absence <- st_centroid(res.species.a.intersects.absence) %>% dplyr::distinct(POLE, .keep_all = TRUE)



  # presence druhu NDOP
  res.ndop.species.p <- res.ndop %>% filter(species == species.name)
  res.ndop.species.p.intersects <- st_intersects(sf.grid, res.ndop.species.p)
  res.ndop.species.p.intersects.true <- lengths(res.ndop.species.p.intersects) > 0 # where intersection is TRUE
  res.ndop.species.p.intersects.presence <- sf.grid[res.ndop.species.p.intersects.true, ]
  res.ndop.species.presence <- st_centroid(res.ndop.species.p.intersects.presence) %>% dplyr::distinct(POLE, .keep_all = TRUE)


  if (nrow(res.species.presence) < 10 | nrow(res.ndop.species.presence) < 10) {
    print("******************************************************************************************************************************************")
    print(species.name)
    print(nrow(res.species.presence))
    print(nrow(res.ndop.species.presence))
    next
  }


  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species.selected <- ENMToolsPB::enmtools.species(env.sentinel_bio[[1]], as.data.frame(st_coordinates(res.species.presence$geometry)),
    background.points = as.data.frame(st_coordinates(res.species.absence$geometry)),
    species.name = species.name
  )

  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species.selected.ndop <- ENMToolsPB::enmtools.species(env.sentinel_bio[[1]], as.data.frame(st_coordinates(res.ndop.species.presence$geometry)),
    # background.points=as.data.frame(st_coordinates(absence$geometry)),
    species.name = species.name
  )


  replicates <- 3
  k_classes <- 2
  # ###### výběr s odebíráním prediktorů
  # data.species[[species.name]] <- data <- permImp_remove_last(species.selected, species.selected.ndop, env.sentinel_bio, path.igaD, replicates)
  # data.maxAUC <- rev(round(sapply(data, function(x) x$auc), 2)) # záměrně zaokrouhlím na 2 des. místa a pak dám opačné pořadí, aby byl dále vybrán model s nejmenším počtem prediktorů, pokud AUC osciluje kolem jedné hodnoty
  # data.maxAUC.value <- as.numeric(data.maxAUC[which.max(data.maxAUC)])
  # data.maxAUC.index <- names(data.maxAUC[which.max(data.maxAUC)])
  # data.species.best.model[[species.name]] <- best.model <- data[[data.maxAUC.index]]

  ###### výběr se všemi kombinacemi prediktorů
  data.species[[species.name]] <- data <- permImp_comb(species.selected, species.selected.ndop, env.sentinel_bio, path.igaD, replicates, k_classes)



  #   data.aucs <- sapply(data, function(x) x$auc)

  #   # vyberu nejlepší procento (jednoprocentní rozsah) nejlepších modelů, které považuji za rovnocenné
  #   auc.treshold <- max(data.aucs) - 0.01
  #   data.aucs.treshold <- which(data.aucs >= auc.treshold)

  #   data.predictors  <- sapply(data, function(x) x$predictors.c)
  #   # vyberu počet použitých prediktorů pro modely s nejlepším AUC
  #   data.predictors.auc.treshold <- data.predictors[data.aucs.treshold]

  #   data.predictors.min.treshold <- min(data.predictors.auc.treshold)
  #   print(data.predictors.min.treshold )
  #   # kombinace prediktorů s největším AUC a nejmenším počtem prediktorů
  #   data.predictors.min <- which(data.predictors.auc.treshold == data.predictors.min.treshold)

  #   data.predictors.v  <- sapply(data, function(x) x$predictors.v)

  #   unique(unlist(data.predictors.v[names(data.predictors.min)]))

  # #  as.numeric(sapply(data, function(x) x$adj.selected.sorensen))


  # raději ukládám i průběžné výsledky jednotlivých druhů zvlášť
  saveRDS(data, file = paste0(path.igaD, results_name, "-", species.name, ".rds"))
  print(Sys.time())
}
saveRDS(data.species, file = paste0(path.igaD, results_name, ".rds"))
# saveRDS(data.species.best.model, file = paste0(path.igaD, results_name, "-best.model.rds"))
# plot(sapply(data.species$`Luscinia megarhynchos`, function(x) x$auc))
# plot(sapply(d$`Certhia familiaris`, function(x) x$adj.selected))


print("konec:")
print(Sys.time())

#  m[[1]]$model$coefficients
# str(m[[1]], max.level=2)
# m[[1]]$conf




# # ### rychlá vizuální verifikace PA
# str(data$ba.m[[1]][[1]], max.level=1)
# m0 <- data$ba.m[[1]][[1]]
# writeRaster(m0$suitability, paste0(path.igaD, "smazat-Alcedo-atthis.tif"))
# s <- m0$suitability
# s[m0$suitability < 0.5094012] <- 0
# s[m0$suitability >= 0.5094012] <- 1
# writeRaster(s, paste0(path.igaD, "smazat-Alcedo-atthis-binary.tif"))
# plogis(m0$thr$spec_sens) # 0.5094012
# m0$thr$spec_sens
# m0$conf
# m0$test.evaluation@auc
# st_write(res.species.presence$geometry,paste0(path.igaD, "smazat-Alcedo-atthis-P.shp"))
# st_write(res.species.absence$geometry,paste0(path.igaD, "smazat-Alcedo-atthis-A.shp"))




#
# získání příkladu generování background biasu
#
bias <- readRDS("/home/petr/Documents/igaD/bias-generating.rds")


bias.bg <- sapply(bias, function(x) x$background.points)
# str(bias.bg)
# bias.bg[[1]]
# length(bias.bg)

bias.coords <- list()
for (i in 1:(length(bias.bg) / 2)) {
  print(i)
  add <- i - 1
  print(add)
  bias.coords[[i]] <- list(
    lon = bias.bg[[i + add]], # lon
    lat = bias.bg[[i + add + 1]] # lat
  )

  xy <- Pair(bias.bg[[i + add]], bias.bg[[i + add + 1]])

  png(paste0(path.igaD, "bias-test-", i, ".png"), width = 1000, height = 600)
  plot(xy, pch = 19, cex = 0.1, alpha = 0.5)
  dev.off()


  # write.csv(xy, paste0(path.igaD, "bias-PerAte-",i, ".csv"))
}










#
# vytažení dat z průběžných výsledků jednotlivých druhů a uložení rds
#

"%notin%" <- Negate("%in%")
env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor03-vidstep15.grd"))

# env.sentinel_bio <- dropLayer(env.sentinel_bio, which(names(env.sentinel_bio) == c("kfme16.l8_30_6_mndwi_cv.nd", "kfme16.wc_30_6_cv.bio06"))) # nutné po vifcor 0.2

env.sentinel_bio.names <- names(env.sentinel_bio)

# načtení RDS jednotlivých druhů, spojení do jednoho listu
rds_list <-
  list.files(
    path = paste0(path.igaD, "rds-all02-final"),
    # původní: "^glm_fmt_", d, "_.*\\.rds$"
    # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
    pattern = paste0("[A-z]+ [a-z]+\\.rds$"), # "^enmsr_glmE[0-9]_.+\\.rds$"  "^enmsr_xxx[0-9]_.+\\.rds$"   enmsr_glmF0_5000_3_1621355712.12381.rds       "^enmsr_glm2PATEST[0-9]_.+\\.rds$"
    ignore.case = TRUE,
    full.names = TRUE
  )
species.names <- regmatches(rds_list, regexpr("([A-z]+ [A-z]+)", rds_list))
rds_append <- list()
# rds_append <- sapply(species.names, function(x) NA)
for (i in seq_along(rds_list)) {
  print(species.name)
  species.name <- regmatches(rds_list[[i]], regexpr("([A-z]+ [A-z]+)", rds_list[[i]]))
  rds_append[[species.name]] <- readRDS(rds_list[[i]])
}

# ### uložení kompletních výsledků z průběžných druhových
# saveRDS(rds_append, file = paste0(path.igaD, "all-merged.rds"))

# ### ukázka struktury
# str(rds_append[["Cygnus olor"]]$kfme16.l8_30_4_raw_cv.B5__kfme16.l8_30_4_mndwi_cv.nd, max.level=1)

# soubor s výsledky modelů
rds_append <- readRDS(paste0(path.igaD, "all-merged.rds"))
species.names <- names(rds_append)
cn <- c("species", "auc", "pt", env.sentinel_bio.names)
create.tibble <- TRUE
for (species.name in species.names) {
  print(species.name)
  data <- rds_append[[species.name]]
  data.aucs <- sapply(data, function(x) x$auc) # x$sorensen x$auc

  # vyberu nejlepší procento (jednoprocentní rozsah = 0.01, nebo jiný) nejlepších modelů, které považuji za rovnocenné
  auc.treshold <- max(data.aucs) - 0.01
  data.aucs.treshold <- which(data.aucs >= auc.treshold)

  data.predictors <- sapply(data, function(x) x$predictors.c)
  # vyberu počet použitých prediktorů pro modely s nejlepším AUC
  data.predictors.auc.treshold <- data.predictors[data.aucs.treshold]

  data.predictors.min.treshold <- min(data.predictors.auc.treshold)
  print(data.predictors.min.treshold)
  # kombinace prediktorů s největším AUC a nejmenším počtem prediktorů
  data.predictors.min <- which(data.predictors.auc.treshold == data.predictors.min.treshold)

  data.predictors.v <- sapply(data, function(x) x$predictors.v)
  selected.combs <- data.predictors.v[names(data.predictors.min)]
  # vybrané kombinace prediktorů
  print(selected.combs)
  # vybrané unikátní prediktory
  selected.combs.unique <- unique(unlist(selected.combs))

  env.sentinel_bio.names.true <- which(env.sentinel_bio.names %in% selected.combs.unique)
  env.sentinel_bio.names.false <- which(env.sentinel_bio.names %notin% selected.combs.unique)

  env.sentinel_bio.names.tf <- env.sentinel_bio.names

  env.sentinel_bio.names.tf[env.sentinel_bio.names.true] <- 1
  env.sentinel_bio.names.tf[env.sentinel_bio.names.false] <- 0

  row <- c(species.name, max(data.aucs), length(env.sentinel_bio.names.tf[env.sentinel_bio.names.tf == 1]), env.sentinel_bio.names.tf)
  names(row) <- cn
  if (create.tibble) {
    tbl <- bind_rows(row)
    create.tibble <- FALSE
    next
  } else {
    tbl %<>% add_row(bind_rows(row))
  }
}

# tbl %>% filter(auc > 0.75) %>% arrange(desc(auc)) %>% dplyr::select(species, auc, pt)






























################### cluster nově
# auc limit 0.05 + auc >= 0.75 + dist(method = "binary") hclust(method = "average") vycvhází OK
library(cluster)
library(factoextra)
library(dendextend)

permImp <- tbl

permImp.auc75 <- permImp %>%
  filter(auc >= 0.75) %>%
  arrange(species)

### read bird traits table (traits according to Kolecek et al 2010)
traits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")

permImp.auc75 <- synonyms_unite(permImp.auc75)


###
# join bird traits
permImp.traits <- permImp.auc75 %>%
  left_join(traits, by = c("species" = "species")) %>%
  filter(!is.na(Habitat)) %>%
  dplyr::select(-species_old)




traits2 <- read_excel(paste0(path.igaD, "/functional_traits_GEB_Pearman.xlsx"), skip = 6)
traits2 %<>% mutate(species = gsub("_", " ", sp.names))


traits2 <- synonyms_unite(traits2)
###
# join bird traits
permImp.traits2 <- permImp.traits %>%
  left_join(traits2, by = c("species" = "species"))
# %>% filter(!is.na(Habitat))
permImp.traits2 <- as_tibble(permImp.traits2)
# joined_traits_anti <- species.filtry %>%
#    anti_join(traits, by = c("species" = "species"))


permImp.auc75 <- permImp.traits2 %>%
  arrange(species)


permImp.test <- drop_na(subset(permImp.auc75, select = -c(
  pt,
  auc, Migration, Distribution, Habitat, Protection, sp.names, code
))) %>% dplyr::select(-starts_with(c("e.", "f.", "b.", "3.")))
permImp.test.names <- permImp.test %>% column_to_rownames(var = "species")
permImp.test.names.dist <- dist(permImp.test.names, method = "binary")


# dissimilarity  <- 1 - permImp.test.names.dist
# permImp.test.names.dist <- distance  <- as.dist(dissimilarity)


# plot(hclust(as.dist(1 - permImp.test.names.dist) ), main="permImp.test.names.dist")

hc <- hclust(permImp.test.names.dist, method = "average")
plot(hc, main = "permImp.test.names.dist")


# hclust do formátu dendrogramu, aby šel upravit
dend <- as.dendrogram(hc)

permImp.auc75.f <- permImp.auc75 %>% filter(species %in% permImp.test$species)

traits.all <- subset(permImp.auc75.f, select = -c(
  pt,
  auc, sp.names, code, species, `e.bodymass(g)`
)) %>% dplyr::select(-starts_with(c("kfme16.", "l8_", "wc_")))


pdf(paste0(path.igaD, "hclust.pdf"), width = 14, height = 6)

for (t in names(traits.all)) {
  # nutné seřadit druhy (s vazbou na habitat) tak aby odpovídaly řazení v dendrogramu a podbarvily se podle habitatu
  # labels_colors(dend) <- as.numeric(as.factor(permImp.auc75$Habitat[order.dendrogram(dend)]))

  labels_colors(dend) <- as.numeric(as.factor(permImp.auc75.f[[`t`]][order.dendrogram(dend)]))
  # as.numeric(as.factor(permImp.auc75[["Habitat"]][order.dendrogram(dend)]))
  # permImp.auc75$Habitat
  par(cex = 0.7, mar = c(10, 2, 2, 2))
  plot(dend, main = t)
}

dev.off()










#
# # modifikovaný usdm s přidaným výstupem @excludedPairs se seznamem korelovaných prediktorů vzhledem k finálním vybraným
#
stop()
library(usdmPB) # /mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/usdmPB
raster_stack <-
  rasters_dir_stack(
    paste0(
      "/home/petr/Documents/igaD/kfme16_prediktory/"
    ),
    "tif"
  )
# raster_stack <- dropLayer(raster_stack, grep("srtm|clc|count|bio03|bio08|bio09|stdev", names(raster_stack), ignore.case = TRUE))
# raster_stack <- dropLayer(raster_stack, grep("srtm|clc|count|bio03|bio08|bio09|stdev|mean", names(raster_stack), ignore.case = TRUE))
raster_stack <- dropLayer(raster_stack, grep("srtm|clc|count|bio03|bio08|bio09|stdev|bio", names(raster_stack), ignore.case = TRUE))


vif.vifcor <- usdmPB::vifcor(raster_stack, th = 0.5, maxobservations = 100000)
vif.vifcor
vif.vifcor@excludedPairs

rs_names <- names(raster_stack)
rs_indexes <- seq_along(names(raster_stack))
rs_ex <- match(vif.vifcor@excluded, rs_names)

raster_stack_cor.cv <- dropLayer(raster_stack, rs_ex) # L8
rr <- writeRaster(raster_stack_cor.cv, paste0(path.igaD, "kfme16-vifcor05-vidstep2-cvMean.grd"), format = "raster", overwrite = TRUE)
hdr(rr, format = "ENVI")


# vif.vifcor <- names(vif.vifcor@excludedPairs)

saveRDS(vif.vifcor, paste0(path.igaD, "vif.vifcor_excludedPairs---kfme16-vifcor05-vidstep2-cvMean.rds"))
# vif.vifcor <- readRDS(paste0(path.igaD, "vif.vifcor_excludedPairs-04.rds"))

vif.selected <- as.vector(vif.vifcor@results$Variables)

vif.diff <- setdiff(vif.selected, names(vif.vifcor@excludedPairs))


vif.vifcor.tree <- append(
  vif.vifcor@excludedPairs,
  # setNames(rep(NA, length(vif.diff)), setdiff(env.sentinel_bio.names, names(vif.vifcor@excludedPairs)))
  setNames(rep(NA, length(vif.diff)), vif.diff)
)

saveRDS(vif.vifcor.tree, paste0(path.igaD, "vif.vifcor_tree---vif.vifcor_excludedPairs---kfme16-vifcor05-vidstep2-cvMean.rds"))

vif.vifcor.tree <- readRDS(paste0(path.igaD, "vif.vifcor_tree-04.rds"))









### zobrazení jak se generuje BG s/bez korekce bias rasterem
# nback <- 10000

# bias_czechia <- raster(paste0(path.igaD, "bias-ptaci-adj-0.1-kfme16.tif"))
# ### bez korekce BG bias rasterem
# background.points <- as.data.frame(rasterToPoints(bias_czechia)[, 1:2])
# inds <- sample(1:nrow(background.points), size = nback, replace = TRUE)
# background.points.zero <- background.points[inds, ]

# ### s korekcí BG bias rasterem
# # Drawing background points from sample raster
# background.points <- as.data.frame(rasterToPoints(bias_czechia))
# inds <- sample(1:nrow(background.points),
#   size = nback,
#   prob = background.points[, 3],
#   replace = TRUE
# )
# background.points.bias <- background.points[inds, 1:2]