# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "ENMToolsPB", "abind")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

path.igaD <- "/home/petr/Documents/igaD/"


# shp.grid <- shapefile(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
# shp.grid <- shapefile(paste0(path.igaD, "EEA_site/EEA_1km.shp"))

shp.grid <- shapefile(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_3035.shp"))



excel.files <- list.files(paste0(path.igaD, "3km_prediktory"), full.names = TRUE, pattern = ".xls")


rcrs <- "+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

print("start:")
print(Sys.time())

# library(foreign)
# dbf <- read.dbf(paste0(path.igaD, "OneDrive_1_9-16-2022/linie_1km_sum.dbf"))
# write.csv(dbf, paste0(path.igaD, "OneDrive_1_9-16-2022/linie_1km_sum.csv"))

# library(openxlsx)
# write.xlsx(dbf, paste0(path.igaD, "OneDrive_1_9-16-2022/linie_1km_sum.xlsx"))

# ### raster z excelů s indexy
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








# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
st_crs(sf.grid) <- 3035


# source(paste0(getwd(), "/R/export_raster/ndop_ugc_WS2.R"))
# res <- ndop_ugc(list(from = "2018-01-01", to = "2021-12-31"), list(from = 1, to = 12), "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/zal", 3035, 1000)
# res <- res %>% filter(cat == "Ptáci")
# # res %>% filter(species == "Ciconia nigra")
# saveRDS(res, file = paste0(path.igaD, "ptaci_ndop_2018-2021.rds"))

# res <- ndop_ugc(list(from = "2018-01-01", to = "2021-12-31"), list(from = 1, to = 12), "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/zal", 3035, 1000)
# res <- res %>% filter(cat == "Ptáci")
# saveRDS(res, file = paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))


res <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))

species <- res %>%
  group_by(species) %>%
  summarise(count = n_distinct(key)) %>%
  arrange(desc(count)) %>%
  filter(count >= 100) %>%
  filter(count < 10000) %>%
  filter(!is.na(species))


write.table(
  data.frame("species", "bioclim", "bioclim_landsat", "bioclim_indices", "bioclim_landsat_indices", "1km_occupied_count"),
  file = paste0(path.igaD, "result_AUCs.csv"), sep = ",", append = TRUE, quote = TRUE,
  col.names = FALSE, row.names = FALSE
)

for (sp in species$species) {
  species.name <- sp #  Ciconia nigra, Coturnix coturnix, Mergus merganser, Crex crex, Falco subbuteo, Jynx torquilla, Asio otus, "Vanellus vanellus"
  # res <- res %>% filter(species == "Ciconia nigra")



  res.species <- res %>% filter(species == species.name)


  res.sf <- st_as_sf(x = res.species, coords = c("X", "Y"), crs = rcrs)
  st_crs(res.sf) <- 3035


  ### udělat (rychlejším způsobem - svým dříve) per pixel z funkcí, pokud nepotřebuju absence
  grid.intersects <- st_intersects(sf.grid, res.sf)

  sf.grid.true <- lengths(grid.intersects) > 0 # Finds only countries where intersection is TRUE
  sf.grid.presence <- sf.grid[sf.grid.true, ]
  presence <- st_centroid(sf.grid.presence)
  if (nrow(presence) < 100) {
    next
  }
  # sf.grid.false <-  lengths(grid.intersects) <= 0
  # sf.grid.absence <-  sf.grid[sf.grid.false,]
  # absence <- st_centroid(sf.grid.absence)


  # ENMTools
  #  env.files <- list.files(path = path.igaD, pattern = "1km_cv.tif$", full.names = TRUE)
  #  env <- stack(env.files)
  #  env <- setMinMax(env)

  #  env.files <- list.files(path = paste0(path.igaD, "1km_prediktory2"), pattern = ".tif$", full.names = TRUE)
  #  env <- stack(env.files)
  #  env <- setMinMax(env)


  env.bio <- stack(paste0(path.igaD, "igaD-1km-czechia-bio.grd"))
  env.biol8 <- stack(paste0(path.igaD, "igaD-1km-czechia-biol8.grd"))
  env.bioi <- stack(paste0(path.igaD, "igaD-1km-czechia-bioi.grd"))
  env.all <- stack(paste0(path.igaD, "igaD-1km-czechia-all.grd"))
  # kolik by udělalo přidání samotného corine? (se sloučenými třídami - umí s tím enmtools???) - tohle musím také přidat nebo zahrnout jako další variantu!!!!


  # library(ENMTools)
  # env <- check.env(env)







  # species <- enmtools.species(env.bio[[1]], as.data.frame(st_coordinates(presence)), as.data.frame(st_coordinates(absence)), species.name)

  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species <- enmtools.species(env.bio[[1]], as.data.frame(st_coordinates(presence)), NA, species.name)

  # cm <- raster.cor.matrix(env)
  # cp <- raster.cor.plot(env)



  # env <- env[[c("B5_10km_cv", "B6_10km_cv", "ndvi_10km_cv")]]


  # # species.glm <- enmtools.glm(species = species, env = env, test.prop = "block")
  # species.glm <- enmtools.glm(species = species, env = env, test.prop = 0.3)
  # # species.glm <- enmtools.glm(species = species, env = env, nback=10000, test.prop = 0.3)

  # species.glm.vip <- enmtools.vip(species.glm)

  replicates <- 10
  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.bio, test.prop = 0.3, nback = 10000)
  }
  m.auc.bio <- median(sapply(m, function(x) x$test.evaluation@auc))


  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.biol8, test.prop = 0.3, nback = 10000)
  }
  m.auc.biol8 <- median(sapply(m, function(x) x$test.evaluation@auc))


  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.bioi, test.prop = 0.3, nback = 10000)
  }
  m.auc.bioi <- median(sapply(m, function(x) x$test.evaluation@auc))


  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.all, test.prop = 0.3, nback = 10000)
  }
  m.auc.all <- median(sapply(m, function(x) x$test.evaluation@auc))


  write.table(
    data.frame(species.name, round(m.auc.bio, digits = 3), round(m.auc.biol8, digits = 3), round(m.auc.bioi, digits = 3), round(m.auc.all, digits = 3), nrow(presence)),
    file = paste0(path.igaD, "result_AUCs.csv"), sep = ",", append = TRUE, quote = TRUE,
    col.names = FALSE, row.names = FALSE
  )

  print(paste0(species.name, ",", round(m.auc.bio, digits = 3), ",", round(m.auc.biol8, digits = 3), ",", round(m.auc.bioi, digits = 3), ",", round(m.auc.all, digits = 3), ",", nrow(presence)))
  print(Sys.time())
}

print("konec:")
print(Sys.time())
stop()








################################################################################################################################################################################
# s VIP
enm_mxt_gbif.vip <- sapply(m, enmtools.vip)

enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
  as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
})

if (replicates == 1) {
  enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
} else {
  b_g <- enm_mxt_gbif.vip.t[[1]]

  for (n in 1:replicates) {
    if (n > 1) {
      b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
    }
  }
  enm_mxt_gbif.vip.s <- b_g %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
}
enm_mxt_gbif.vip.s.z <- enm_mxt_gbif.vip.s %>% unite("enm_mxt_gbif.vip", names(enm_mxt_gbif.vip.t[[1]])) # separate(xy, c("x", "y"))


m.vip <- sapply(m, enmtools.vip)



stop()

env2 <- stack(paste0(path.igaD, "czechia3-10000.grd"))

# nechat extent czechia (opačně nejde)
env.crop <- crop(env, extent(env2))
env.crop <- mask(env.crop, env2[[c(1, 2, 3)]])

stack.all <- stack(env2, env.crop)
stack.all <- mask(stack.all, stack.i)

stack.i <- stack(env.crop, env.crop)
stack.i <- stack(stack.i, stack.i)
stack.i <- stack(stack.i, env.crop)
stack.i <- stack.i[[-c(14, 15)]]
stack.all <- mask(stack.all, stack.i)


library(usdm)
vif.a <- vif(stack.all[[-2]])
vif.b <- vifcor(stack.all[[-2]], th = 0.7)
vif.c <- vifstep(stack.all[[-2]], th = 10)

names(stack.all[[-c(2, 11)]])

rr <- writeRaster(stack.all[[-c(2, 11)]], paste0(path.igaD, "czechia3-10000-2indexy.grd"), format = "raster", overwrite = TRUE)
hdr(rr, format = "ENVI")































env.sentinel <- stack(paste0(path.igaD, "igaD-1km-czechia-mean-sentinel.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "igaD-1km-czechia-cv-sentinel.grd"))

# env.sentinel <- dropLayer(env.sentinel, c(6)) # c(1, 31, 32)
# env.sentinel_bio <- dropLayer(env.sentinel_bio, c(9)) # c(1, 31, 32)




# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
st_crs(sf.grid) <- 3035


res <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))

species <- res %>%
  group_by(species) %>%
  summarise(count = n_distinct(key)) %>%
  arrange(desc(count)) %>%
  filter(count >= 100) %>%
  filter(count < 10000) %>%
  filter(!is.na(species))


write.table(
  data.frame("species", "cv", "mean", "1km_occupied_count"),
  file = paste0(path.igaD, "result_AUCs_sentinel-cvMean.csv"), sep = ",", append = TRUE, quote = TRUE,
  col.names = FALSE, row.names = FALSE
)

for (sp in species$species) {
  species.name <- sp #  Ciconia nigra, Coturnix coturnix, Mergus merganser, Crex crex, Falco subbuteo, Jynx torquilla, Asio otus, "Vanellus vanellus"
  # res <- res %>% filter(species == "Ciconia nigra")



  res.species <- res %>% filter(species == species.name)


  res.sf <- st_as_sf(x = res.species, coords = c("X", "Y"), crs = rcrs)
  st_crs(res.sf) <- 3035


  ### udělat (rychlejším způsobem - svým dříve) per pixel z funkcí, pokud nepotřebuju absence
  grid.intersects <- st_intersects(sf.grid, res.sf)

  sf.grid.true <- lengths(grid.intersects) > 0 # Finds only countries where intersection is TRUE
  sf.grid.presence <- sf.grid[sf.grid.true, ]
  presence <- st_centroid(sf.grid.presence)
  if (nrow(presence) < 100) {
    next
  }
  # sf.grid.false <-  lengths(grid.intersects) <= 0
  # sf.grid.absence <-  sf.grid[sf.grid.false,]
  # absence <- st_centroid(sf.grid.absence)


  # ENMTools
  #  env.files <- list.files(path = path.igaD, pattern = "1km_cv.tif$", full.names = TRUE)
  #  env <- stack(env.files)
  #  env <- setMinMax(env)

  #  env.files <- list.files(path = paste0(path.igaD, "1km_prediktory2"), pattern = ".tif$", full.names = TRUE)
  #  env <- stack(env.files)
  #  env <- setMinMax(env)


  ######

  # kolik by udělalo přidání samotného corine? (se sloučenými třídami - umí s tím enmtools???) - tohle musím také přidat nebo zahrnout jako další variantu!!!!


  # library(ENMTools)
  # env <- check.env(env)







  # species <- enmtools.species(env.bio[[1]], as.data.frame(st_coordinates(presence)), as.data.frame(st_coordinates(absence)), species.name)

  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species <- enmtools.species(env.sentinel[[1]], as.data.frame(st_coordinates(presence)), NA, species.name)

  # cm <- raster.cor.matrix(env)
  # cp <- raster.cor.plot(env)



  # env <- env[[c("B5_10km_cv", "B6_10km_cv", "ndvi_10km_cv")]]


  # # species.glm <- enmtools.glm(species = species, env = env, test.prop = "block")
  # species.glm <- enmtools.glm(species = species, env = env, test.prop = 0.3)
  # # species.glm <- enmtools.glm(species = species, env = env, nback=10000, test.prop = 0.3)

  # species.glm.vip <- enmtools.vip(species.glm)

  replicates <- 10
  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.sentinel, test.prop = 0.3, nback = 10000)
  }
  m.auc.sentinel <- median(sapply(m, function(x) x$test.evaluation@auc))


  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.sentinel_bio, test.prop = 0.3, nback = 10000)
  }
  m.auc.sentinel_bio <- median(sapply(m, function(x) x$test.evaluation@auc))


  write.table(
    data.frame(species.name, round(m.auc.sentinel, digits = 3), round(m.auc.sentinel_bio, digits = 3), nrow(presence)),
    file = paste0(path.igaD, "result_AUCs_sentinel-cvMean.csv"), sep = ",", append = TRUE, quote = TRUE,
    col.names = FALSE, row.names = FALSE
  )

  print(paste0(species.name, ",", round(m.auc.sentinel, digits = 3), ",", round(m.auc.sentinel_bio, digits = 3), ",", nrow(presence)))
  print(Sys.time())
}

print("konec:")
print(Sys.time())


stop()
































################################################################################################################################################################################
# s VIP



# env.sentinel <- stack(paste0(path.igaD, "igaD-1km-czechia-all-sentinel.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "igaD-1km-czechia-sentinel-cv.bio-0.7-.grd"))


# bias_czechia <- raster(paste0(path.igaD, "bias-ptaci-adj-0.8-1km.tif"))

# env.sentinel <- dropLayer(env.sentinel, c(6)) # c(1, 31, 32)
# env.sentinel_bio <- dropLayer(env.sentinel_bio, c(9)) # c(1, 31, 32)




# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
st_crs(sf.grid) <- 3035


res <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))

species <- res %>%
  group_by(species) %>%
  summarise(count = n_distinct(key)) %>%
  arrange(desc(count)) %>%
  filter(count >= 100) %>%
  filter(count < 10000) %>%
  filter(!is.na(species))
# species <- readRDS(paste0(path.igaD, "vybrane-druhy.rds"))
# species$species  <- species

write.table(
  data.frame(
    "species", "sentinel_bio",

    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B1_1km_cv",
    # "B6_1km_cv",
    # "B6_1km_mean",
    # "ndvi_1km_cv",
    # "ndvi_1km_mean",
    # "prechod_pole_les_delka_1km",


    # "l8_10.10_1000_B10",
    #    "l8_6.6_1000_B1",
    #   "l8_6.6_1000_B10",
    #    "l8_7.7_1000_B1",
    #   "l8_8.8_1000_B10",
    #    "l8_8.8_1000_B5",
    #  "l8_5.5_1000_NDWI",
    # "l8_8.8_1000_MNDWI",
    # "l8_9.9_1000_MNDWI",
    #    "wc_1000_bio02",
    #    "wc_1000_bio06",
    #    "wc_1000_bio15",

    #     "l8_10.10_1000_B10",
    #       "l8_4.4_1000_B10",
    #        "l8_5.5_1000_B1",
    #       "l8_5.5_1000_B10",
    #        "l8_5.5_1000_B5",
    #        "l8_6.6_1000_B1",
    #       "l8_6.6_1000_B10",
    #        "l8_7.7_1000_B1",
    #       "l8_7.7_1000_B10",
    #       "l8_8.8_1000_B1",
    #      "l8_8.8_1000_B10",
    #       "l8_8.8_1000_B5",
    #      "l8_9.9_1000_B10",
    #  "l8_10.10_1000_MNDWI",
    #     "l8_4.4_1000_NDVI",
    #    "l8_5.5_1000_MNDWI",
    #     "l8_6.6_1000_NDWI",
    #    "l8_9.9_1000_MNDWI",
    #       "wc_1000_bio02",
    #        "wc_1000_bio04",
    #        "wc_1000_bio06",
    #       "wc_1000_bio15",




    #     "l8_10.10_1000_B10",
    #       "l8_4.4_1000_B10",
    #       "l8_5.5_1000_B1",
    #     "l8_5.5_1000_B10",
    #        "l8_5.5_1000_B5",
    #       "l8_6.6_1000_B1",
    #       "l8_6.6_1000_B10",
    #        "l8_7.7_1000_B1",
    #       "l8_7.7_1000_B10",
    #      "l8_8.8_1000_B10",
    #       "l8_8.8_1000_B5",
    #      "l8_9.9_1000_B10",
    #  "l8_10.10_1000_MNDWI",
    #     "l8_4.4_1000_NDVI",
    #    "l8_5.5_1000_MNDWI",
    #     "l8_6.6_1000_NDWI",
    #    "l8_8.8_1000_MNDWI",
    #    "l8_9.9_1000_MNDWI",
    #        "wc_1000_bio02",
    #        "wc_1000_bio04",
    #        "wc_1000_bio13",
    #       "wc_1000_bio15",
    #     "srtm_1000_aspect",
    #     "srtm_1000_slope",


    #   "l8_10.10_1000_B10",
    #     "l8_5.5_1000_B10",
    #      "l8_6.6_1000_B1",
    #     "l8_6.6_1000_B10",
    #      "l8_7.7_1000_B1",
    #     "l8_8.8_1000_B10",
    #      "l8_8.8_1000_B5",
    #    "l8_4.4_1000_NDVI",
    #    "l8_5.5_1000_NDWI",
    #  "l8_8.8_1000_MNDWI",
    #  "l8_9.9_1000_MNDWI",
    #      "wc_1000_bio02",
    #      "wc_1000_bio13",
    #      "wc_1000_bio15",
    #   "srtm_1000_aspect",
    #    "srtm_1000_slope",


    # "B1_1km_cv",
    # "B5_1km_cv",
    # "B5_1km_mean",
    # "B6_1km_cv",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_1km_cv",
    # "ndvi_9_10_1km_mean",


    # "B5_1km_mean",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_mean",


    # "B1_1km_cv",
    # "B6_1km_cv",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_cv",

    # "B1_1km_cv",
    # "B5_1km_mean",
    # "B6_1km_cv",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_1km_cv",
    # "ndvi_9_10_1km_mean",



    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B1_1km_cv",
    # "B5_1km_mean",
    # "B6_1km_cv",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_cv",
    # "ndvi_9_10_1km_mean",


    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B5_1km_mean",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_mean",


    "wc_1000_bio02",
    "wc_1000_bio04",
    "wc_1000_bio06",
    "wc_1000_bio15",
    "B1_1km_cv",
    "B6_1km_cv",
    "linie_1km_sum",
    "ndvi_9_10_1km_cv",



    "1km_occupied_count",

    "Habitat",
    "Migration",
    "Protection",
    "Distribution"
  ),
  file = paste0(path.igaD, "result_AUCs_sentinel.cv.bio.csv"), sep = ",", append = TRUE, quote = TRUE,
  col.names = FALSE, row.names = FALSE
)




### Jen hnízdící druhy: http://fkcso.cz/fk/ptacicr.html
fkcso_csv <- read_csv(paste0(wd, "/species/ndop/ptaci-hnizdeni-do-2020-fkcso.csv"))
fkcso <- fkcso_csv %>%
  filter(cat == "A") %>%
  filter(grepl("h,|H,", type))



# obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků
"%notin%" <- Negate("%in%")

species.filtry <- species %>%
  filter(species %in% fkcso$species) %>%
  filter(species %notin% nepuvodni_problematicke()$nepuvodni) %>%
  filter(species %notin% nepuvodni_problematicke()$problematicke) %>%
  filter(species %notin% nepuvodni_problematicke()$nevhodne) %>%
  arrange(species)




### read bird traits table (traits according to Kolecek et al 2010)
traits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")

# nahrazení názvů traits druhů novějšími názvy z NDOPu
for (s in names(synonyms)) {
  matched <- traits %>% filter(species == synonyms[[s]])
  if (nrow(matched) == 1) {
    traits[traits$species == synonyms[[s]], "species"] <- s
  }
}


###
# join bird traits
species.filtry.joined_traits <- species.filtry %>%
  left_join(traits, by = c("species" = "species")) %>%
  filter(!is.na(Habitat))

# joined_traits_anti <- species.filtry %>%
#    anti_join(traits, by = c("species" = "species"))

species.filtry.joined_traits$species %<>% as.factor
species.filtry.joined_traits$Habitat %<>% as.factor
species.filtry.joined_traits$Migration %<>% as.factor
species.filtry.joined_traits$Distribution %<>% as.factor
species.filtry.joined_traits$Protection %<>% as.factor
# saveRDS(permImp.auc75.filtry.joined_traits$species, paste0(path.igaD,"vybrane-druhy.rds"))





for (sp in species.filtry.joined_traits$species) {
  species.name <- sp #  Ciconia nigra, Coturnix coturnix, Mergus merganser, Crex crex, Falco subbuteo, Jynx torquilla, Asio otus, "Vanellus vanellus"
  # res <- res %>% filter(species == "Ciconia nigra")

  res.species <- res %>% filter(species == species.name)

  res.sf <- st_as_sf(x = res.species, coords = c("X", "Y"), crs = rcrs)
  st_crs(res.sf) <- 3035


  ### udělat (rychlejším způsobem - svým dříve) per pixel z funkcí, pokud nepotřebuju absence
  grid.intersects <- st_intersects(sf.grid, res.sf)

  sf.grid.true <- lengths(grid.intersects) > 0 # Finds only countries where intersection is TRUE
  sf.grid.presence <- sf.grid[sf.grid.true, ]
  presence <- st_centroid(sf.grid.presence)
  if (nrow(presence) < 100) {
    next
  }


  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species.selected <- enmtools.species(env.sentinel_bio[[1]], as.data.frame(st_coordinates(presence)), NA, species.name)


  replicates <- 10

  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(
      species = species.selected, env = env.sentinel_bio, test.prop = 0.3, nback = 10000,
      # bias = bias_czechia,
      bg.source = "range",
      verbose = TRUE
    )
  }
  m.auc.sentinel_bio <- median(sapply(m, function(x) x$test.evaluation@auc))




  enm_mxt_gbif.vip <- sapply(m, enmtools.vip)
  enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
    as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
  })

  if (replicates == 1) {
    enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
  } else {
    b_g <- enm_mxt_gbif.vip.t[[1]]

    for (n in 1:replicates) {
      if (n > 1) {
        b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
      }
    }
    enm_mxt_gbif.vip.s <- b_g %>%
      summarise_if(is.numeric, mean, na.rm = TRUE)
  }



  write.table(
    data.frame(
      species.name, round(m.auc.sentinel_bio, digits = 3),

      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$prechod_pole_les_delka_1km.Importance, digits = 3),


      #  round(enm_mxt_gbif.vip.s$l8_10.10_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_6.6_1000_B1.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_6.6_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_7.7_1000_B1.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_8.8_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_8.8_1000_B5.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_5.5_1000_NDWI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_8.8_1000_MNDWI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_9.9_1000_MNDWI.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),

      #     round(enm_mxt_gbif.vip.s$l8_10.10_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_4.4_1000_B10.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$l8_5.5_1000_B1.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_5.5_1000_B10.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$l8_5.5_1000_B5.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$l8_6.6_1000_B1.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_6.6_1000_B10.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$l8_7.7_1000_B1.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_7.7_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_8.8_1000_B1.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_8.8_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_8.8_1000_B5.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_9.9_1000_B10.Importance, digits = 3),
      #  round(enm_mxt_gbif.vip.s$l8_10.10_1000_MNDWI.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_4.4_1000_NDVI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_5.5_1000_MNDWI.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_6.6_1000_NDWI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_9.9_1000_MNDWI.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),







      #     round(enm_mxt_gbif.vip.s$l8_10.10_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_4.4_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_5.5_1000_B1.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_5.5_1000_B10.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$l8_5.5_1000_B5.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_6.6_1000_B1.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_6.6_1000_B10.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$l8_7.7_1000_B1.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_7.7_1000_B10.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_8.8_1000_B10.Importance, digits = 3),
      #       round(enm_mxt_gbif.vip.s$l8_8.8_1000_B5.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_9.9_1000_B10.Importance, digits = 3),
      #  round(enm_mxt_gbif.vip.s$l8_10.10_1000_MNDWI.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_4.4_1000_NDVI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_5.5_1000_MNDWI.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_6.6_1000_NDWI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_8.8_1000_MNDWI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_9.9_1000_MNDWI.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio13.Importance, digits = 3),
      #        round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$srtm_1000_aspect.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$srtm_1000_slope.Importance, digits = 3),


      #   round(enm_mxt_gbif.vip.s$l8_10.10_1000_B10.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_5.5_1000_B10.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_6.6_1000_B1.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_6.6_1000_B10.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_7.7_1000_B1.Importance, digits = 3),
      #     round(enm_mxt_gbif.vip.s$l8_8.8_1000_B10.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$l8_8.8_1000_B5.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_4.4_1000_NDVI.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$l8_5.5_1000_NDWI.Importance, digits = 3),
      #  round(enm_mxt_gbif.vip.s$l8_8.8_1000_MNDWI.Importance, digits = 3),
      #  round(enm_mxt_gbif.vip.s$l8_9.9_1000_MNDWI.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$wc_1000_bio13.Importance, digits = 3),
      #      round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      #   round(enm_mxt_gbif.vip.s$srtm_1000_aspect.Importance, digits = 3),
      #    round(enm_mxt_gbif.vip.s$srtm_1000_slope.Importance, digits = 3),



      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),


      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),



      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_cv.Importance, digits = 3),

      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),

      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),


      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),

      round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_cv.Importance, digits = 3),

      nrow(presence),

      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Habitat),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Migration),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Protection),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Distribution)
    ),
    file = paste0(path.igaD, "result_AUCs_sentinel.cv.bio.csv"), sep = ",", append = TRUE, quote = TRUE,
    col.names = FALSE, row.names = FALSE
  )

  print(paste0(species.name, ",", round(m.auc.sentinel_bio, digits = 3), ",", nrow(presence)))
  print(Sys.time())
}

print("konec:")
print(Sys.time())


stop()



























################### cluster

library(cluster)
library(factoextra)
library(dendextend)

# permImp <- read_delim(paste0(path.igaD, "/result_AUCs_sentinel.all.csv"), delim=",")
permImp <- read_delim(paste0(path.igaD, "/lsdHod-result_AUCs_kfme16-glm-131-remove-lessPreds-nsim100.csv"), delim = ",")

permImp.auc75 <- permImp %>%
  filter(sentinel_bio >= 0.75) %>%
  arrange(species)
### vybrat jen druhy z předchozího csv bez biasu, aby to bylo srovnatelné!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# permImp.auc75.species <- permImp.auc75$species
# permImp.auc75 <- permImp %>% filter(species %in% permImp.auc75.species)

permImp.test <- drop_na(subset(permImp.auc75, select = -c(predictors_count, `1km_occupied_count`, `1km_unoccupied_count`, sentinel_bio, Migration, Distribution, Habitat, Protection)))
permImp.test.names <- permImp.test %>% column_to_rownames(var = "species")





permImp.test.names.dist <- dist(permImp.test.names, method = "euclidean")


# dissimilarity  <- 1 - permImp.test.names.dist
# permImp.test.names.dist <- distance  <- as.dist(dissimilarity)




# plot(hclust(as.dist(1 - permImp.test.names.dist) ), main="permImp.test.names.dist")

hc <- hclust(permImp.test.names.dist)
plot(hc, main = "permImp.test.names.dist")


# hclust do formátu dendrogramu, aby šel upravit
dend <- as.dendrogram(hc)
# nutné seřadit druhy (s vazbou na habitat) tak aby odpovídaly řazení v dendrogramu a podbarvily se podle habitatu
labels_colors(dend) <- as.numeric(as.factor(permImp.auc75$Habitat[order.dendrogram(dend)]))

plot(dend, main = "permImp.test.names.dist")



pdf(paste0(path.igaD, "lsdHod-result_AUCs_kfme16-glm-131-remove-lessPreds-nsim100.pdf"), width = 14, height = 6)
par(cex = 0.7, mar = c(10, 2, 2, 2))
plot(dend, main = "permImp.test.names.dist")
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






############################################################################
avif <- read_csv(paste0(path.igaD, "AVIF3.csv"), locale = locale("cs", decimal_mark = ","))

avif %<>% separate(Name1, sep = " ", into = c("gen", "spec"), remove = FALSE) %>%
  unite("species", c("gen", "spec"), sep = " ", remove = FALSE) %>%
  filter(!(is.na(Longitude))) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(3035)) %>%
  dplyr::select(Id, species, geometry)

avif %<>% type_convert(col_types = cols(
  Id = "i",
  ObsListId = "i",
  POLE = "f",
  SpeciesId = "f",
  NameCZ = "f",
  Name1 = "f",
  Name_original = "f",
  AtlasActivity = "f",
  ObsDate = col_date("%d.%m.%Y"),
  species = "f"
  # SiteName6 = "f"
))
avif$Id %<>% as.integer
avif$ObsListId %<>% as.integer
avif$POLE %<>% as.factor
avif$SpeciesId %<>% as.factor

saveRDS(avif, file = paste0(path.igaD, "avif-test.rds"))


############################################################################
avif <- read_csv(paste0(path.igaD, "AVIF3.csv"), locale = locale("cs", decimal_mark = ","))

avif %<>% separate(Name1, sep = " ", into = c("gen", "spec"), remove = FALSE) %>%
  unite("species", c("gen", "spec"), sep = " ", remove = FALSE) %>%
  filter(!(is.na(Longitude) & is.na(SiteName))) %>% # jen 4 nálezy, kde je NA v souřadnicích i SiteName
  # # mutate(SiteName6 = substr(SiteName, start = 1, stop = 6)) %>% # nee, beru jen čisté subkvadráty, neořezávat?!

  filter(is.na(Longitude) & !is.na(SiteName)) %>%
  filter(grepl("^[0-9]{4}[a-d]{2}$", SiteName)) # jen Hodinovky (mají subkvadrát místo názvu lokality)?


avif %<>% type_convert(col_types = cols(
  Id = "i",
  ObsListId = "i",
  POLE = "f",
  SpeciesId = "f",
  NameCZ = "f",
  Name1 = "f",
  Name_original = "f",
  AtlasActivity = "f",
  ObsDate = col_date("%d.%m.%Y"),
  species = "f"
  # SiteName6 = "f"
))

# znovu přetypování, proč výše uvedene nefachčí u všech sloupců?
avif$Id %<>% as.integer
avif$ObsListId %<>% as.integer
avif$POLE %<>% as.factor
avif$SpeciesId %<>% as.factor


avif.sum_per_subq <- avif %>%
  group_by(SiteName) %>%
  summarise(count = n_distinct(Id)) %>%
  arrange(desc(count))
# avif.sum_per_subq %>% arrange(count)

avif %>% arrange(ObsDate)
# jak napárovat SiteName aspoň na střed kvadrátu?

# res.sf <- st_as_sf(x = res.species, coords = c("Longitude", "Latitude"), crs = 4326)
# st_crs(res.sf) <- 3035
# avif %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# shp.sitmap_2rad <- shapefile(paste0(path.igaD, "sitmap_2rad/sitmap_2rad.shp"))
# shp.sitmap_2rad.sf <- st_read(shp.sitmap_2rad)
# shp.sitmap_2rad.centroid <- st_centroid(shp.sitmap_2rad)



# st.sitmap_1rad <- st_read(paste0(path.igaD, "sitmap_1rad/sitmap_1rad.shp"))
# st.sitmap_1rad.centroids <- st_centroid(st.sitmap_1rad)


st.sitmap_2rad <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad.shp"))
st.sitmap_2rad.centroids <- st_centroid(st.sitmap_2rad)





m.sitmap_2rad.v3 <- merge(st.sitmap_2rad, avif, by.x = "POLE", by.y = "SiteName6")



avif %>% filter(grepl("^[0-9]{4}[a-d]{2}$", SiteName6))


print(avif %>% mutate(SiteName6 = substr(SiteName, start = 1, stop = 6)) %>% dplyr::select(SiteName6) %>% drop_na(), n = 600)


m.sitmap_2rad <- sp::merge(shp.sitmap_2rad, avif, by.x = "POLE", by.y = "SiteName", duplicateGeoms = TRUE)
m.sitmap_2rad.v2 <- merge(shp.sitmap_2rad, avif, by.y = "POLE", by.x = "SiteName")

saveRDS(avif, file = paste0(path.igaD, "avif.rds"))


print(unique(avif %>% filter(is.na(Longitude)) %>% dplyr::select(SiteName6)), n = 300)
avif %>%
  filter(is.na(Longitude)) %>%
  dplyr::select(SiteName6)

unique(avif %>% dplyr::select(ObsListId))

avif %>% filter(ObsListId == 118212)




write.csv(avif, paste0(path.igaD, "avif_rds.csv"))

library(openxlsx)
write.xlsx(avif, paste0(path.igaD, "avif_rds.xlsx"))














############################################################################
############################################################################ 22222222222222222222222222222222222222
############################################################################
avif <- read_csv(paste0(path.igaD, "avif/exportavif--hodinovky-2018-2021.csv"), locale = locale("cs"))

unique(avif$ObsListsID)


avif.sum_per_subq <- avif %>%
  group_by(ObsListsID) %>%
  summarise(count = n_distinct(ObsItemsID)) %>%
  arrange(desc(count))
print(avif.sum_per_subq %>% arrange(count), n = 450)



avif$Id %<>% as.integer
avif$ObsListId %<>% as.integer
avif$POLE %<>% as.factor
avif$SpeciesId %<>% as.factor


avif %<>% separate(Name1, sep = " ", into = c("gen", "spec"), remove = FALSE) %>%
  unite("species", c("gen", "spec"), sep = " ", remove = FALSE) %>%
  filter(!(is.na(Longitude))) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(st_crs(3035)) %>%
  dplyr::select(Id, species, geometry)

avif %<>% type_convert(col_types = cols(
  Id = "i",
  ObsListId = "i",
  POLE = "f",
  SpeciesId = "f",
  NameCZ = "f",
  Name1 = "f",
  Name_original = "f",
  AtlasActivity = "f",
  ObsDate = col_date("%d.%m.%Y"),
  species = "f"
  # SiteName6 = "f"
))
avif$Id %<>% as.integer
avif$ObsListId %<>% as.integer
avif$POLE %<>% as.factor
avif$SpeciesId %<>% as.factor

# saveRDS(avif, file = paste0(path.igaD, "avif-test.rds"))







########################################################################################################################################################
# 3333333333333333333333333333333333333333333333333
########################################################################################################################################################
avif <- read_csv(
  # paste0(path.igaD, "avif/export-avif-lsd-2018-2022_utf8.csv"),
  paste0(path.igaD, "avif/exportavif--hodinovky-2018-2022_utf.csv"),
  col_types = cols(ObsItemsNote = "c", SiteNote = "c", ObsListsNote = "c", Mark = "c", ObsItemsNote = "c", PhotoID = "i")
)

# unique(avif$ObsListsID)
# unique(avif$TaxonNameLAT)

avif %<>% filter(between(month(Date), 3, 6)) %>%
  filter(!str_detect(SiteName, "TRANS")) %>% # Vyloučit TRANS v SiteName, je to něco jinačího než LSD?
  separate(TaxonNameLAT, sep = " ", into = c("gen", "spec"), remove = FALSE) %>%
  unite("species", c("gen", "spec"), sep = " ", remove = FALSE) %>%
  filter(!(is.na(Lon))) %>%
  filter(!(is.na(Lat))) %>%
  mutate(Lon = round(Lon, 4)) %>%
  mutate(Lat = round(Lat, 4)) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform(st_crs(3035)) %>%
  mutate(WKT = st_as_text(geometry)) %>%
  mutate(SiteName6 = substr(SiteName, 1, 6))
# %>% dplyr::select(Id, species, geometry)
avif %<>% as_tibble(avif) %>% dplyr::select(-geometry)


st.sitmap_2rad <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad.shp"))
st.sitmap_2rad.centroids <- st_centroid(st.sitmap_2rad)

avif.m <- merge(st.sitmap_2rad.centroids, avif, by.x = "POLE", by.y = "SiteName6")
# Zapsat si počty presencí i absencí!


avif.m$species %<>% as.factor
# avif.m$SiteName6 %<>% as.factor
avif.m$POLE %<>% as.factor
avif.m$ObsListsID %<>% as.factor

avif.m %<>% st_transform(st_crs(3035))

#  saveRDS(avif.m, file = paste0(path.igaD, "export-avif-lsd-2018-2022_utf8_3-6.rds"))
# saveRDS(avif.m, file = paste0(path.igaD, "exportavif--hodinovky-2018-2022_utf_3-6.rds"))


# avif %>% filter(SiteName = "5461ad-lsd-p1-zelvator-v1" | SiteName = "5461ad-lsd-p2-zelvator-v1" |  SiteName = "7168ab-lsd-p2-ales-v1" )
# print(avif %>% filter(!duplicated(paste0(substr(SiteName, 1, 6),WKT))) %>% dplyr::select(SiteName, WKT) %>% arrange(SiteName), n=200)


# unique(avif$geometry) # 187
# unique(avif$SiteName) # 363
# sort(unique(avif$SiteName))
# length(sort(unique(substr(avif$SiteName, 1, 6)))) # 183

# str(st_coordinates(avif))


# avif.c <- st_coordinates(avif)

# avif.c.s <- round(avif.c[,1]/100)+round(avif.c[,2]/100)
# unique(avif.c.s)


# st_set_precision(avif$geometry)
# # unique(avif$species)

# # write.csv(avif, paste0(path.igaD, "avif/export-avif-lsd-2018-2021_3-6.csv"))
# # write.csv(avif %>% filter(SiteName == "5461ad-lsd-p1-zelvator-v1" | SiteName == "5461ad-lsd-p2-zelvator-v1" |  SiteName =="7168ab-lsd-p2-ales-v1" ), paste0(path.igaD, "avif/export-avif-lsd-2018-2021_3-6-delete.csv"))


# avif.sum_per_subq <- avif %>% group_by(ObsListsID) %>%
#   summarise(count = n_distinct(ObsItemsID)) %>%
#   arrange(desc(count))
# print(avif.sum_per_subq %>% arrange(count), n=450)


# avif.sum_per_subq <- avif %>% group_by(SiteName) %>%
#   summarise(count = n_distinct(ObsItemsID)) %>%
#   arrange(desc(count))

# avif.sum_per_subq <- avif %>% group_by(SiteName) %>%
#   summarise(count = n_distinct(TaxonNameLAT)) %>%
#   arrange(desc(count))

# #  21 5461ad-lsd-p1-zelvator-v1          POINT (4739083 3067338)
# #  22 5461ad-lsd-p1-zelvator-v1          POINT (4739163 3066340)
# #  23 5461ad-lsd-p1-zelvator-v1          POINT (4739128 3066784)
# #  24 5461ad-lsd-p2-zelvator-v1          POINT (4738272 3066826)

# # 184 7168ab-lsd-p2-ales-v1              POINT (4838146 2888675)
# # 185 7168ab-lsd-p2-ales-v1              POINT (4838396 2889147)







#####
# SDM
#####

# env.sentinel <- stack(paste0(path.igaD, "igaD-1km-czechia-all-sentinel.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "igaD-1km-czechia-sentinel-cv.bio-0.7-.grd"))



# raster_stack_mask_czechia <- stack(paste0(path.igaD, "czechia-710-2000.grd"))
# vif.all <-  vif(raster_stack_mask_czechia, maxobservations=100000)
# print(vif.all)
# print("vifcor")
# vc <- vifcor(raster_stack_mask_czechia, th = 0.5, maxobservations=100000)

# # corrplot(vc@corMatrix, order = "hclust", main = "hclust")
# print(vc)

# rs_names <- names(raster_stack_mask_czechia)
# rs_indexes <- seq_along(names(raster_stack_mask_czechia))
# rs_ex <- match(vc@excluded, rs_names)

# r <- dropLayer(raster_stack_mask_czechia, rs_ex) # L8
# rr <- writeRaster(r, paste0(path.igaD, "czechia-710-2000-vifcor05.grd"), format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")

# env.sentinel_bio <- stack(paste0(path.igaD, "lsd-3km-czechia-sentinelbio-all.vif.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "lsd-3km1-czechia-sentinelbio-all.vif.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor05-vidstep2.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor03-vidstep15.grd"))


# # vyberu X nejlepších prediktorů
# predictors.top10 <- c("kfme16.l8_30_4_ndvi_cv.nd",
# "kfme16.l8_30_5_mndwi_cv.nd",
# "kfme16.l8_30_6_mndwi_cv.nd",
# "kfme16.l8_30_4_mndwi_cv.nd",
# "kfme16.l8_30_4_mndwi_mean.nd",
# "kfme16.l8_30_5_raw_mean.B5",
# "kfme16.wc_30_6_mean.bio02",
# "kfme16.l8_30_5_raw_mean.B10")
# rs_names <- names(env.sentinel_bio)
# rs_ex <- match(predictors.top10, rs_names)
# env.sentinel_bio <- dropLayer(env.sentinel_bio, setdiff(1:18, rs_ex))





# max correlation ( l8_9.11_2000_B5 ~ l8_3.5_2000_B10 ):  0.4093484
# ---------- VIFs of the remained variables --------
#           Variables      VIF
# 1   l8_3.5_2000_B10 1.757003
# 2   l8_9.11_2000_B5 1.263683
# 3 l8_3.5_2000_MNDWI 1.179927
# 4  l8_3.5_2000_NDWI 1.343045
# 5     wc_2000_bio02 1.361589
# 6     wc_2000_bio13 1.348182
# 7     wc_2000_bio15 1.325292



# bias_czechia <- raster(paste0(path.igaD, "bias-ptaci-adj-0.8-1km.tif"))

# env.sentinel <- dropLayer(env.sentinel, c(6)) # c(1, 31, 32)
# env.sentinel_bio <- dropLayer(env.sentinel_bio, c(9)) # c(1, 31, 32)




# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
# sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
# st_crs(sf.grid) <- 3035

# sjednocení s hodinovkama****************************************************************************
res <- readRDS(paste0(path.igaD, "export-avif-lsd-2018-2022_utf8_3-6.rds"))
res.hodinovky <- readRDS(paste0(path.igaD, "exportavif--hodinovky-2018-2022_utf_3-6.rds"))

res$species %<>% as.character
res.hodinovky$species %<>% as.character

# sjednocení synonym
synonyms <- synonyms()

for (s in names(synonyms)) {
  matched <- res %>% filter(species == synonyms[[s]])
  if (nrow(matched) > 0) {
    res[res$species == synonyms[[s]], "species"] <- s
  }
}

for (s in names(synonyms)) {
  matched <- res.hodinovky %>% filter(species == synonyms[[s]])
  if (nrow(matched) > 0) {
    res.hodinovky[res.hodinovky$species == synonyms[[s]], "species"] <- s
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


# st_write(
#   # res %>% dplyr::distinct(geometry),
#   res %>% filter(POLE %in% ObsListsID_per_sq$POLE),
#  paste0(path.igaD, "delete-distinct-geom-lds-hodinovky.shp"))



# #####################################################################
# # doplnění nálezů z NDOPu do použitých kvadrátů s cílem omezení false absence
# sf.grid <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_3035.shp"))

# sf.grid %<>% filter(POLE %in% ObsListsID_per_sq$POLE)

# res.ndop <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021_3-6.rds"))

# res.ndop %<>% filter(precision <= 1000)  %>%
# st_as_sf(coords = c("X", "Y"), crs = 4326)

# grid.intersection <- st_intersection(sf.grid, res.ndop)

# for (s in names(synonyms)) {
#   matched <- grid.intersection %>% filter(species == synonyms[[s]])
#   if (nrow(matched) > 0) {
#     grid.intersection[grid.intersection$species == synonyms[[s]], "species"] <- s
#   }
# }

# res %<>% dplyr::select(POLE, species, ObsListsID)
# grid.intersection %<>% dplyr::select(POLE, species, ObsListsID)

# res  %<>% add_row(grid.intersection)





# počet POLE na species
species <- res %>%
  filter(POLE %in% ObsListsID_per_sq$POLE) %>%
  group_by(species) %>%
  summarise(count = n_distinct(POLE)) %>%
  arrange(desc(count)) %>%
  filter(count >= 30) %>%
  filter(count < 110) %>% # celkem je (183) (113) 141 subkvadrátů, potřebuju nějaké absence na ověření...
  filter(!is.na(species))


# st_write(species, paste0(path.igaD, "delete-test-LSD-110_2018-2022.shp"))
# st_write(species, paste0(path.igaD, "delete-test-hodinovkyLSD-131_2018-2022.shp"))

species$species %<>% as.character

# # počet ObsItemsID (nálezů) na POLE
# # jsou tu i pole s méně než 30 nálezy!!! !!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! !!!!!!!!!!!!!!!!!!!!!!!!!!!!
# species <- res %>%
#   group_by(POLE) %>%
#   summarise(count = n_distinct(ObsItemsID)) %>%
#   arrange(desc(count)) %>%
#   filter(count >= 30) %>%
#   filter(count < 17000) %>%
#   filter(!is.na(POLE))




# st_write(sf.grid, paste0(path.igaD, "delete-subq.shp"))
# st_write(res.ndop, paste0(path.igaD, "delete-ndop.shp"))
# st_write(grid.intersection, paste0(path.igaD, "delete-intersection-subq-ndop.shp"))

# results_name <- "lsdHod-result_AUCs_kfme16-glm-131-remove-lessPreds-nsim100"
results_name <- "lsdHod-result_AUCs_kfme16-glm-131-gam03"


write.table(
  data.frame(
    "species", "sentinel_bio",
    # "predictors_used",

    # "B1_1km_cv",
    # "B5_1km_mean",
    # "B6_1km_cv",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_1km_cv",
    # "ndvi_9_10_1km_mean",



    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B1_1km_cv",
    # "B5_1km_mean",
    # "B6_1km_cv",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_cv",
    # "ndvi_9_10_1km_mean",


    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B5_1km_mean",
    # "B6_1km_mean",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_mean",


    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B1_1km_cv",
    # "B6_1km_cv",
    # "linie_1km_sum",
    # "ndvi_9_10_1km_cv",


    # "l8_3.5_2000_B10",
    # "l8_9.11_2000_B5",
    # "l8_3.5_2000_MNDWI",
    # "l8_3.5_2000_NDWI",
    # "wc_2000_bio02",
    # "wc_2000_bio13",
    # "wc_2000_bio15",


    # "wc_1000_bio02",
    # "wc_1000_bio04",
    # "wc_1000_bio06",
    # "wc_1000_bio15",
    # "B1_1km_cv_sitmap.delete1",
    # "B6_1km_cv_sitmap.delete1",
    # "B6_1km_mean_sitmap.delete1",
    # "ndvi_1km_cv_sitmap.delete1",
    # "sitmap_linie.delete1",



    # "kfme16.l8_30_4_raw_cv.B10",
    # "kfme16.l8_30_4_raw_cv.B5",
    # "kfme16.l8_30_5_raw_cv.B10",
    # "kfme16.l8_30_5_raw_cv.B7",
    # "kfme16.l8_30_5_raw_mean.B10",
    # "kfme16.l8_30_5_raw_mean.B5",
    # "kfme16.l8_30_6_raw_cv.B1",
    # "kfme16.l8_30_6_raw_cv.B10",
    # "kfme16.l8_30_4_mndwi_cv.nd",
    # "kfme16.l8_30_4_mndwi_mean.nd",
    # "kfme16.l8_30_4_ndvi_cv.nd",
    # "kfme16.l8_30_4_ndvi_mean.nd",
    # "kfme16.l8_30_5_mndwi_cv.nd",
    # "kfme16.l8_30_6_mndwi_cv.nd",
    # "kfme16.wc_30_6_cv.bio06",
    # "kfme16.wc_30_6_cv.bio11",
    # "kfme16.wc_30_6_mean.bio02",
    # "kfme16.wc_30_6_mean.bio15",

    "kfme16.l8_30_4_raw_cv.B5",
    "kfme16.l8_30_5_raw_mean.B10",
    "kfme16.l8_30_6_raw_cv.B1",
    "kfme16.l8_30_4_mndwi_cv.nd",
    "kfme16.l8_30_4_ndvi_cv.nd",
    "kfme16.l8_30_5_mndwi_cv.nd",
    "kfme16.l8_30_6_mndwi_cv.nd",
    "kfme16.wc_30_6_cv.bio06",
    "kfme16.wc_30_6_cv.bio11",
    "kfme16.wc_30_6_mean.bio15",


    # "kfme16.l8_30_4_ndvi_cv.nd",
    # "kfme16.l8_30_5_mndwi_cv.nd",
    # "kfme16.l8_30_6_mndwi_cv.nd",
    # "kfme16.l8_30_4_mndwi_cv.nd",
    # "kfme16.l8_30_4_mndwi_mean.nd",
    # "kfme16.l8_30_5_raw_mean.B5",
    # "kfme16.wc_30_6_mean.bio02",
    # "kfme16.l8_30_5_raw_mean.B10",


    "1km_occupied_count",
    "1km_unoccupied_count",
    "Habitat",
    "Migration",
    "Protection",
    "Distribution"
  ),
  file = paste0(path.igaD, results_name, ".csv"), sep = ",", append = TRUE, quote = TRUE,
  col.names = FALSE, row.names = FALSE
)




### Jen hnízdící druhy: http://fkcso.cz/fk/ptacicr.html
fkcso_csv <- read_csv(paste0(wd, "/species/ndop/ptaci-hnizdeni-do-2020-fkcso.csv"))
fkcso <- fkcso_csv %>%
  filter(cat == "A") %>%
  filter(grepl("h,|H,", type))

### read bird traits table (traits according to Kolecek et al 2010)
traits <- read_delim(paste0(wd, "/R/export_raster/Rp/birds_traits_K.csv"), delim = ";")


# obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků
"%notin%" <- Negate("%in%")







for (s in names(synonyms)) {
  matched <- fkcso %>% filter(species == synonyms[[s]])
  if (nrow(matched) > 0) {
    fkcso[fkcso$species == synonyms[[s]], "species"] <- s
  }
}

# nahrazení názvů traits druhů novějšími názvy z NDOPu
for (s in names(synonyms)) {
  matched <- traits %>% filter(species == synonyms[[s]])
  if (nrow(matched) > 0) {
    traits[traits$species == synonyms[[s]], "species"] <- s
  }
}

fkcso %>% anti_join(species, by = c("species" = "species"))

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

species.filtry.joined_traits$species %<>% as.factor
species.filtry.joined_traits$Habitat %<>% as.factor
species.filtry.joined_traits$Migration %<>% as.factor
species.filtry.joined_traits$Distribution %<>% as.factor
species.filtry.joined_traits$Protection %<>% as.factor
# saveRDS(permImp.auc75.filtry.joined_traits$species, paste0(path.igaD,"vybrane-druhy.rds"))


data <- list()
data.species <- list()
permImp_remove_last <- function(species.selected, env.sentinel_bio, replicates, data = NA) {
  if (!is.list(data)) {
    data <- list()
  }
  n_layers <- nlayers(env.sentinel_bio)
  print(paste0("Počet použitých prediktorů/layerů:", n_layers))
  print(names(env.sentinel_bio))
  if (n_layers > 1) {
    n_layers <- as.character(n_layers)
    m <- list()
    for (r in 1:replicates) {
      m[[r]] <- ENMToolsPB::enmtools.glm(
        species = species.selected, env = env.sentinel_bio,
        test.prop = "checkerboard2",
        # nback = 10000,
        # bias = bias_czechia,
        bg.source = "points",
        corner = r,
        verbose = TRUE
      )
    }
    data[[n_layers]]$auc <- median(sapply(m, function(x) x$test.evaluation@auc))



    cm <- lapply(m, function(x) performance(x$conf))
    cm.matrix <- abind(cm, along = 3)
    cm.perf <- apply(cm.matrix, c(1, 2), median)
    cm.perf.t <- as_tibble(cm.perf)

    data[[n_layers]] <- append(data[[n_layers]], as.list(cm.perf.t))


    enm_mxt_gbif.vip <- sapply(m, enmtools.vip, nsim = 100)
    enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
      as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
    })

    if (replicates == 1) {
      enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
    } else {
      b_g <- enm_mxt_gbif.vip.t[[1]]

      for (n in 1:replicates) {
        if (n > 1) {
          b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
        }
      }
      enm_mxt_gbif.vip.s <- b_g %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)
    }

    data[[n_layers]]$permImp <- enm_mxt_gbif.vip.s


    # odstraním nejhorší prediktor
    predictors.all <- as_tibble(cbind(nms = names(enm_mxt_gbif.vip.s), t(enm_mxt_gbif.vip.s))) %>%
      mutate(across(V2, as.numeric)) %>%
      arrange(V2)

    predictor.bad <- predictors.all %>% slice_min(V2, n = 1)
    rs_names <- names(env.sentinel_bio)

    rs_ex <- match(substr(predictor.bad$nms, 1, nchar(predictor.bad$nms) - 11), rs_names) # odstraním příponu .Importance
    env.sentinel_bio.minus1 <- dropLayer(env.sentinel_bio, rs_ex)

    data[[n_layers]]$bad.name <- predictor.bad$nms
    data[[n_layers]]$bad.value <- predictor.bad$V2
    if (n_layers == 2) {
      print("end")
      return(data)
    } else {
      print("continue")
      return(permImp_remove_last(species.selected, env.sentinel_bio.minus1, replicates, data))
    }
  } else {
    return(NA)
  }
}


for (sp in species.filtry.joined_traits$species) {
  species.name <- sp #  Ciconia nigra, Coturnix coturnix, Mergus merganser, Crex crex, Falco subbuteo, Jynx torquilla, Asio otus, "Vanellus vanellus"

  # # presence druhu
  #   res.species <- res %>% filter(species == species.name)
  # # absence druhu (kvadráty kde nejsou presence)
  # res.species.a <- res %>% filter(POLE %notin% res.species$POLE)


  subqs <- res %>% filter(POLE %in% ObsListsID_per_sq$POLE)

  # presence druhu
  res.species <- subqs %>% filter(species == species.name)
  # absence druhu (kvadráty kde nejsou presence)
  res.species.a <- subqs %>%
    filter(POLE %notin% res.species$POLE) %>%
    filter(species != species.name)

  # ### udělat (rychlejším způsobem - svým dříve) per pixel z funkcí, pokud nepotřebuju absence
  # grid.intersects <- st_intersects(sf.grid, res.species)

  # sf.grid.true <- lengths(grid.intersects) > 0 # Finds only countries where intersection is TRUE
  # sf.grid.presence <- sf.grid[sf.grid.true, ]
  # presence <- st_centroid(sf.grid.presence)


  presence <- as_tibble(res.species) %>%
    dplyr::distinct(POLE, .keep_all = TRUE) %>%
    dplyr::distinct(geometry)
  # if (nrow(presence) < 30) {
  #   next
  # }
  if (nrow(presence) < 30) {
    print("******************************************************************************************************************************************")
    print(species.name)
    print(nrow(presence))
    next
  }
  # grid.intersects.a <- st_intersects(sf.grid, res.species.a)
  #   sf.grid.false <- lengths(grid.intersects.a) > 0 # Finds only countries where intersection is TRUE
  #   sf.grid.absence <- sf.grid[sf.grid.false, ]
  #   absence <- st_centroid(sf.grid.absence)

  absence <- as_tibble(res.species.a) %>%
    dplyr::distinct(POLE, .keep_all = TRUE) %>%
    dplyr::distinct(geometry)
  # if (nrow(absence) < 100) {
  #   next
  # }


  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species.selected <- ENMToolsPB::enmtools.species(env.sentinel_bio[[1]], as.data.frame(st_coordinates(presence$geometry)), as.data.frame(st_coordinates(absence$geometry)), species.name)

  replicates <- 4

  # ######
  # data.species[[species.name]] <- data <- permImp_remove_last(species.selected, env.sentinel_bio, replicates)
  # data.maxAUC <- rev(round(sapply(data, function(x) x$auc),2)) # záměrně zaokrouhlím na 2 des. místa a pak dám opačné pořadí, aby byl dále vybrán model s nejmenším počtem prediktorů, pokud AUC osciluje kolem jedné hodnoty
  # data.maxAUC.value <- as.numeric(data.maxAUC[which.max(data.maxAUC)])
  # data.maxAUC.index <- names(data.maxAUC[which.max(data.maxAUC)])
  # best.model <- data[[data.maxAUC.index]]


  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- ENMToolsPB::enmtools.gam(
      species = species.selected, env = env.sentinel_bio,
      test.prop = "block",
      # nback = 10000,
      # bias = bias_czechia,
      bg.source = "points",
      corner = r,
      verbose = TRUE
    )
  }
  m.auc.sentinel_bio <- median(sapply(m, function(x) x$test.evaluation@auc))




  enm_mxt_gbif.vip <- sapply(m, enmtools.vip)
  enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
    as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
  })

  if (replicates == 1) {
    enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
  } else {
    b_g <- enm_mxt_gbif.vip.t[[1]]

    for (n in 1:replicates) {
      if (n > 1) {
        b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
      }
    }
    enm_mxt_gbif.vip.s <- b_g %>%
      summarise_if(is.numeric, mean, na.rm = TRUE)
  }




  # # vyberu 10 nejlepších prediktorů
  #       predictors.all <- as_tibble(cbind(nms = names(enm_mxt_gbif.vip.s), t(enm_mxt_gbif.vip.s))) %>%
  #                 mutate(across(V2, as.numeric)) %>%
  #                 arrange(desc(V2))
  # predictors.top10 <- predictors.all %>% slice_max(V2, n = 10)
  # rs_names <- names(env.sentinel_bio)
  # rs_indexes <- seq_along(names(env.sentinel_bio))
  # predictors.top10.rev <- as.vector(predictors.all %>% filter(nms %notin% predictors.top10$nms) %>% dplyr::select(nms))
  # rs_ex <- match(substr(predictors.top10.rev$nms,1,nchar(predictors.top10.rev$nms)-11), rs_names)
  # raster_stack_vifstep <- dropLayer(env.sentinel_bio, rs_ex)

  # print(names(raster_stack_vifstep))

  #   m <- list()
  #   for (r in 1:replicates) {
  #     m[[r]] <- ENMToolsPB::enmtools.glm(species = species.selected, env = raster_stack_vifstep,
  #     test.prop = "checkerboard2",
  #     # nback = 10000,
  #     # bias = bias_czechia,
  #     bg.source = "points",
  #     corner = r,
  #     verbose = TRUE)
  #   }
  #   m.auc.sentinel_bio.v2 <- median(sapply(m, function(x) x$test.evaluation@auc))


  #   enm_mxt_gbif.vip <- sapply(m, enmtools.vip)
  #   enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
  #     as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
  #   })

  #   if (replicates == 1) {
  #     enm_mxt_gbif.vip.s.v2 <- enm_mxt_gbif.vip.t[[1]]
  #   } else {
  #     b_g <- enm_mxt_gbif.vip.t[[1]]

  #     for (n in 1:replicates) {
  #       if (n > 1) {
  #         b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
  #       }
  #     }
  #     enm_mxt_gbif.vip.s.v2 <- b_g %>%
  #       summarise_if(is.numeric, mean, na.rm = TRUE)
  #   }



  # # vyberu 5 nejlepších prediktorů
  #       predictors.all <- as_tibble(cbind(nms = names(enm_mxt_gbif.vip.s.v2), t(enm_mxt_gbif.vip.s.v2))) %>%
  #                 mutate(across(V2, as.numeric)) %>%
  #                 arrange(desc(V2))
  # predictors.top10 <- predictors.all %>% slice_max(V2, n = 5)
  # rs_names <- names(raster_stack_vifstep)
  # rs_indexes <- seq_along(names(raster_stack_vifstep))
  # predictors.top10.rev <- as.vector(predictors.all %>% filter(nms %notin% predictors.top10$nms) %>% dplyr::select(nms))
  # rs_ex <- match(substr(predictors.top10.rev$nms,1,nchar(predictors.top10.rev$nms)-11), rs_names)
  # raster_stack_vifstep <- dropLayer(raster_stack_vifstep, rs_ex)

  # print(names(raster_stack_vifstep))

  #   m <- list()
  #   for (r in 1:replicates) {
  #     m[[r]] <- ENMToolsPB::enmtools.glm(species = species.selected, env = raster_stack_vifstep,
  #     test.prop = "checkerboard2",
  #     # nback = 10000,
  #     # bias = bias_czechia,
  #     bg.source = "points",
  #     corner = r,
  #     verbose = TRUE)
  #   }
  #   m.auc.sentinel_bio.v3 <- median(sapply(m, function(x) x$test.evaluation@auc))






  write.table(
    data.frame(
      species.name,
      round(m.auc.sentinel_bio, digits = 3),
      # round(data.maxAUC.value, digits = 3), data.maxAUC.index,

      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),

      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),


      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B5_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_mean.Importance, digits = 3),

      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$linie_1km_sum.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_9_10_1km_cv.Importance, digits = 3),


      # round(enm_mxt_gbif.vip.s$l8_3.5_2000_B10.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$l8_9.11_2000_B5.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$l8_3.5_2000_MNDWI.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$l8_3.5_2000_NDWI.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_2000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_2000_bio13.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_2000_bio15.Importance, digits = 3),

      # round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio04.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B1_1km_cv_sitmap.delete1.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_cv_sitmap.delete1.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$B6_1km_mean_sitmap.delete1.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$ndvi_1km_cv_sitmap.delete1.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$sitmap_linie.delete1.Importance, digits = 3),



      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_raw_cv.B10.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_raw_cv.B5.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_cv.B10.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_cv.B7.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_mean.B10.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_mean.B5.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_6_raw_cv.B1.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_6_raw_cv.B10.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_mndwi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_mndwi_mean.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_ndvi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_ndvi_mean.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_mndwi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_6_mndwi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_cv.bio06.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_cv.bio11.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_mean.bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_mean.bio15.Importance, digits = 3),


      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_4_raw_cv.B10.Importance),round(best.model$permImp$kfme16.l8_30_4_raw_cv.B10.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_4_raw_cv.B5.Importance),round(best.model$permImp$kfme16.l8_30_4_raw_cv.B5.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_5_raw_cv.B10.Importance),round(best.model$permImp$kfme16.l8_30_5_raw_cv.B10.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_5_raw_cv.B7.Importance),round(best.model$permImp$kfme16.l8_30_5_raw_cv.B7.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_5_raw_mean.B10.Importance),round(best.model$permImp$kfme16.l8_30_5_raw_mean.B10.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_5_raw_mean.B5.Importance),round(best.model$permImp$kfme16.l8_30_5_raw_mean.B5.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_6_raw_cv.B1.Importance),round(best.model$permImp$kfme16.l8_30_6_raw_cv.B1.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_6_raw_cv.B10.Importance),round(best.model$permImp$kfme16.l8_30_6_raw_cv.B10.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_4_mndwi_cv.nd.Importance),round(best.model$permImp$kfme16.l8_30_4_mndwi_cv.nd.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_4_mndwi_mean.nd.Importance),round(best.model$permImp$kfme16.l8_30_4_mndwi_mean.nd.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_4_ndvi_cv.nd.Importance),round(best.model$permImp$kfme16.l8_30_4_ndvi_cv.nd.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_4_ndvi_mean.nd.Importance),round(best.model$permImp$kfme16.l8_30_4_ndvi_mean.nd.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_5_mndwi_cv.nd.Importance),round(best.model$permImp$kfme16.l8_30_5_mndwi_cv.nd.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.l8_30_6_mndwi_cv.nd.Importance),round(best.model$permImp$kfme16.l8_30_6_mndwi_cv.nd.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.wc_30_6_cv.bio06.Importance),round(best.model$permImp$kfme16.wc_30_6_cv.bio06.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.wc_30_6_cv.bio11.Importance),round(best.model$permImp$kfme16.wc_30_6_cv.bio11.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.wc_30_6_mean.bio02.Importance),round(best.model$permImp$kfme16.wc_30_6_mean.bio02.Importance, digits = 3),0)),
      # suppressWarnings(ifelse(!is.null(best.model$permImp$kfme16.wc_30_6_mean.bio15.Importance),round(best.model$permImp$kfme16.wc_30_6_mean.bio15.Importance, digits = 3),0)),

      round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_raw_cv.B5.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_mean.B10.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.l8_30_6_raw_cv.B1.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_mndwi_cv.nd.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_ndvi_cv.nd.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_mndwi_cv.nd.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.l8_30_6_mndwi_cv.nd.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_cv.bio06.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_cv.bio11.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_mean.bio15.Importance, digits = 3),


      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_ndvi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_mndwi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_6_mndwi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_mndwi_cv.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_4_mndwi_mean.nd.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_mean.B5.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.wc_30_6_mean.bio02.Importance, digits = 3),
      # round(enm_mxt_gbif.vip.s$kfme16.l8_30_5_raw_mean.B10.Importance, digits = 3),


      nrow(presence),
      nrow(absence),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Habitat),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Migration),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Protection),
      species.filtry.joined_traits %>% filter(species == species.name) %>% dplyr::select(Distribution)
    ),
    file = paste0(path.igaD, results_name, ".csv"), sep = ",", append = TRUE, quote = TRUE,
    col.names = FALSE, row.names = FALSE
  )

  print(paste0(
    species.name, ",",
    # round(data.maxAUC.value, digits = 3),
    round(m.auc.sentinel_bio, digits = 3),
    ",", nrow(presence)
  ))
  print(Sys.time())
}
saveRDS(data.species, file = paste0(path.igaD, results_name, ".rds"))
# plot(sapply(data.species$`Luscinia megarhynchos`, function(x) x$auc))

print("konec:")
print(Sys.time())


stop()



# write.csv(presence %>% mutate(WKT = st_as_text(geometry)), paste0(path.igaD, "delete-cejka-presence.csv"))
# write.csv(absence %>% mutate(WKT = st_as_text(geometry)), paste0(path.igaD, "delete-cejka-absence.csv"))
# write.csv(res %>% filter(species == "Vanellus vanellus"), paste0(path.igaD, "delete-cejka-all.csv"))



for (k in 1:10) {
  # print(combn(letters[1:5], k, simplify=FALSE))

  k.list <- combn(letters[1:10], k, simplify = FALSE)
  # str(k.list)

  for (k2 in 1:length(k.list)) {
    print(k.list[[k2]])
  }
  # print(k.list[[k]])
}