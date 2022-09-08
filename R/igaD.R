# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "ENMTools")
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
shp.grid <- shapefile(paste0(path.igaD, "EEA_site/EEA_1km.shp"))

excel.files <- list.files(paste0(path.igaD, "1km_prediktory2"), full.names = TRUE, pattern = ".xls")


rcrs <- "+init=epsg:3035 +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

print("start:")
print(Sys.time())


# ### raster z excelů s indexy
# for (excel.file.item in excel.files){
# print("***********************************************************")
# excel.file <- read_excel(excel.file.item)
# print(str(excel.file))

# filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(excel.file.item))
# print(filename)

# # merge gridu shapefile a exceli podle ID
# # m <- merge(shp.grid, excel.file, by='ID')
# m <- merge(shp.grid, excel.file, by='FID_EEA_gr')

# # print(str(m))
# print("jmena sloupcu mergnute")
# print(names(m@data))


# # náhoda, takto to cca vychází?! Ale to nejsou KFME gridy?
# ext <- extent(4470000, 4970000, 2830000, 3120000)
# gridsize <- 1000
# r <- raster(ext, res=gridsize)


# ## field - výběr názvu sloupce s indexy obsahující slovo sentinel
# rr <- rasterize(m, r, field=m@data[which(grepl("sentinel|variation|mean", names(m@data)))])

# crs(rr) <- rcrs

# # raster::crs(r) <- "EPSG:3035"
# # crs(r) <- CRS('+init=EPSG:3035')

# print(paste0(path.igaD, filename, ".tif"))
# writeRaster(rr, paste0(path.igaD, filename, ".tif"), format = "GTiff", overwrite = TRUE)

# }


# # jen doplnění nul (0) místo NA
# rr <- raster(paste0(path.igaD, "prechod_pole_les_delka_1km.tif"))
# rr[is.na(rr[])] <- 0
# writeRaster(rr, paste0(path.igaD, "prechod_pole_les_delka_1km.tif"), format = "GTiff", overwrite = TRUE)



# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
st_crs(sf.grid) <- 3035


# source(paste0(getwd(), "/R/export_raster/ndop_ugc_WS2.R"))
# res <- ndop_ugc(list(from = "2018-01-01", to = "2021-12-31"), list(from = 1, to = 12), "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/zal", 3035, 1000)
# res <- res %>% filter(cat == "Ptáci")
# # res %>% filter(species == "Ciconia nigra")
# saveRDS(res, file = paste0(path.igaD, "ptaci_ndop_2018-2021.rds"))
res <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021.rds"))

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


enm_mxt_gbif.vip.s$wc_1000_bio02.Importance




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































env.sentinel <- stack(paste0(path.igaD, "igaD-1km-czechia-all-sentinel.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "igaD-1km-czechia-all-sentinel_bio.grd"))

env.sentinel <- dropLayer(env.sentinel, c(6)) # c(1, 31, 32)
env.sentinel_bio <- dropLayer(env.sentinel_bio, c(9)) # c(1, 31, 32)




# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
st_crs(sf.grid) <- 3035


res <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021.rds"))

species <- res %>%
  group_by(species) %>%
  summarise(count = n_distinct(key)) %>%
  arrange(desc(count)) %>%
  filter(count >= 100) %>%
  filter(count < 10000) %>%
  filter(!is.na(species))


write.table(
  data.frame("species", "sentinel", "sentinel_bio", "1km_occupied_count"),
  file = paste0(path.igaD, "result_AUCs_sentinel.csv"), sep = ",", append = TRUE, quote = TRUE,
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
    file = paste0(path.igaD, "result_AUCs_sentinel.csv"), sep = ",", append = TRUE, quote = TRUE,
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




env.sentinel <- stack(paste0(path.igaD, "igaD-1km-czechia-all-sentinel.grd"))
env.sentinel_bio <- stack(paste0(path.igaD, "igaD-1km-czechia-all-sentinel_bio.grd"))

# env.sentinel <- dropLayer(env.sentinel, c(6)) # c(1, 31, 32)
# env.sentinel_bio <- dropLayer(env.sentinel_bio, c(9)) # c(1, 31, 32)




# sf.grid <-  st_read(paste0(path.igaD, "EEA_site/EEA_10km-50.shp"))
sf.grid <- st_read(paste0(path.igaD, "EEA_site/EEA_1km.shp"))
st_crs(sf.grid) <- 3035


res <- readRDS(paste0(path.igaD, "ptaci_ndop_2018-2021.rds"))

species <- res %>%
  group_by(species) %>%
  summarise(count = n_distinct(key)) %>%
  arrange(desc(count)) %>%
  filter(count >= 100) %>%
  filter(count < 10000) %>%
  filter(!is.na(species))


write.table(
  data.frame(
    "species", "sentinel_bio",

    "wc_1000_bio02",
    "wc_1000_bio06",
    "wc_1000_bio15",
    "B1_1km_cv",
    "B1_1km_mean",
    "B6_1km_cv",
    "B6_1km_mean",
    "ndvi_1km_cv",
    "prechod_pole_les_delka_1km",



    "1km_occupied_count"
  ),
  file = paste0(path.igaD, "result_AUCs_sentinel.csv"), sep = ",", append = TRUE, quote = TRUE,
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


  # při 1km nemůžu používat absence z neobsazených kvadrátů!!!
  species <- enmtools.species(env.sentinel[[1]], as.data.frame(st_coordinates(presence)), NA, species.name)


  replicates <- 10

  m <- list()
  for (r in 1:replicates) {
    m[[r]] <- enmtools.glm(species = species, env = env.sentinel_bio, test.prop = 0.3, nback = 10000)
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

      round(enm_mxt_gbif.vip.s$wc_1000_bio02.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$wc_1000_bio06.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$wc_1000_bio15.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$B1_1km_cv.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$B1_1km_mean.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$B6_1km_cv.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$B6_1km_mean.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$ndvi_1km_cv.Importance, digits = 3),
      round(enm_mxt_gbif.vip.s$prechod_pole_les_delka_1km.Importance, digits = 3),

      nrow(presence)
    ),
    file = paste0(path.igaD, "result_AUCs_sentinel.csv"), sep = ",", append = TRUE, quote = TRUE,
    col.names = FALSE, row.names = FALSE
  )

  print(paste0(species.name, ",", round(m.auc.sentinel_bio, digits = 3), ",", nrow(presence)))
  print(Sys.time())
}

print("konec:")
print(Sys.time())

stop()