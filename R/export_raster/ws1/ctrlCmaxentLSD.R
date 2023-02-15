tempdir_delete <- tempdir()
# Sys.setenv(TMP = "D:\\PERSONAL_DATA\\tempR")
# Sys.setenv(TEMP = "D:\\PERSONAL_DATA\\tempR")
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "ENMToolsPB", "abind")
install.packages(setdiff(required_packages, rownames(installed.packages())))
# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)
# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "D:/PERSONAL_DATA/pb/rgee"
# wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)
source(paste0(getwd(), "/R/export_raster/functions.R"))
path.igaD <- "D:/PERSONAL_DATA/pb/igaD/"
# path.igaD <- "/home/petr/Documents/igaD/"



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#################################################################################################################################################
# NDOP data evaluována lsdHod nad kfme16 - použití fitování adjust bias rasteru + fitování prediktorů
#################################################################################################################################################

results_name <- "00001mGlm3-"
# kolik druhů má být v jedné skupině
speciesPerGroup <- 6
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- 18

data <- list()
data.species <- list()
data.species.best.model <- list()


# obrácení pořadí druhů, od nejméně početných pro urychlení prvních výsledků
"%notin%" <- Negate("%in%")

sf.grid <- st_read(paste0(path.igaD, "sitmap_2rad/sitmap_2rad_4326.shp"))
#  env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor05-vidstep2.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor03-vidstep15.grd"))
# env.sentinel_bio <- stack(paste0(path.igaD, "kfme16-vifcor05-vidstep2.grd"))
# env.sentinel_bio <- dropLayer(env.sentinel_bio, which(names(env.sentinel_bio) == "kfme16.l8_30_5_mndwi_cv.nd") )
# env.sentinel_bio <- dropLayer(env.sentinel_bio, which(names(env.sentinel_bio) == c("kfme16.l8_30_6_mndwi_cv.nd", "kfme16.wc_30_6_cv.bio06")) )
# env.sentinel_bio <- env.sentinel_bio[[1:3]]
# env.sentinel_bio <- env.sentinel_bio[[c(3,5)]]

raster_stack <- rasters_dir_stack(paste0(path.igaD, "kfme16_prediktory/"), "tif")

raster_stack <- dropLayer(raster_stack, grep("stdev|bio", names(raster_stack)))

names(raster_stack) <- str_replace_all(names(raster_stack), c("kfme16." = "", "_30" = "", "\\.nd" = ""))

env.sentinel_bio <- raster_stack




combs <- readRDS(paste0(path.igaD, "vif.vifcor_excludedPairs2-GLM-selected.rds"))





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




r0 <- env.sentinel_bio[[1]]
r0[!is.na(r0)] <- 0

r1 <- r0
r1[r1 == 0] <- 1




for (sp in species.parts[[species.part]]) { # species.filtry.joined_traits$species # (species.filtry.joined_traits %>% filter(species == "Certhia familiaris" | species == "Ardea cinerea"))$species
  # (species.filtry.joined_traits %>% filter(species == "Periparus ater"))$species
  species.name <- sp #  Ciconia nigra, Coturnix coturnix, Mergus merganser, Crex crex, Falco subbuteo, Jynx torquilla, Asio otus, "Vanellus vanellus" Certhia familiaris Ardea cinerea
  gc()
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



  r.p <- extract(r1, res.species.presence, sp = TRUE)
  r.a <- extract(r0, res.species.absence, sp = TRUE)
  r.pa <- rbind(r.p, r.a)



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
  k_classes <- 3




  ###### výběr se všemi kombinacemi prediktorů
  data.species[[species.name]] <- data <- permImp_comb(species.selected, species.selected.ndop, env.sentinel_bio, path.igaD, replicates, k_classes, combs, r.pa)


  # raději ukládám i průběžné výsledky jednotlivých druhů zvlášť
  saveRDS(data, file = paste0(path.igaD, results_name, "-", species.name, ".rds"))
  print(Sys.time())
  unlink(paste0(tempdir_delete, "/*"), recursive = TRUE, force = TRUE)
}
saveRDS(data.species, file = paste0(path.igaD, results_name, ".rds"))

print("konec:")
print(Sys.time())
unlink(paste0(tempdir_delete, "/*"), recursive = TRUE, force = TRUE)
species.part