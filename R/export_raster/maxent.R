## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

# fitrace NDOP záznamů
# source(paste0(wd, "/R/export_raster/ndop.R"))
# print(ndop(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../ndop/csv")))

# fitrace GBIF záznamů
# source(paste0(wd, "/R/export_raster/gbif.R"))
# print(gbif(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../gbif/csv")))
# print(gbif(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv"))


# res_ndop <- ndop(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../ndop/csv"))
# res_gbif <- gbif(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv")


# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# csvgee <- read.csv(paste0(getwd(), "/gee_datasets/gee-pouzite-datasety.csv"))
# csvgee$"short"[[2]]
# csvgee[1,]


wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)
export_path <-
  paste0(getwd(), "/../export/schuzka2-total-gbif-ndop3/")

# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
species <-
  c(
    "Parus major",
    "Fringilla coelebs",
    "Ardea cinerea",
    "Buteo buteo",
    "Emberiza citrinella",
    "Phylloscopus collybita",
    "Cygnus olor",
    "Cyanistes caeruleus",
    "Ciconia ciconia",
    "Columba palumbus",
    "Dendrocopos major",
    "Sylvia atricapilla",
    "Hirundo rustica",
    "Fulica atra",
    "Sturnus vulgaris",
    "Chroicocephalus ridibundus",
    "Erithacus rubecula",
    "Turdus philomelos",
    "Podiceps cristatus",
    "Motacilla alba",
    "Aythya fuligula",
    "Falco tinnunculus",
    "Lanius collurio",
    "Garrulus glandarius",
    "Circus aeruginosus",
    "Phalacrocorax carbo",
    "Alcedo atthis",
    "Aythya ferina",
    "Phoenicurus ochruros",
    "Alauda arvensis",
    "Anser anser",
    "Pica pica",
    "Apus apus",
    "Corvus corax",
    "Chloris chloris",
    "Picus viridis",
    "Carduelis carduelis",
    "Passer montanus",
    "Turdus pilaris",
    "Periparus ater",
    "Ardea alba",
    "Dryocopus martius",
    "Streptopelia decaocto",
    "Delichon urbicum",
    "Passer domesticus",
    "Tachybaptus ruficollis",
    "Turdus viscivorus",
    "Phylloscopus trochilus",
    "Crex crex",
    "Mergus merganser",
    "Coccothraustes coccothraustes",
    "Cuculus canorus",
    "Ciconia nigra",
    "Phasianus colchicus",
    "Regulus regulus",
    "Accipiter nisus",
    "Haliaeetus albicilla",
    "Prunella modularis",
    "Anas crecca",
    "Gallinula chloropus",
    "Spinus spinus",
    "Motacilla cinerea",
    "Pyrrhula pyrrhula",
    "Charadrius dubius",
    "Emberiza schoeniclus",
    "Corvus corone",
    "Phoenicurus phoenicurus",
    "Regulus ignicapilla",
    "Ficedula albicollis",
    "Serinus serinus",
    "Columba oenas",
    "Corvus cornix",
    "Luscinia megarhynchos",
    "Bucephala clangula",
    "Aegithalos caudatus",
    "Saxicola rubetra",
    "Anthus trivialis",
    "Strix aluco",
    "Certhia familiaris",
    "Sylvia curruca",
    "Gallinago gallinago",
    "Acrocephalus arundinaceus",
    "Oriolus oriolus",
    "Lanius excubitor",
    "Poecile palustris",
    "Coloeus monedula",
    "Milvus milvus",
    "Jynx torquilla",
    "Sylvia borin",
    "Actitis hypoleucos",
    "Loxia curvirostra",
    "Streptopelia turtur",
    "Tringa ochropus",
    "Larus cachinnans",
    "Coturnix coturnix",
    "Acrocephalus palustris",
    "Netta rufina",
    "Emberiza calandra",
    "Sterna hirundo",
    "Lophophanes cristatus"
  ) # "Loxia curvirostra", "Charadrius dubius", "Cinclus cinclus", "Locustella luscinioides" "Parus major" "Turdus merula"


species_u <- gsub(" ", "_", species)
px_size <- c(2000) # 100, 200, 1000, 2000, 5000, 10000
reps <- 10

if (1 == 1) {
  me <- " pictures=FALSE outputGrids=FALSE plots=FALSE "
  
} else{
  me <- " responsecurves jackknife writeplotdata"
  
}


# dataframe <- data.frame()
for (px_size_item in px_size) {
  for (species_u_item in species_u) {
    for (i in 1:reps) {
      dir_key <- paste0(px_size_item, "_", species_u_item)
      
      # ponechat změnu thinnování, nebo dát hodnotu napevno?
      # px_size_item_change <- px_size_item
      px_size_item_change <- 100
      
      dir_key_ndop <-
        paste0(px_size_item_change, "_ndop_", species_u_item, "_", i)
      dir_key_gbif <-
        paste0(px_size_item_change, "_gbif_", species_u_item, "_", i)
      dir_key_all <-
        paste0(px_size_item_change, "_all_", species_u_item, "_", i)
      
      
      dir_key_1_p <-
        paste0(export_path, "maxent/", dir_key, "_1/", i, "/")
      dir_key_2_p <-
        paste0(export_path, "maxent/", dir_key, "_2/", i, "/")
      
      #
      # 1) vygenerování modelů
      #
      
      dir.create(dir_key_1_p,
                 showWarnings = FALSE,
                 recursive = TRUE)
      dir.create(dir_key_2_p,
                 showWarnings = FALSE,
                 recursive = TRUE)
      system(
        paste0(
          "java -mx2000m -jar ~/maxent/test_project/maxent/maxent.jar nowarnings noprefixes  outputdirectory=",
          dir_key_1_p,
          " samplesfile=",
          export_path,
          "species/",
          dir_key_gbif,
          ".csv environmentallayers=/home/petr/Downloads/rgee2/export/raster/schuzka_rastery_vif/",
          px_size_item,
          " testsamplesfile=",
          export_path,
          "species/",
          dir_key_ndop,
          ".csv ",
          " randomtestpoints=0 replicates=0 writebackgroundpredictions responsecurvesexponent appendtoresultsfile threads=4  autorun visible=FALSE removeDuplicates=FALSE ",
          me
        )
      )
      system(
        paste0(
          "java -mx2000m -jar ~/maxent/test_project/maxent/maxent.jar nowarnings noprefixes  outputdirectory=",
          dir_key_2_p,
          " samplesfile=",
          export_path,
          "species/",
          dir_key_all,
          ".csv environmentallayers=/home/petr/Downloads/rgee2/export/raster/schuzka_rastery_vif/",
          px_size_item,
          "  randomtestpoints=0 replicates=0 writebackgroundpredictions responsecurvesexponent appendtoresultsfile threads=4  autorun visible=FALSE removeDuplicates=FALSE ",
          me
        )
      )
      
      
      #
      # 2) načtení získaných statistik do CSV
      #
      
      # csvgee_1 <- read_csv(paste0(dir_key_1_p, "maxentResults.csv"))
      # csvgee_2 <- read_csv(paste0(dir_key_2_p, "maxentResults.csv"))
      # csvgee_1 %<>% select(1:11)
      # csvgee_2 %<>% select(2:7)
      # csvgee_1$px_size_item <- px_size_item
      # csvgee_1$repetition <- i
      # write_csv(bind_cols(csvgee_1[1,], csvgee_2[1,]), paste0(export_path, "results_100_200_1000_2000-100top.csv"), append = TRUE)
      
      
      ########################################
      # csvgee_1$type <- 1
      #csvgee_1$species <- species_u_item
      
      
      # data.frame(type = 1, csvgee_1)
      # data.frame(px_size_item = px_size_item, csvgee_1)
      # data.frame(species = species_u_item, csvgee_1)
      
      # add_column(csvgee_1, type = 1, .before = 1)
      # add_column(csvgee_1, px_size_item = px_size_item, .before = 1)
      # add_column(csvgee_1, species = species_u_item, .before = 1)
      
      # csvgee_2$type <- 2
      # csvgee_2$px_size_item <- px_size_item
      # csvgee_2$species <- species_u_item
      # data.frame(type = 1, csvgee_2)
      # data.frame(px_size_item = px_size_item, csvgee_2)
      # data.frame(species = species_u_item, csvgee_2)
      
      
      # add_column(csvgee_2, type = 1, .before = 1)
      # add_column(csvgee_2, px_size_item = px_size_item, .before = 1)
      # add_column(csvgee_2, species = species_u_item, .before = 1)
      
      # csvgee_1$"Training.AUC"
      # csvgee_1$"Test.AUC"
      # csvgee_2$"Training.AUC"
      
      
      # write_csv(csvgee_1[1,], paste0(export_path, "mv_1sT4.csv"), append = TRUE)
      # write_csv(csvgee_2[1,], paste0(export_path, "mv_2sT4.csv"), append = TRUE)
      ########################################
    }
  }
}
