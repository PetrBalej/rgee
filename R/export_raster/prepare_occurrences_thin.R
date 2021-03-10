# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)
export_path <-
  paste0(getwd(), "/../export/schuzka2-total-gbif-ndop/")

source(paste0(getwd(), "/R/export_raster/functions.R"))
source(paste0(getwd(), "/R/export_raster/gbif.R"))
source(paste0(getwd(), "/R/export_raster/ndop_divland.R"))
source(paste0(getwd(), "/R/export_raster/prepare_occurrences.R"))

# všechny z NDOP
#species <- c("Parus major","Fringilla coelebs","Ardea cinerea","Buteo buteo","Emberiza citrinella","Phylloscopus collybita","Cygnus olor","Cyanistes caeruleus","Ciconia ciconia","Columba palumbus","Dendrocopos major","Sylvia atricapilla","Hirundo rustica","Fulica atra","Sturnus vulgaris","Chroicocephalus ridibundus","Erithacus rubecula","Turdus philomelos","Podiceps cristatus","Motacilla alba","Aythya fuligula","Falco tinnunculus","Lanius collurio","Garrulus glandarius","Circus aeruginosus","Phalacrocorax carbo","Alcedo atthis","Aythya ferina","Phoenicurus ochruros","Alauda arvensis","Anser anser","Pica pica","Mareca strepera","Apus apus","Corvus corax","Chloris chloris","Picus viridis","Carduelis carduelis","Passer montanus","Turdus pilaris","Periparus ater","Ardea alba","Dryocopus martius","Streptopelia decaocto","Delichon urbicum","Passer domesticus","Tachybaptus ruficollis","Turdus viscivorus","Phylloscopus trochilus","Crex crex","Mergus merganser","Coccothraustes coccothraustes","Cuculus canorus","Ciconia nigra","Phasianus colchicus","Regulus regulus","Accipiter nisus","Haliaeetus albicilla","Prunella modularis","Anas crecca","Gallinula chloropus","Spinus spinus","Motacilla cinerea","Pyrrhula pyrrhula","Charadrius dubius","Emberiza schoeniclus","Corvus corone","Phoenicurus phoenicurus","Regulus ignicapilla","Ficedula albicollis","Serinus serinus","Columba oenas","Corvus cornix","Luscinia megarhynchos","Bucephala clangula","Aegithalos caudatus","Saxicola rubetra","Anthus trivialis","Strix aluco","Certhia familiaris","Sylvia curruca","Gallinago gallinago","Acrocephalus arundinaceus","Oriolus oriolus","Lanius excubitor","Poecile palustris","Coloeus monedula","Milvus milvus","Columba livia f. domestica","Jynx torquilla","Muscicapa striata","Sylvia borin","Actitis hypoleucos","Loxia curvirostra","Streptopelia turtur","Spatula clypeata","Tringa ochropus","Larus cachinnans","Coturnix coturnix","Acrocephalus palustris","Netta rufina","Phylloscopus sibilatrix","Emberiza calandra","Sterna hirundo","Lophophanes cristatus") # "Loxia curvirostra", "Charadrius dubius", "Cinclus cinclus", "Locustella luscinioides" "Parus major" "Turdus merula"

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
px_size <- c(1000) # 100, 200, 1000, 2000, 10000


# předem si načíst .csv nálezů do proměnných a předávat rovnou je!
set_cols1 <-
  cols(
    gbifID = "c",
    coordinateUncertaintyInMeters = "d",
    coordinatePrecision = "d",
    day = "i",
    month = "i",
    year = "i"
  )
ptaci_gbif <-
  read_tsv(
    paste0(
      getwd(),
      "/../new-species/gbif/0209125-200613084148143-redukce4.csv"
    ),
    col_types = set_cols1
  )

set_cols2 <-
  cols(
    ID_ND_NALEZ = "c",
    DRUH = "c",
    AUTOR = "c",
    DATUM_OD = col_date("%Y-%m-%d"),
    DATUM_DO = col_date("%Y-%m-%d"),
    CXLOKAL_TYP = "c",
    NEGATIVNI = "i",
    VEROH = "i",
    PRESNOST = "i"
  )
ptaci_ndop <-
  read_csv(
    paste0(
      getwd(),
      "/../new-species/ndop/ptaci_ndop_reduction/ptaci_ndop_utf8-redukce.csv"
    ),
    col_types = set_cols2
  )

for (px_size_item in px_size) {
  for (species_item in species) {
    gc()
    
    res_ndop <-
      ndop_divland(
        list(from = '2016-01-01', to = '2020-12-31'),
        list(from = 4, to = 6),
        paste0(getwd(), "/../new-species/ndop/ptaci_ndop_reduction"),
        NULL,
        px_size_item,
        ptaci_ndop
      )
    # res_ndop <- ndop(list(from = '2016-01-01', to = '2020-12-31'), list(from = 4, to = 6), paste0(getwd(), "/../ndop/csv"), NULL, px_size)
    # print(as_tibble(res_ndop), n = 10)
    gc()
    
    res_gbif <-
      gbif(
        list(from = '2016-01-01', to = '2020-12-31'),
        list(from = 4, to = 6),
        paste0(getwd(), "/../new-species/gbif"),
        "0209125-200613084148143-redukce4.csv",
        NULL,
        px_size_item,
        ptaci_gbif
      )
    # res_gbif <- gbif(list(from = '2016-01-01', to = '2020-12-31'), list(from = 4, to = 6), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv", NULL, px_size)
    # print(as_tibble(res_gbif), n = 10)
    
    gc()
    
    occ_prepared <-
      prepare_occurrences(
        species_item,
        (px_size_item / 1000),
        paste0(export_path, "species/"),
        res_ndop,
        res_gbif,
        3035
      )
    
  }
}
