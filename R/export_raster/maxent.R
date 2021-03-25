## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"

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


source(paste0(getwd(), "/R/export_raster/functions.R"))
source(paste0(getwd(), "/R/export_raster/ndop_top.R"))


export_path <-
  "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/schuzka2-total-gbif-ndop8/"


ndop_top <- ndop_top(paste0(getwd(), "/species/ndop/ndop-top-2021-03-21.xlsx"))
# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
# species <- pull(ndop_top %>% select(species1, species2))
species <- ndop_top %>% select(species1_, species2_)




# species_u <- gsub(" ", "_", species)
px_size <- c(2000) # 100, 200, 1000, 2000, 5000, 10000
reps <- 1



environmentallayers <- function(wd, px_size) {
  # /export/raster/schuzka_rastery_vif/
  return(paste0(wd, "/../export/raster2/", px_size, "/l8"))
}

samplesfile <- function(export_path, dir_key_) {
  return(paste0(export_path, "species/", dir_key_, ".csv"))
}



if (1 == 1) {
  me <- " pictures=FALSE outputGrids=FALSE plots=FALSE perSpeciesResults randomseed "
} else {
  me <- " responsecurves jackknife writeplotdata"
}


# dataframe <- data.frame()
for (px_size_item in px_size) {
  for (i in 1:reps) {
    px_size_item_change <- 9999
    dir_key <- paste0(px_size_item, "_")

    # ponechat změnu thinnování, nebo dát hodnotu napevno?
    # px_size_item_change <- px_size_item


    dir_key_ndop <-
      paste0(px_size_item_change, "_ndop_", "_", 1)
    dir_key_gbif <-
      paste0(px_size_item_change, "_gbif_", "_", 1)
    dir_key_all <-
      paste0(px_size_item_change, "_all_", "_", 1)


    dir_key_1_p <-
      paste0(export_path, "maxent/", dir_key, "_1/", i, "/")
    dir_key_2_p <-
      paste0(export_path, "maxent/", dir_key, "_2/", i, "/")

    #
    # 1) vygenerování modelů
    #

    dir.create(dir_key_1_p,
      showWarnings = FALSE,
      recursive = TRUE
    )
    dir.create(dir_key_2_p,
      showWarnings = FALSE,
      recursive = TRUE
    )

    system(
      paste0(
        "java -Xms10g -Xmx11g -jar /mnt/2AA56BAE3BB1EC2E/maxent/test_project/maxent344/maxent.jar nowarnings noprefixes  outputdirectory=",
        dir_key_1_p,
        " samplesfile=",
        samplesfile(export_path, dir_key_gbif),
        " environmentallayers=",
        environmentallayers(wd, px_size_item),
        " testsamplesfile=",
        samplesfile(export_path, dir_key_ndop),
        " randomtestpoints=20 replicates=0 writebackgroundpredictions responsecurvesexponent appendtoresultsfile threads=2  autorun visible=FALSE removeDuplicates=TRUE ",
        me
      )
    )
    system(
      paste0(
        "java -Xms10g -Xmx11g -jar /mnt/2AA56BAE3BB1EC2E/maxent/test_project/maxent344/maxent.jar nowarnings noprefixes  outputdirectory=",
        dir_key_2_p,
        " samplesfile=",
        samplesfile(export_path, dir_key_all),
        " environmentallayers=",
        environmentallayers(wd, px_size_item),
        "  randomtestpoints=20 replicates=0 writebackgroundpredictions responsecurvesexponent appendtoresultsfile threads=2  autorun visible=FALSE removeDuplicates=TRUE ",
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
    # csvgee_1$species <- species_u_item


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

# source("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/export_raster/maxent.R", encoding = "UTF-8")
