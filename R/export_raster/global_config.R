## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

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