# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)
export_path <-
  paste0(getwd(), "/../tmp/")

source(paste0(getwd(), "/R/export_raster/functions.R"))
source(paste0(getwd(), "/R/export_raster/gbif.R"))
source(paste0(getwd(), "/R/export_raster/ndop_divland.R"))
source(paste0(getwd(), "/R/export_raster/prepare_occurrences.R"))
source(paste0(getwd(), "/R/export_raster/ndop_top.R"))


ndop_top <- ndop_top(paste0(getwd(), "/species/ndop/ndop-top-2021-03-21.xlsx"))
# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
# species <- pull(ndop_top %>% select(species1, species2))
species <- ndop_top %>% select(species1, species2)

# species_u <- gsub(" ", "_", species)
px_size <- c(10000) # 100, 200, 1000, 2000, 10000


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
  res_ndop <-
    ndop_divland(
      list(from = "2010-01-01", to = "2020-12-31"),
      list(from = 1, to = 12),
      paste0(getwd(), "/../new-species/ndop/ptaci_ndop_reduction"),
      NULL,
      px_size_item,
      ptaci_ndop
    )
  # # res_ndop <- ndop(list(from = '2016-01-01', to = '2020-12-31'), list(from = 4, to = 6), paste0(getwd(), "/../ndop/csv"), NULL, px_size)
  # # print(as_tibble(res_ndop), n = 10)
  # gc()

  res_gbif <-
    gbif(
      list(from = "2010-01-01", to = "2020-12-31"),
      list(from = 1, to = 12),
      paste0(getwd(), "/../new-species/gbif"),
      "0209125-200613084148143-redukce4.csv",
      NULL,
      px_size_item,
      ptaci_gbif
    )
  # # res_gbif <- gbif(list(from = '2016-01-01', to = '2020-12-31'), list(from = 4, to = 6), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv", NULL, px_size)
  # # print(as_tibble(res_gbif), n = 10)


  write_csv(res_ndop, paste0(export_path, "ndop_", px_size_item, ".csv"))
  write_csv(res_gbif, paste0(export_path, "gbif_", px_size_item, ".csv"))
}



# ptaci_ndop_distinct <- ptaci_ndop %>% group_by(DRUH) %>% summarise(count = n_distinct(ID_ND_NALEZ)) %>% arrange(desc(count))

# ptaci_ndop_distinct %>% filter(count > 100)

# print(ptaci_ndop_distinct, n=200)













# ptaci_ndop_na <- ptaci_ndop %>% filter(PRESNOST < 100) %>% filter(CXLOKAL_TYP == "B")  %>% select(PRESNOST)

# write_csv(
#   x,
#   file,
#   na = "NA",
#   append = FALSE,
#   col_names = !append,
#   quote_escape = "double",
#   eol = "\n",
#   path = deprecated()
# )

# plot_ndop_gbif_csv <- read_csv("/mnt/6C2B3F36770071FA/puv-20gb/petr/bordel-ke-smazani/phd/iga-new (2)/ndop-gbif-pocet-zaznamu-dle-presnosti.csv")

# ggplot(plot_ndop_gbif_csv, aes(presicion)) +                    # basic graphical object
#   geom_line(aes(y=y1), colour="red") +  # first layer
#   geom_line(aes(y=y2), colour="green")  # second layer



#   # dummy data
# set.seed(45)
# df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45),
#                    variable=rep(paste0("category", 1:9), each=5))
# # plot
# ggplot(data = plot_ndop_gbif_csv, aes(x=precision, y=count)) + geom_line(aes(colour=db))

# plot_ndop_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/ndop_10000.csv")
# plot_gbif_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/gbif_10000.csv")
# gplot(data = plot_ndop_gbif_csv, aes(x=precision, y=count)) + geom_line(aes(colour=db))




# boxplot(list("ndop" = ptaci_ndop$PRESNOST, "gbif" = ptaci_gbif$coordinatePrecision))

# gplot(data = plot_ndop_gbif_csv, aes(x=precision, y=count)) + geom_line(aes(colour=db))


boxplot(list("ndop" = ptaci_ndop$PRESNOST, "gbif" = ptaci_gbif$coordinateUncertaintyInMeters), ylim = c(0, 10000))
plot_ndop_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/ndop_10000.csv")
plot_gbif_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/gbif_10000.csv")


