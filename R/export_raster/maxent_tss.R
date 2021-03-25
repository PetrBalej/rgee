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



required_packages <-
  c("tidyverse", "lubridate", "magrittr", "dplyr")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)
source(paste0(getwd(), "/R/export_raster/functions.R"))
source(paste0(getwd(), "/R/export_raster/ndop_top.R"))

export_path <-
  "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/schuzka2-total-gbif-ndop5/"

ndop_top <- ndop_top(paste0(getwd(), "/species/ndop/ndop-top-2021-03-21.xlsx"))
# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
# species <- pull(ndop_top %>% select(species1, species2))
species <- ndop_top %>%
    select(species1_, species2_, species1, species2, species1_short, species2_short) %>%
    filter(species1_ != "Muscicapa_striata") %>%
    filter(species1_ != "Bombycilla_garrulus") %>%
    filter(species1_ != "Hydrocoloeus_minutus")

px_size <- c(100, 200, 1000, 2000) # 100, 200, 1000, 2000, 5000, 10000
reps <- 5

write_csv(
    data.frame(
        pixel_size = "pixel_size",
        species1_ = "species1_",
        species2_ = "species2_",
        species1 = "species1",
        species2 = "species2",
        species1_short = "species1_short",
        species2_short = "species2_short",
        trainGBIF = "trainGBIF",
        trainGBIF_sd = "trainGBIF_sd",
        testNDOP = "testNDOP",
        testNDOP_sd = "testNDOP_sd",
        trainALL = "trainALL",
        trainALL_sd = "trainALL_sd",
        Q1 = "Q1",
        Q2 = "Q2"
    ),
    paste0(export_path, "topX-final-TSS.csv"),
    append = TRUE
)
# dataframe <- data.frame()
for (px_size_item in px_size) {
    # for (i in 1:reps) {
    for (sindex in 1:nrow(species)) {
        tss <- maxtss2test(paste0(export_path, "maxent/", px_size_item, "__1/"), species[sindex, 1])
        tss2 <- maxtss2train(paste0(export_path, "maxent/", px_size_item, "__1/"), species[sindex, 1])
        tss3 <- maxtss2train(paste0(export_path, "maxent/", px_size_item, "__2/"), species[sindex, 1])


        if (species[sindex, 6] == "nana") {
            s2 <- NA
        }

        write_csv(
            data.frame(
                pixel_size = px_size_item,
                species1_ = species[sindex, 1],
                species2_ = species[sindex, 2],
                species1 = species[sindex, 3],
                species2 = species[sindex, 4],
                species1_short = species[sindex, 5],
                species2_short = s2,
                trainGBIF = tss2$max_tss_mean,
                trainGBIF_sd = tss2$max_tss_sd,
                testNDOP = tss$max_tss_mean,
                testNDOP_sd = tss$max_tss_sd,
                trainALL = tss3$max_tss_mean,
                trainALL_sd = tss3$max_tss_sd,
                Q1 = tss2$max_tss_mean - tss$max_tss_mean,
                Q2 = tss2$max_tss_mean - tss3$max_tss_mean
            ),
            paste0(export_path, "topX-final-TSS.csv"),
            append = TRUE
        )
    }
    # }
}