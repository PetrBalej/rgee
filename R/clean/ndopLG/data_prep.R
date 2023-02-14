# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

export_path <- "/home/petr/Downloads/ndopLG/"

dir.v <- "v0"


ndop.occ.min <- 30
ndop.date.min <- "2018-01-01"
ndop.date.max <- "2020-12-31"


export_path.v <- paste0("/home/petr/Downloads/ndopLG/", dir.v, "_", ndop.occ.min, "_", ndop.date.min, "_", ndop.date.max, "/")
dir.create(export_path.v, showWarnings = FALSE)



source(paste0(getwd(), "/R/export_raster/functions.R"))

ndop.ugc <- read_csv("/home/petr/Downloads/ndopLG/ndop-5514.csv")

ndop.ugc$cat %<>% as.factor
ndop.ugc$species %<>% as.factor


# summary(ndop.ugc)

# print("Tax. skupiny:")
# paste(unique(ndop.ugc$cat), collapse = ", ")

ndop.fd <- ndop.ugc %>%
    filter(date_from >= "2018-01-01" | date_do >= "2018-01-01") %>%
    filter(precision <= 1000)

# ndop.fd %>% split(.$cat) %>% map(summary)


ndop.fd.min.species.stat <- ndop.fd %>%
    group_by(species) %>%
    summarise(
        count = n_distinct(key),
        cat = first(cat),
        presicion_mean = round(mean(precision), digits=1)
    ) %>%
    arrange(desc(count)) %>%
    filter(count >= 30) %>%
    filter(!is.na(species))


# ndop.fd.min.species.stat %>%
#     arrange(presicion_mean)


print(ndop.fd.min.species.stat)
write.csv(ndop.fd.min.species.stat, paste0(export_path.v, "pocet-nalezu-druhy.csv"))
saveRDS(ndop.fd.min.species.stat, paste0(export_path.v, "pocet-nalezu-druhy.RDS"))






ndop.fd.min <- ndop.fd %>% filter(species %in% ndop.fd.min.species.stat$species)
print(ndop.fd.min)
write.csv(ndop.fd.min, paste0(export_path.v, "ndop-5514.filtry.csv"))
saveRDS(ndop.fd.min, paste0(export_path.v, "ndop-5514.filtry.RDS"))



ndop.fd.min.stat <- ndop.fd.min %>%
    group_by(cat) %>%
    summarize(
        n = n_distinct(key),
        species_n = n_distinct(species),
        presicion_mean = round(mean(precision), digits=1)
    ) %>%
    arrange(desc(n))

print(ndop.fd.min.stat)
write.csv(ndop.fd.min.stat, paste0(export_path.v, "pocet-nalezu-druhu-skupiny.csv"))
saveRDS(ndop.fd.min.stat, paste0(export_path.v, "pocet-nalezu-druhu-skupiny.RDS"))