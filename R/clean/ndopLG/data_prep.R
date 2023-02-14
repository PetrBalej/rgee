# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm", "usdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

export_path <- "/home/petr/Downloads/ndopLG/"


#
# ns: nastavení a průběžné výsledky pro export do protokolu
#
ns <- list()
ns[["time.start"]] <- Sys.time()
ns[["dir.v"]] <- "v0"
ns[["ndop.occ.min"]] <- 30
ns[["ndop.precision"]] <- 1000
ns[["ndop.date.min"]] <- "2018-01-01"
ns[["ndop.date.max"]] <- "2020-12-31"
ns[["vif.th"]] <- 0.5

# základní NDOP data
ndop.ugc <- read_csv("/home/petr/Downloads/ndopLG/ndop-5514.csv")

export_path.v <- paste0("/home/petr/Downloads/ndopLG/", ns[["dir.v"]], "_", ns[["ndop.occ.min"]], "_", ns[["ndop.date.min"]], "_", ns[["ndop.date.max"]], "/")
dir.create(export_path.v, showWarnings = FALSE)


# source(paste0(getwd(), "/R/export_raster/functions.R"))

ns[["ndop.nrows.base"]] <- nrow(ndop.ugc)

ndop.ugc$cat %<>% as.factor
ndop.ugc$species %<>% as.factor

# summary(ndop.ugc)


#
# základní filtr přesností a datem
#
ndop.fd <- ndop.ugc %>%
    filter(date_from >= ns[["ndop.date.min"]] | date_do >= ns[["ndop.date.min"]]) %>%
    filter(precision <= ns[["ndop.precision"]])

ns[["ndop.nrows.datePrecision"]] <- nrow(ndop.fd)



#
# počet nálezů na druhy s omezením do min počtu nálezů na druh
#
ndop.fd.min.species.stat <- ndop.fd %>%
    group_by(species) %>%
    summarise(
        count = n_distinct(key),
        cat = first(cat),
        presicion_mean = round(mean(precision), digits = 1)
    ) %>%
    arrange(desc(count)) %>%
    filter(count >= ns[["ndop.occ.min"]]) %>%
    filter(!is.na(species))

print(ndop.fd.min.species.stat)
write.csv(ndop.fd.min.species.stat, paste0(export_path.v, "pocet-nalezu-druhy.csv"), row.names = FALSE)
saveRDS(ndop.fd.min.species.stat, paste0(export_path.v, "pocet-nalezu-druhy.RDS"))


ndop.fd.min <- ndop.fd %>% filter(species %in% ndop.fd.min.species.stat$species)
print(ndop.fd.min)
write.csv(ndop.fd.min, paste0(export_path.v, "ndop-5514.filtry.csv"), row.names = FALSE)
saveRDS(ndop.fd.min, paste0(export_path.v, "ndop-5514.filtry.RDS"))

ns[["ndop.nrows.minOcc"]] <- nrow(ndop.fd.min)


#
# počet nálezů druhů na skupiny
#
ndop.fd.min.stat <- ndop.fd.min %>%
    group_by(cat) %>%
    summarize(
        n = n_distinct(key),
        species_n = n_distinct(species),
        presicion_mean = round(mean(precision), digits = 1)
    ) %>%
    arrange(desc(n))

print(ndop.fd.min.stat)
write.csv(ndop.fd.min.stat, paste0(export_path.v, "pocet-nalezu-druhu-skupiny.csv"), row.names = FALSE)
saveRDS(ndop.fd.min.stat, paste0(export_path.v, "pocet-nalezu-druhu-skupiny.RDS"))


#
# odstraním vybrané skupiny
#
ns[["ndop.groups.remove"]] <- c("Ryby a mihule", "Pavouci")


"%notin%" <- Negate("%in%")
ndop.fd.min.groups <- ndop.fd.min %>% filter(cat %notin% ns[["ndop.groups.remove"]])


ndop.fd.min.groups <- droplevels(ndop.fd.min.groups)

ns[["ndop.groups"]] <- unique(ndop.fd.min.groups$cat)

ns[["ndop.nrows.removeGroups"]] <- nrow(ndop.fd.min.groups)


#
# temp řešení načtení starých Landsat 8 rasterů
#
r.list <-
    list.files(
        path = paste0(export_path, "1000"),
        pattern = paste0("tif$"),
        ignore.case = TRUE,
        full.names = TRUE,
        recursive = TRUE
    )

r.list <- r.list[grepl("^((?!count|clc|bio03|bio09).)*$", r.list, perl = TRUE)]
r.stack <- stack(lapply(r.list, raster::raster))


ns[["r.names"]] <- names(r.stack)


rr <- writeRaster(r.stack, paste0(export_path.v, "predictors.grd"), format = "raster", overwrite = TRUE)
hdr(rr, format = "ENVI")
saveRDS(r.stack, paste0(export_path.v, "predictors.RDS"))


vif.cor <- vifcor(r.stack, ns[["vif.th"]])
ns[["vif.cor"]] <- t(vif.cor@results)

r.stack.vif <- dropLayer(r.stack, which(ns[["r.names"]] %in% vif.cor@excluded))
# + propíše NA hodnoty napříč layery
r.stack.vif <- raster::mask(r.stack.vif, sum(r.stack.vif))
#  znovu určí minMax hodnoty
r.stack.vif <- raster::setMinMax(r.stack.vif)

rr.vif <- writeRaster(r.stack.vif, paste0(export_path.v, "predictors.vif.grd"), format = "raster", overwrite = TRUE)
hdr(rr.vif, format = "ENVI")
saveRDS(r.stack.vif, paste0(export_path.v, "predictors.vif.RDS"))

ns[["r.names.vif"]] <- names(r.stack.vif)


#
# příprava pro SDM
#



#
# uložení protokolu
#
ns[["time.end"]] <- Sys.time()
# export nastaveí a průběhu do protokolu
ns.t <- lapply(ns, function(x) toString(x))
write.csv(as_tibble(cbind(name = names(ns.t), value = unlist(t(ns.t)))), paste0(export_path.v, "protokol.csv"), row.names = FALSE)