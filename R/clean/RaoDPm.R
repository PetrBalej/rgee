cmd_arg <- commandArgs(trailingOnly = TRUE)
if (is.na(cmd_arg[1])) {
    print("*********************************************************************************************************************")
    print("nezadán parametr pro část druhů")
    print("*********************************************************************************************************************")
    cmd_arg <- NULL
} else {
    cmd_arg <- cmd_arg[1]
}


print(cmd_arg)

# Rscript "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/clean/RaoDPm.R" 0rad_4_mean.tif

r.path <- "/mnt/6C2B3F36770071FA/presuny-u20/igaD0/c100m/merged/0-rad"

if (is.null(cmd_arg)) {
    tifs.path <- list.files(r.path, "^0rad.*mean\\.tif$", full.names = TRUE)
} else {
    tifs.path <- paste0(r.path, "/", cmd_arg)
}

print(tifs.path)


# nastavit working directory
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

path.igaD <- "/home/petr/Documents/igaD/"

path.export <- "/mnt/6C2B3F36770071FA/presuny-u20/igaD0/rao/"

# závisí na některých funkcích z:
source(paste0(getwd(), "/R/export_raster/functions.R"))

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "readxl", "abind", "stringr")
# "googledrive" # kontrola při použití v ee_Initialize?
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


## výběr regionu
SitMap.selected <- unname(unlist(read_csv(paste0(wd, "/R/clean/SitMap_0Rad-selected.csv"))))
SitMap <- st_transform(st_read(paste0(path.igaD, "sitmap_0rad/sitmap_0rad.shp")), crs = 4326) %>% dplyr::select(POLE)
SitMap.POLE.selected <- SitMap %>% dplyr::filter(POLE %in% SitMap.selected)


RaoQareaM <- function(x, y) {
    crop1 <- crop(x, y)
    mat_s_all <- values(crop1)
    mat_s <- na.omit(mat_s_all)
    n_s <- length(mat_s)
    n2_s <- n_s^2
    distm_s <- as.matrix(dist(mat_s))
    rao <- (sum(distm_s)) / n2_s
    return(list("rao" = rao, "pct" = round(n_s / length(mat_s_all), 3)))
}




rr <- list()
for (tif.path in tifs.path) {
    print(tif.path)
    img <- stack(tif.path)
    img.name <- sub("\\..*$", "", basename(tif.path))

    for (band in names(img)) {
        print(band)

        for (idPOLE in SitMap.POLE.selected$POLE) {
            print(idPOLE)
            POLE.selected <- SitMap.POLE.selected %>% filter(POLE == idPOLE)
            rr[[as.character(idPOLE)]][[band]][[img.name]] <- RaoQareaM(img[[band]], POLE.selected)
            gc()
        }
    }
}


saveRDS(rr, file = paste0(path.export, "0rad-export-mean-", cmd_arg, "-.rds"))


