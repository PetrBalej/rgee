# nastavit working directory
wd <- "/home/petr/Documents/igaD/iga2/"
setwd(wd)

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("sp", "raster", "tidyverse", "sf", "magrittr", "dplyr", "stringr")
# "googledrive" # kontrola při použití v ee_Initialize?
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)


# výběr 2019-2022
ndopP <- readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/RP/dataPrep/ndop/ndopP.rds") %>% dplyr::select(DRUH, X, Y, ID_NALEZ)

##############################################################################
### https://code.earthengine.google.com/8e4ad8483815a19996afc3ad59c6966a

# var dataset = ee.Image("CGIAR/SRTM90_V4");
# var elevation = dataset.select("elevation");

# var czechiaBB = ee.Geometry.Polygon({
#   coords: [
#     [[12.0, 48.5], [12.0, 51.1], [19.0, 51.1], [19.0, 48.5], [12.0, 48.5]]
#   ]
# });

# Export.image.toDrive({
#   image: elevation.clip(czechiaBB),
#   description: "elevation_CZ",
#   region: czechiaBB,
#   scale: 90,
#   crs: "EPSG:4326"
# });

# Map.setCenter(15.0, 50.0, 7);
# Map.addLayer(elevation, {min: 0, max: 2000}, "elevation CZ");
# Map.addLayer(czechiaBB, null, "BB");
##############################################################################

elev <- raster::raster(paste0(wd, "elevation_CZ.tif"))

ndopP.elev <- extract(elev, as_Spatial(ndopP))

ndopP$elev <- ndopP.elev

ndopP.export <- as_tibble(ndopP %>% dplyr::select(DRUH, elev)) %>% dplyr::select(-geometry)

saveRDS(ndopP.export, paste0(wd, "ndop_elevation.rds"))

ndopP.stats <- ndopP.export %>%
    group_by(DRUH) %>%
    summarise(
        n = n(),
        mean = mean(elev),
        median = median(elev),
        sd = sd(elev),
        min = min(elev),
        max = max(elev),
        IQR = IQR(elev),
        q10 = quantile(elev, 0.10),
        q25 = quantile(elev, 0.25),
        q75 = quantile(elev, 0.75),
        q90 = quantile(elev, 0.90)
    )


saveRDS(ndopP.stats, paste0(wd, "ndop_elevation_stats.rds"))


#
# odstranění outlierů (_out)
#

outliersNA <- function(x) {
    return(ifelse(x %in% boxplot.stats(x)$out, NA, x))
}

ndopP.export.out <- ndopP.export %>%
    group_by(DRUH) %>%
    mutate(elev = outliersNA(elev)) %>%
    na.omit() %>%
    ungroup()

saveRDS(ndopP.export.out, paste0(wd, "ndop_elevation_out.rds"))

ndopP.stats.out <- ndopP.export.out %>%
    group_by(DRUH) %>%
    summarise(
        n = n(),
        mean = mean(elev),
        median = median(elev),
        sd = sd(elev),
        min = min(elev),
        max = max(elev),
        IQR = IQR(elev),
        q10 = quantile(elev, 0.10),
        q25 = quantile(elev, 0.25),
        q75 = quantile(elev, 0.75),
        q90 = quantile(elev, 0.90)
    )

saveRDS(ndopP.stats, paste0(wd, "ndop_elevation_stats_out.rds"))