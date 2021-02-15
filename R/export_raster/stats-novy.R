# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("raster", "usdm", "stars", "rgdal", "spThin", "tidyverse", "dismo", "rJava")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)


# install.packages("unix") 
# library(unix)
# rlimit_as(1e12) #increases to ~12GB




export_path <- paste0(getwd(), "/../export/raster/2020-12-06-05-06-49/")
# export_path <- paste0(getwd(), "/../export/raster/2020-12-06-05-59-39/")
# file_name <- paste0(export_path, "/2020-11-30-15-11-29/multiband_2020-11-30-15-11-29.grd")
file_name <- paste0(export_path, "/multibandX_2020-12-06-05-06-49.grd")
# file_name <- paste0(export_path, "/multibandX_2020-12-06-05-59-39.gri")


# import uloženého rasteru z fyzického souboru
raster_stack <- stack(file_name)
# odstranění evidentně nesmyslných bandů do VIFu pro předvýběr do SDM (px_count nechci, slope a aspect k ničemu při velkých cell size)
raster_stack <- dropLayer(raster_stack, c()) # c(1, 31, 32)



# VIF
# https://rdrr.io/rforge/usdm/man/vif.html
# https://rdrr.io/rforge/usdm/man/exclude.html

vif(raster_stack)

# identify collinear variables that should be excluded
# v1 <- vifcor(raster_stack, th = 0.9)
v2 <- vifstep(raster_stack, th = 10)
# v1@corMatrix # @variables, @excluded, @results


# výběr indexů excluded layerů a jejich následné odstranění - dočasně, musím exportovat (v export_raster) rastery do .grd aby se zachovaly názvy vrstev!!!
rs_names <- names(raster_stack)
rs_indexes <- seq_along(names(raster_stack))
rs_ex <- match(v2@excluded, rs_names)

raster_stack_vifstep <- dropLayer(raster_stack, rs_ex)

# export do asc - zatím rovnou do MaxEntu
writeRaster(raster_stack_vifstep, paste0(export_path, "/vif"), "ascii", bylayer = TRUE, overwrite = TRUE)
# export do asc s příponami původních názvů
writeRaster(raster_stack_vifstep, paste0(export_path, "/vif2"), "ascii", bylayer = TRUE, suffix=names(raster_stack_vifstep), overwrite = TRUE)

# výběr indexů included layerů - dočasně, musím exportovat (v export_raster) rastery do .grd aby se zachovaly názvy vrstev!!!
layers_included_indexes <- c()
for (v in v2@results$Variables) {
  ex <- strsplit(v, split = "\\.")[[1]]
  layers_included_indexes <- c(layers_included_indexes, ex[length(ex)])
}
all_bands_c <- c("px_count", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10", "NDVI", "bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "elevation", "slope", "aspect")
print(all_bands_c[as.integer(layers_included_indexes)])





# + coordinateCleaner a spThin na NDOP/GBIF data...
res_ndop <- ndop(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../ndop/csv-top5"))
# res_gbif <- gbif(list(from = '2017-01-01', to = '2019-12-31'), list(from = 4, to = 7), paste0(getwd(), "/../gbif/csv"), "0123613-200613084148143.csv")
write_csv(res_ndop, paste0(export_path, "res_ndop", ".csv"))

species <- "Bombina bombina"
species_col <- "Bombina_bombina"

# NDOP
res_ndop_ll <- res_ndop %>% filter(species == !!species)
write_csv(res_ndop_ll, paste0(export_path, "res_ndop_ll_bb", ".csv"))

res_ndop_ll_spthin <-
  thin(loc.data = res_ndop_ll,
        lat.col = "latitude", long.col = "longitude",
        spec.col = "species",
        thin.par = 0.1, reps = 10,
        locs.thinned.list.return = TRUE,
        write.files = FALSE,
        write.log.file = FALSE)

res_ndop_ll_spthin <- as_tibble(res_ndop_ll_spthin[[1]]) # %>% add_column(species = !!species_col, .before = 1)
write_csv(res_ndop_ll_spthin, paste0(export_path, "res_ndop_ll_spthin_bb", ".csv"))


# GBIF
# res_gbif_ll <- res_gbif %>% filter(species == !!species)

# res_gbif_ll_spthin <-
#   thin(loc.data = res_gbif_ll,
#         lat.col = "latitude", long.col = "longitude",
#         spec.col = "species",
#         thin.par = 0.1, reps = 10,
#         locs.thinned.list.return = TRUE,
#         write.files = FALSE,
#         write.log.file = FALSE)

# res_gbif_ll_spthin <- as_tibble(res_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
# write_csv(res_gbif_ll_spthin, paste0(export_path, "res_gbif_ll_spthin", ".csv"))




# čistě spojení už thinovaných datasetů - asi ne, thinning znovu až nad spojeným datasetem?
# ndop_gbif <- res_gbif_ll_spthin %>% add_row(res_ndop_ll_spthin)


# GBIF+NDOP
# ndop_gbif <- res_ndop %>% add_row(res_gbif)

# ndop_gbif_ll <- ndop_gbif %>% filter(species == !!species)

# ndop_gbif_ll_spthin <-
#   thin(loc.data = ndop_gbif_ll,
#         lat.col = "latitude", long.col = "longitude",
#         spec.col = "species",
#         thin.par = 0.1, reps = 10,
#         locs.thinned.list.return = TRUE,
#         write.files = FALSE,
#         write.log.file = FALSE)

# ndop_gbif_ll_spthin <- as_tibble(ndop_gbif_ll_spthin[[1]]) %>% add_column("species" = !!species_col, .before = 1)
# write_csv(ndop_gbif_ll_spthin, paste0(export_path, "ndop_gbif_ll_spthin", ".csv"))





# test if you can use maxent
# maxent()
# if (maxent()) {
#   # dismo maxent vyžaduje pouze souřadnice bez sloupce s názvem druhu
#   res_gbif_ll_spthin_df <- as.data.frame(res_gbif_ll_spthin %>% select(Longitude, Latitude))
#   mx <- maxent(raster_stack_vifstep, res_gbif_ll_spthin_df, a = NULL, factors = NULL, removeDuplicates = TRUE, nbg = 1000, path = paste0(export_path, "/2020-11-30-15-11-29/maxent"))
#   mx_predict <- predict(mx, raster_stack_vifstep)

#   # Odlišný prostorový rozsah nálezů a modelované predikce distribuce!!! Proč?
#   # plot(mx_predict)
#   # points(res_gbif_ll_spthin_df)

#   # Dále:
#   # https://www.rdocumentation.org/packages/dismo/versions/1.3-3/topics/maxent
# }


