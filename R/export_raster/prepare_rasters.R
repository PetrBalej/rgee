# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("raster", "usdm", "stars", "rgdal", "tidyverse")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))
# install.packages("unix")
# library(unix)
# rlimit_as(1e12) #increases to ~12GB

# 1000 c(10, 11, 13) "/home/petr/Downloads/rgee2/export/raster/2021-02-23-10-22-49"
# 2000 c(10, 11, 13) "/home/petr/Downloads/rgee2/export/raster/2021-02-23-12-48-18"
# 200 c(10, 11, 13) "/home/petr/Downloads/rgee2/export/raster/2021-02-23-03-07-28"
# 100 c(10, 11, 13) "/home/petr/Downloads/rgee2/export/raster/2021-02-23-17-30-29"

# 100 c(10, 11, 13) "/home/petr/Downloads/rgee2/export/raster/schuzka_rastery/1000"





# odstranění evidentně nesmyslných bandů do VIFu pro předvýběr do SDM (px_count nechci, slope a aspect k ničemu při velkých cell size)
# 10 px_count
# c(10:32) - vše landsat (+ ndvi) c(as.numeric(10:32))
# c(1:10) - tradiční
# c(3,4,5,7,8,10,12,14,17,18,19,20,23,24,25,26,27,29,30,32)
#raster_stack <- dropLayer(raster_stack, c(10,11,13)) # c(1, 31, 32)
###raster_stack <- dropLayer(raster_stack, c(3,4,5,7,8,10,11,12,13,14,17,18,19,20,23,24,25,27,29,30,31,32)) # c(1, 31, 32)

pixel_size <- 2000

export_path <-
  paste0(getwd(),
         "/../export/raster/schuzka_rastery_vif/",
         pixel_size)
dir.create(export_path, showWarnings = FALSE)
raster_stack <-
  rasters_dir_stack(
    paste0(
      "/home/petr/Downloads/rgee2/export/raster/schuzka_rastery/",
      pixel_size
    ),
    "tif"
  )

# názvy a indexy layerů
names(raster_stack)

# podle potřeby vyloučit nechtěné vrstvy: px_count, aspect, slope, corine (pokud nejsou vhodně sloučené třídy)
raster_stack <-
  dropLayer(raster_stack, c(1, 11, 12, 14)) # c(1,11,12,14)

# VIF
# https://rdrr.io/rforge/usdm/man/vif.html
# https://rdrr.io/rforge/usdm/man/exclude.html

vif(raster_stack)

# identify collinear variables that should be excluded
# v1 <- vifcor(raster_stack, th = 0.9)
v2 <- vifstep(raster_stack, th = 5)
# v1@corMatrix # @variables, @excluded, @results


# # výběr indexů excluded layerů a jejich následné odstranění - dočasně, musím exportovat (v export_raster) rastery do .grd aby se zachovaly názvy vrstev!!!
rs_names <- names(raster_stack)
rs_indexes <- seq_along(names(raster_stack))
rs_ex <- match(v2@excluded, rs_names)

raster_stack_vifstep <- dropLayer(raster_stack, rs_ex)


# # export do asc s příponami původních názvů
# writeRaster(raster_stack_vifstep, paste0(export_path, "/vif2_"), "ascii", bylayer = TRUE, suffix=names(raster_stack_vifstep), overwrite = TRUE)
###writeRaster(raster_stack2, paste0(export_path, "/vif2_"), "ascii", bylayer = TRUE, suffix=names(raster_stack2), overwrite = TRUE)
# # výběr indexů included layerů - dočasně, musím exportovat (v export_raster) rastery do .grd aby se zachovaly názvy vrstev!!!
# layers_included_indexes <- c()
# for (v in v2@results$Variables) {
#   ex <- strsplit(v, split = "\\.")[[1]]
#   layers_included_indexes <- c(layers_included_indexes, ex[length(ex)])
# }
# all_bands_c <- c("px_count", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10", "NDVI", "bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "elevation", "slope", "aspect")
# print(all_bands_c[as.integer(layers_included_indexes)])

writeRaster(
  raster_stack_vifstep,
  paste0(export_path, "/"),
  "ascii",
  bylayer = TRUE,
  suffix = names(raster_stack_vifstep),
  overwrite = TRUE
)