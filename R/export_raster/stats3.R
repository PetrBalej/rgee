# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("raster", "usdm", "stars", "rgdal", "spThin", "tidyverse") # , "dismo", "rJava"
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)


# VIF
# https://rdrr.io/rforge/usdm/man/vif.html
# https://rdrr.io/rforge/usdm/man/exclude.html

raster_stack <- stack("./../mbAAA_3035.grd")

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
writeRaster(raster_stack_vifstep, paste0(export_path, "/vif_"), "ascii", bylayer = TRUE, overwrite = TRUE)


# výběr indexů included layerů - dočasně, musím exportovat (v export_raster) rastery do .grd aby se zachovaly názvy vrstev!!!
layers_included_indexes <- c()
for (v in v2@results$Variables) {
  ex <- strsplit(v, split = "\\.")[[1]]
  layers_included_indexes <- c(layers_included_indexes, ex[length(ex)])
}
all_bands_c <- c("px_count", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10", "NDVI", "bio01", "bio02", "bio03", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "elevation", "slope", "aspect")
print(all_bands_c[as.integer(layers_included_indexes)])


