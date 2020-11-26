# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("raster", "usdm", "stars", "rgdal")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)



export_path <- paste0(getwd(), "/../export/raster/")
file_name <- paste0(export_path, "/test_2020-11-25-18-29-52.tif")
# import uloženého rasteru z fyzického souboru
raster_stack <- stack(file_name) 
# odstranění evidentně nesmyslných bandů do VIFu pro předvýběr do SDM
raster_stack <- dropLayer(raster_stack, c(1,31,32))



# VIF
# https://rdrr.io/rforge/usdm/man/vif.html
# https://rdrr.io/rforge/usdm/man/exclude.html

vif(raster_stack)

# identify collinear variables that should be excluded
v1 <- vifcor(raster_stack, th=0.9) 
v2 <- vifstep(raster_stack, th=10)
# v1@corMatrix # @variables, @excluded, @results



