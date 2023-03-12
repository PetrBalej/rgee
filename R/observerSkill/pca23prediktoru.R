# library(devtools)
# install_github("bleutner/RStoolbox")
# install_github("PetrBalej/RStoolbox", force = TRUE)
library(RStoolbox)
raster_stack_100_na_23 <- readRDS("raster_stack_100_na_23.rds")

# PCA všech 23
pcamap <- rasterPCA(raster_stack_100_na_23, spca = TRUE)
summary(pcamap$model)
# alespoň 90 % vysvětluje prvních 14 os

# names(raster_stack_100_na_23)

# PCA jen Landsat (15)
pcamap.landsat <- rasterPCA(raster_stack_100_na_23[[1:15]], spca = TRUE)
summary(pcamap.landsat$model)
# alespoň 90 % vysvětluje prvních 9 os


# PCA jen BioCLim (8)
pcamap.bioclim <- rasterPCA(raster_stack_100_na_23[[16:23]], spca = TRUE)
summary(pcamap.bioclim$model)
# alespoň 90 % vysvětluje prvních 5 os


# Zuur, A.F., Ieno, E.N. and Elphick, C.S., 2010. A protocol for data exploration to avoid common statistical problems. Methods in ecology and evolution, 1(1), pp.3-14.
# - but a more stringent approach is to use values as low as 3 as we did here. High, or even moderate, collinearity is especially problematic when ecological signals are weak. In that case, even a VIF of 2 may cause nonsignificant parameter estimates, compared to the situation without collinearity.

library(usdm)
library(raster)
# vifstep
vs <- vifstep(raster_stack_100_na_23, th = 2)
# vifcor - jen pro dofiltr, asi není nutné
vs.vc <- vifcor(raster_stack_100_na_23[[vs@results$Variables]], th = 0.7)
print(vs.vc@results$Variables)
# PCA vs.vc
pcamap <- rasterPCA(raster_stack_100_na_23[[vs.vc@results$Variables]], spca = TRUE)
summary(pcamap$model)



# 100 původních rasterů
raster_stack_100_na <- stack("C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.grd")
names(raster_stack_100_na)

pcamap100 <- rasterPCA(raster_stack_100_na, spca = TRUE)
summary(pcamap100$model)