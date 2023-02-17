# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

wd <- "C:/Users/balej/Documents/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

export_path <- "C:/Users/balej/Documents/ndopLG/"
# "C:/Program Files/R/R-4.1.2/bin/x64/Rscript.exe" "C:/Users/balej/Documents/rgee/R/clean/ndopLG/data_sdm.R"


ndop.cat.remove <- c("Ryby a mihule", "Pavouci")
ndop.data <- readRDS("C:/Users/balej/Documents/ndopLG/ndop-5514.filtry.RDS")
preds <- readRDS("C:/Users/balej/Documents/ndopLG/predictors.vif.RDS")
names(preds) <- str_replace_all(names(preds), "[^A-z0-9]", "_")

cz3035 <- st_read(paste0(wd, "/shp/ne_10m_admin_0_countries/czechia/cz_3035.shp"))
cz5514 <- cz3035 %>% st_transform(5514)
preds.c <- mask(preds, cz5514)
preds.c <- stack(preds.c)

"%notin%" <- Negate("%in%")
ndop.data %<>% filter(cat %notin% ndop.cat.remove)
ndop.data <- droplevels(ndop.data)


ndop.data %<>% mutate(species = str_replace_all(species, "[^A-z]", "_"))
ndop.data %<>% mutate(cat = str_replace_all(cat, "[^A-z]", "_"))


sp <- unique(ndop.data$species)
cats <- unique(ndop.data$cat)


spc <- paste(sp, collapse = "+")
predsc <- paste(names(preds) , collapse = "+")


file <- system.file("external/po_spatial_points.shp", package="sdm") # path to a shapefile
# use an appropriate function to read the shapefile (e.g., readOGR in rgdal, readShapeSpatial in
# maptools, or shapefile in raster):
po <- shapefile(file)

for(ct in cats) {
  spss.t <-  ndop.data %>% filter(cat == ct)
  print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  print(ct)
  first <- TRUE
for(sps in unique(spss.t$species)) {
  print("------------------")
  print(sps)
  spss <-  as.data.frame(spss.t %>% filter(species == sps) %>% dplyr::select(X,Y))
  
  spss %<>% mutate(!!sps := 1)
  
  spss.sp <- as_Spatial(spss %>% st_as_sf(coords = c("X", "Y"), crs = 5514) )
 
  frml <- as.formula(paste0(sps, "~",predsc))
  d <- sdm::sdmData(formula=frml,train=spss.sp , bg=list(n=10000, method="gRandom"), predictors = preds.c,  crs=5514)
  m <- sdm::sdm(frml,data=d,methods=c('maxlike'),replication=c('cv'),cv.folds=3, n=3, seed=TRUE,
   parallelSetting=list(ncore=6,method="parallel"))
  
  
  
  df.merge.train <- merge(
    getEvaluation(m, wtest='training',
                  stat=c('AUC','COR','Deviance','obs.prevalence', 'threshold','sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','prevalence')
    ),
    
    getModelInfo(m),
    
    
    by="modelID")
  
  
  df.merge.test <- merge(
    getEvaluation(m,wtest='test.dep',
                  stat=c('AUC','COR','Deviance','obs.prevalence', 'threshold','sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','prevalence')
    ),
    
    getModelInfo(m),
    
    by="modelID")
  


  
  
  dir.create(paste0(export_path, "export/",ct), showWarnings = FALSE)
  
  write.table(df.merge.train, file = paste0(export_path, "export/",ct,"-train.csv"), append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = first)
  write.table(df.merge.test, file = paste0(export_path, "export/",ct,"-test.csv"), append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = first)
  
  if(first){
    first <- FALSE
  }
  
  
  
  terra::predict(terra::rast(preds.c),m,  filename=paste0(export_path, "export/",ct,"/",sps,".tif"), mean=TRUE, overwrite=TRUE) 
  
  
}
}

#####################

library(terra)
dir_list <- list.dirs(paste0(export_path, "exportAll"),recursive = FALSE, full.names = FALSE) 
source(paste0(wd, "/R/export_raster/functions.R"))

for (gr in dir_list) {
  print(gr)
  raster_stack <-
    rasters_dir_stack(
      paste0(
        paste0(export_path, "exportAll/", gr)
      ),
      "tif"
    )
  
  raster_stack_mean <- calc(raster_stack, fun = mean)
  
  
  terra::writeRaster( raster_stack_mean, paste0(export_path, "groupMean/", gr,".tif"), filetype = "GTiff", overwrite = TRUE)

  }




