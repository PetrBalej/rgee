cmd_arg <- commandArgs(trailingOnly = TRUE)
if (is.na(cmd_arg[1])) {
    print("*********************************************************************************************************************")
    print("nezadán parametr pro část druhů")
    print("*********************************************************************************************************************")
    cmd_arg <- NULL
} else {
    cmd_arg <- cmd_arg[1]
}




# https://github.com/PetrBalej/rgee/blob/master/R/clean/export_raster.R#L459
# "C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/kfme16-100.grd"
# ne, ponechávám všechny Bio  a propíšu NA

# "C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_104_na.grd"



raster_stack <-
  rasters_dir_stack(
    paste0(
      "C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors"
    ),
    "tif"
  )
# 165

raster_stack <- dropLayer(raster_stack, grep("srtm|clc|count|bio03|bio09|b10|stdev", names(raster_stack), ignore.case = TRUE)) # 100

names(raster_stack) <- str_replace_all(names(raster_stack), c("kfme16." = "", "6_mean.bio" = "mean.bio", "6_cv.bio" = "cv.bio", "_30" = "", "\\.nd" = "", "\\.constant" = "", "\\." = "_"))


# + propíše NA hodnoty napříč layery
raster_stack_na <- raster::mask(raster_stack, sum(raster_stack))
#znovu určí minMax hodnoty
raster_stack_na <- raster::setMinMax(raster_stack_na) # inf values - nefungovalo...


rr <- writeRaster(raster_stack_na, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.grd", format = "raster", overwrite = TRUE)
hdr(rr, format = "ENVI")

writeRaster(raster_stack_na, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.tiff", format = "gtiff", overwrite = TRUE)

# nejde - nefunkční python/gdal na NB?!?!
saveRDS(raster_stack_na, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.rds")



#saveRDS(m, "C:/Users/petr/Downloads/igaD/igaD/clean/k2023.rds")

### presence 9307 pozorování, 164 kvadrátů; https://github.com/PetrBalej/rgee/blob/master/R/clean/export_raster.R#L661
# "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/LSD-164-4326_2018-2021_4-6_synonymUnite_distinct.rds" 








lsd.prep <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/LSD-164-4326_2018-2021_4-6_synonymUnite_distinct.rds")

# odstranit kvadráty v NA z rasteru ??
lsd.NA <- which(is.na(extract(raster_stack_na[[1]], as_Spatial(lsd.prep))))
lsd.prepNA <- lsd.prep[-lsd.NA, ] 



# odvodit pro LSD absence
"%notin%" <- Negate("%in%")

lsd.prepNA.temp <-  lsd.prepNA 
lsd.prepNA.temp$presence <- 1
lsd.prepNA.temp %<>% dplyr::select(POLE, species, presence)
POLE.unique <- lsd.prepNA.temp %>%   dplyr::select(POLE) %>%  distinct(across(c(POLE, geometry)))



POLE.unique.sampled.th09 <- extract(raster_stack_na.th09, as_Spatial(POLE.unique))
POLE.unique.sampled.df.th09 <- cbind(POLE.unique, POLE.unique.sampled.th09 )



POLE.unique.sampled.df.th09.t <- as_tibble(POLE.unique.sampled.df.th09 )


POLE.unique.sampled.df.th09.t %<>% arrange(POLE)



POLE.unique.sampled.df.th09.t.new <-  POLE.unique.sampled.df.th09.t
lsd.prepNA.temp.selectedSpecies$presence  %<>% as.character
lsd.prepNA.temp.selectedSpecies$presence  %<>% as.integer

for (sp in as.vector(unique(lsd.prepNA.temp.t.selectedSpecies$species))) {

 sp.presence <- as_tibble(lsd.prepNA.temp.selectedSpecies  %>% filter(species==sp) %>% arrange(POLE) ) %>% dplyr::select(presence)
  
 sp.u <- str_replace(sp, " ", "_")
 
 POLE.unique.sampled.df.th09.t.new  %<>% add_column(!!(as.character(sp.u)) := as.vector(unlist(sp.presence)))
  
}


# udělat predikce: jen bio, jen landsat, kombinace


POLE.unique.sampled.df.th09.t.new

#  saveRDS(POLE.unique.sampled.df.th09.t.new, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.rds")

POLE.unique.sampled.df.th09.t.new.csv <- POLE.unique.sampled.df.th09.t.new
POLE.unique.sampled.df.th09.t.new.csv$geometry  %<>% st_as_text
#  write.csv(POLE.unique.sampled.df.th09.t.new.csv, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.csv", row.names = FALSE)

POLE.unique.sampled.df.th09.t.new.csv <- POLE.unique.sampled.df.th09.t.new

POLE.unique.sampled.df.th09.t.new %<>% mutate()


POLE.unique.sampled.df.th09.t.new.xy <- POLE.unique.sampled.df.th09.t.new 


POLE.unique.sampled.df.th09.t.new.xy %<>% mutate(x = st_coordinates(geometry)[,1])
POLE.unique.sampled.df.th09.t.new.xy %<>% mutate(y = st_coordinates(geometry)[,2])
#  write.csv(POLE.unique.sampled.df.th09.t.new.xy %>% dplyr::select(-geometry), "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.csv", row.names = FALSE)


# saveRDS(POLE.unique.sampled.df.th09.t.new.xy, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.rds")

df.final <-  as.data.frame(POLE.unique.sampled.df.th09.t.new.xy  %>% dplyr::select(-c(POLE, geometry)))



# saveRDS(df.final, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.df.rds")





df <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.df.rds")










ndop.all <- read_csv("C:/Users/petr/Downloads/ndop-5514/ndop-5514.csv")


ndop.all$cat %<>% as.factor
ndop.all$species %<>% as.factor

summary(ndop.all)

ndop.all %>%    filter(date_do <= "2012-01-01" & cat == "Plazi")


ndop.all %>%                     
  split(.$cat) %>%
  map(summary)



plot(ndop.all$precision)


hist(ndop.all$precision)




# POLE.unique.sampled.df.th09.t %>% left_join(lsd.prepNA.temp.t, by="POLE")






lsd.prepNA.temp.sampled.df.th09 <- cbind(lsd.prepNA.temp.selectedSpecies, lsd.prepNA.temp.sampled.th09)
# saveRDS(lsd.prepNA.temp.sampled.df.th09, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118selectes_bioclim-landsat8-cv-mean-100vifcor57.th09.rds")






for (sp in as.vector(unique(lsd.prepNA.temp$species))) {
  sp.presences <- lsd.prepNA.temp %>% filter(species == sp)
  sp.absences <- POLE.unique %>% filter(POLE %notin% sp.presences$POLE)
  
  if(nrow(sp.absences) > 0){
  sp.absences$species <- sp
  sp.absences$presence <- 0
  
  lsd.prepNA.temp %<>% add_row(sp.absences)
  }
  
}

# 32076 PA, 198 druhů

lsd.prepNA.temp$POLE %<>% as.factor 
lsd.prepNA.temp$species %<>% as.factor 
lsd.prepNA.temp$presence %<>% as.factor 
# saveRDS(lsd.prepNA.temp, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp.rds")
summary(lsd.prepNA.temp)




lsd.prepNA.temp.sampled <- extract(raster_stack_na, as_Spatial(lsd.prepNA.temp))
lsd.prepNA.temp.sampled.df <- cbind(lsd.prepNA.temp, lsd.prepNA.temp.sampled)

# saveRDS(lsd.prepNA.temp.sampled.df, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp_bioclim-landsat8-cv-mean-100.rds")

library(usdmPB)
raster_stack_na.vif <- usdmPB::vif(raster_stack_na)

raster_stack_na.vifcor <- usdmPB::vifcor(raster_stack_na, th=0.9) # potřebuju znát "vítězný" raster

# saveRDS(raster_stack_na.vifcor, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/vif/lsd-PA-162sq-198sp_bioclim-landsat8-cv-mean-100_vifcor.rds")


raster_stack_na.th09 <- dropLayer(raster_stack_na, raster_stack_na.vifcor@excluded)




lsd.prepNA.temp.sampled.th09 <- extract(raster_stack_na.th09, as_Spatial(lsd.prepNA.temp))
lsd.prepNA.temp.sampled.df.th09 <- cbind(lsd.prepNA.temp, lsd.prepNA.temp.sampled.th09)
#  saveRDS(lsd.prepNA.temp.sampled.df.th09, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp_bioclim-landsat8-cv-mean-100.th09.rds")


lsd.prepNA.temp.t <- as_tibble(lsd.prepNA.temp)

lsd.prepNA.temp.t$presence  %<>% as.character
lsd.prepNA.temp.t$presence  %<>% as.integer



lsd.prepNA.temp.t.c <- lsd.prepNA.temp.t %>% group_by(species) %>% summarise(p = sum(presence))  %>% arrange(p)  
# saveRDS(lsd.prepNA.temp.t.c, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp_countSq.rds")

lsd.prepNA.temp.t.selectedSpecies <- lsd.prepNA.temp.t.c  %>% filter(p >= 10 & p <= 152)

# saveRDS(lsd.prepNA.temp.t.selectedSpecies, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118.rds")


lsd.prepNA.temp.selectedSpecies <- lsd.prepNA.temp %>% filter(species %in% lsd.prepNA.temp.t.selectedSpecies$species)

# saveRDS(lsd.prepNA.temp.selectedSpecies, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118selectes.rds")



 
lsd.prepNA.temp.sampled.th09 <- extract(raster_stack_na.th09, as_Spatial(lsd.prepNA.temp.selectedSpecies))
lsd.prepNA.temp.sampled.df.th09 <- cbind(lsd.prepNA.temp.selectedSpecies, lsd.prepNA.temp.sampled.th09)
# saveRDS(lsd.prepNA.temp.sampled.df.th09, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118selectes_bioclim-landsat8-cv-mean-100vifcor57.th09.rds")



# umí sdm vše najednou? Naimi tvrdil že ano a PA to umožňuje vložit

##
## udělat pokus s přepsíním centra výkytu druhu v jeho rasteru s nějvětším přispěním, jak se to proveví ve VIFu
##

library(sdm)

file <- system.file("external/multi_pa_df.csv", package="sdm")
df <- read.csv(file)



data.temp <-
  sdmPB::sdmData(frml,
                 train = train,
                 bg = bg,
                 test = test)
sdmPB::sdm(
  frml,
  data = data.temp,
  methods = c("maxent"),
  feat = c("linear", "quadratic", "product"),
  removeDuplicates = FALSE,
  c("removeDuplicates=FALSE"),
  parallelSettings = NULL
)






