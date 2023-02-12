# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)




# wd <- "D:/PERSONAL_DATA/pb/rgee"
wd <- "C:/Users/petr/Downloads/igaD/rgeeDP/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

path.igaD <- "C:/Users/petr/Downloads/igaD/igaD/clean/k2023/"



# scénáře a vliv na výsledky: 
# 1) pevně dané prediktory vifcor 0.7 (biomean/cv) x landsat(mean/cv)) - musím dodělat vifcor=0.7 (zvlášť) a vybrat jen dotyčné prediktory!
# 2a) nesmysl omezevat velkým vifem (nechat vyšší korelaci, nevím na co druh a z kterých oblastí zabírá)
# 2b) nechat vybrat nejlepší kombinaci


# udělat i glmnet!!!
# GLM - je transparentní, žádné optimalizace ani magie - jen medián a kombinace prediktorů (3 fold cross-validation + 5 replication)



#  variance inflation factor (VIF) to avoid over-fitting !!! - najít citaca Araujo Standards
# Jak dokážu, že jsem nezpůsobil overfitting volbou korelovaných proměnných???

#Spatial filtering to reduce sampling bias can improve the performance of ecological niche models:
# To quantify overfitting and model performance, we calculated evaluation AUC, the difference between calibration and evaluation AUC (=AUCdiff), and omission rates.



# chybně jsem přidal názv y sloupců jen k "1-*.csv" - musím je načíst samostatně se záhlavím a pak zvlášť ostatní bez záhlaví
csv.first <- list.files(paste0(path.igaD,"csv/k2023-temp2"), "^1-.*test\\.csv$", full.names = TRUE)
csv.rest <- list.files(paste0(path.igaD,"csv/k2023-temp2"), "^([2-9]|[1-9][0-9])-.*test\\.csv$", full.names = TRUE)
first <- TRUE
for(ff in csv.first){

  if(first){
    first <- FALSE
    csv.out <-  read_csv(
      ff,
      col_names = TRUE)
  }else{
    csv.out %<>% add_row(read_csv(
      ff,
      col_names = TRUE))
    
  }

}



for(fr in csv.rest){

fr.tmp <-  read_csv(fr,col_names = FALSE)
names(fr.tmp) <- names(csv.out)

  csv.out %<>% add_row(fr.tmp)

}


csv.out$species %<>% as.factor
csv.out$preds %<>% as.factor

#summary(csv.out)
# bylo by vhodné připojit i počet presencí z LSD 162...

#saveRDS(csv.out, paste0(path.igaD, "k2023-temp2.rds"))

csv.out.selected <- csv.out %>% dplyr::select(AUC, COR, Deviance, Prevalence, sensitivity, specificity, TSS, prevalence, species, preds)
# saveRDS(csv.out.selected, paste0(path.igaD, "k2023-temp2-selected.rds"))


# zprůměrování a
# výběr nej kombinace

 

#summary(csv.out.selected)


csv.out.selected.g <- csv.out.selected %>% group_by(species, preds)


csv.out.selected.g.tbl <- csv.out.selected.g %>% 
  summarise(across(everything(), median),
            .groups = 'drop')

# saveRDS(csv.out.selected.g.tbl, paste0(path.igaD, "k2023-temp2-selected-mean.rds"))

 saveRDS(csv.out.selected.g.tbl, paste0(path.igaD, "k2023-temp2-selected-median.rds"))




unique((csv.out.selected.g.tbl %>% filter(AUC >= 0.75))$species) # zatím 84 (mean) / 90 (median) ze 118
unique(csv.out.selected.g.tbl$species)

unique(csv.out.selected.g.tbl$preds)









# nový vif
library(usdmPB)

raster_stack_na <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.rds")

raster_stack_na.vifcor <- usdmPB::vifcor(raster_stack_na, th=0.7) # potřebuju znát "vítězný" raster



remove <- as.vector(unlist(raster_stack_na.vifcor@excludedPairs))
df <- subset(mydata, select = -remove )


ok <- names(raster_stack_na.vifcor@excludedPairs)

saveRDS(remove, paste0(path.igaD, "preds07-remove.rds"))
saveRDS(ok, paste0(path.igaD, "preds07-ok.rds"))


df <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-FINAL-57preds-118sp.df.rds")
#df <- df[sample(1:nrow(df)), ]
#rownames(df) <- NULL 


species.selected.prevalence <- names(df)[58:175]

species.parts <- split(species.selected.prevalence, ceiling(seq_along(species.selected.prevalence) / speciesPerGroup))


species.parts.f <- paste(species.parts[[species.part]], collapse = "+")

pred.names.all <- names(df)[1:57]

pred.names.all.f <- paste(pred.names.all, collapse = "+")

d <- sdm::sdmData(as.formula(paste0(species.parts.f, "~",pred.names.all.f,"+coords(x+y)")),train=df)

# m <- sdm::sdm(~.,data=d,methods=c('glm'),replication=c('cv'),cv.folds=5)









#pred.names.all.comb <- comb_all(pred.names.all, 2)


m <- list()

for (preds.k in 1:3) {
  pred.names.all.comb <- comb_k(pred.names.all, preds.k)

for (pred.names in pred.names.all.comb) {
pred.names.f <- paste(pred.names, collapse = "+")
frml <- as.formula(paste0(species.parts.f,"~", pred.names.f))
print(pred.names.f)

m <- sdm::sdm(frml,data=d,methods=c('glm'),replication=c('cv'),cv.folds=5)
saveRDS(m, paste0(path.igaD, preds.k, "---", species.part, "---",  pred.names.f,".rds"))

m <- list()
gc()
}
}

print(cmd_arg)











raster_stack_na <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.rds")
raster_stack_na.vifcor <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/vif/lsd-PA-162sq-198sp_bioclim-landsat8-cv-mean-100_vifcor.rds")


raster_stack_na.th09 <- dropLayer(raster_stack_na, raster_stack_na.vifcor@excluded)















p1 <- predict(m, newdata=raster_stack_na.th09, filename='kostelec23-test.png')
plot(p1)




m <- sdm::sdm(~.+coords(x+y),data=d,methods=c('glm'),
              replication='sub',test.percent=30,n=3)

m2 <- sdm::sdm(~.,data=d,methods=c('glm'),
              replication='sub',test.percent=30,n=3)


str(m2, max.level=2)
str(m2@replicates$Ardea_alba[[1]], max.level=1)
str(m@models$Ardea_alba[[1]][[1]], max.level=1)


