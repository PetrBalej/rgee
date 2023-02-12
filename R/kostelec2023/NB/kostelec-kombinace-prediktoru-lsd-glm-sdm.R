cmd_arg <- commandArgs(trailingOnly = TRUE)
if (is.na(cmd_arg[1])) {
  print("*********************************************************************************************************************")
  print("nezadán parametr pro část druhů")
  print("*********************************************************************************************************************")
  cmd_arg <- 1
}else{
  cmd_arg <- cmd_arg[1]
}


print(cmd_arg)

speciesPerGroup <- 4
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- cmd_arg # 1



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


