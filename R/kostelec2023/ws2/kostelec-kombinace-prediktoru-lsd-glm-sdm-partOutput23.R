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

speciesPerGroup <- 3
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- cmd_arg # 1

# "C:/Program Files/R/R-4.2.1/bin/x64/Rscript.exe" "D:/PersonalWork/Balej/sdmKostelec2023/kostelec-kombinace-prediktoru-lsd-glm-sdm-partOutput23.R" 1

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)




# wd <- "D:/PERSONAL_DATA/pb/rgee"
wd <- "D:/PersonalWork/Balej/sdmKostelec2023/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

path.igaD <- "D:/PersonalWork/Balej/sdmKostelec2023/k2023-23/"

df <- readRDS("D:/PersonalWork/Balej/sdmKostelec2023/lsd-PA-FINAL-57preds-118sp.df.rds")
set.seed(85)
df <- df[sample(1:nrow(df)), ]
rownames(df) <- NULL 

remove <- readRDS("D:/PersonalWork/Balej/sdmKostelec2023/preds07-remove.rds")[-1] # jedna záměna ve VIFu

df <- df[,!(names(df) %in% remove)]

species.selected.prevalence <- names(df)[24:141]

species.parts <- split(species.selected.prevalence, ceiling(seq_along(species.selected.prevalence) / speciesPerGroup))


species.parts.f <- paste(species.parts[[species.part]], collapse = "+")

pred.names.all <- names(df)[1:23]

pred.names.all.f <- paste(pred.names.all, collapse = "+")

d <- sdm::sdmData(as.formula(paste0(species.parts.f, "~",pred.names.all.f,"+coords(x+y)")),train=df)

# m <- sdm::sdm(~.,data=d,methods=c('glm'),replication=c('cv'),cv.folds=5)









#pred.names.all.comb <- comb_all(pred.names.all, 2)

#'sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr','mcr','or','ommission','commission','predicted.prevalence'

#c('threshold','sensitivity','specificity','TSS','Kappa','NMI','phi','ppv','npv','ccr', 'mcr','or','ommission','commission','prevalence','obsPrevalence')

m <- list()

for (preds.k in 1:4) {
  pred.names.all.comb <- comb_k(pred.names.all, preds.k)
first <- TRUE
for (pred.names in pred.names.all.comb) {
pred.names.f <- paste(pred.names, collapse = "+")
frml <- as.formula(paste0(species.parts.f,"~", pred.names.f))
print(pred.names.f)

m <- sdm::sdm(frml,data=d,methods=c('glm'),replication=c('cv'),cv.folds=3, n=5, seed=TRUE)
#m <- sdm::sdm(frml,data=d,methods=c('glm'),replication='sub', test=30, n=10)








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

df.merge.train$preds <- pred.names.f
df.merge.test$preds <- pred.names.f


write.table(df.merge.train, file = paste0(path.igaD, preds.k, "-", species.part,"-train.csv"), append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = first)
write.table(df.merge.test, file = paste0(path.igaD, preds.k, "-", species.part,"-test.csv"), append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = first)



if(first){
first <- FALSE
}

#saveRDS(m, paste0(path.igaD, preds.k, "---", species.part, "---",  pred.names.f,".rds"))

m <- list()
gc()
}
}

print(cmd_arg)





#getVarImp(t1)
