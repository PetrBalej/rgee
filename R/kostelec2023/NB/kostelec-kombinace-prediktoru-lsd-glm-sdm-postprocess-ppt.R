# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyr", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm")
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




###############################################
###############################################
###############################################

library(dplyr)
library(magrittr)

### k maximám potřebuju i prediktory, kterýma toho bylo dosaženo!!!

lsd162 <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118selectes.rds")

# st_write(lsd162, "C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118selectes.shp")

k2023m <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/k2023/postprocess/k2023-selected-median.rds")

k2023m$preds %<>% as.factor

summary(k2023m)
k2023m.f <- k2023m %>% filter(AUC.cv <= 15 & Deviance <= 1.2 & COR.cv <= 94) # cílem je odstranit "nestabilní" modely
summary( k2023m.f)

# # # # # vybrat 30+ presencí druhy, ikdyž jich bude jen 74; 162 lokalit


sp.count <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/occurrences/lsd-PA-162sq-198sp118.rds")
sp.count.f <- sp.count %>% filter(p >= 30)  %>% dplyr::select(species)
sp.count.f <- gsub(" ", "_", unlist(sp.count.f))

k2023m.fs  <- k2023m %>% filter(species %in% sp.count.f)
summary( k2023m.fs)

m.f <- list("all" = k2023m,
            "fcv" = k2023m.f,
            "f30" = k2023m.fs)

m.v.names <- c("all", "bio", "l8")


# na pevno vnucený počet prediktorů!!! Musím bych udělat i kumulativní! - Bude se to lišit??? (snížení AUC)
m.v <- list()
# kumulativní počet prediktorů
m.v2 <- list()


for (f in names(m.f) ){

  for (k in 1:4) {
    
    m.v[[f]][[m.v.names[2]]][[as.character(k)]] <-
      m.f[[f]] %>% filter(l.bio == TRUE & l.l8 == FALSE & preds.n == k)
    
    m.v[[f]][[m.v.names[3]]][[as.character(k)]] <-
      m.f[[f]] %>% filter(l.bio == FALSE & l.l8 == TRUE & preds.n == k)
    
    m.v[[f]][[m.v.names[1]]][[as.character(k)]] <-
      m.f[[f]] %>% filter(preds.n == k)
    
    
    
    m.v2[[f]][[m.v.names[2]]][[as.character(k)]] <-
      m.f[[f]] %>% filter(l.bio == TRUE & l.l8 == FALSE & preds.n <= k)
    
    m.v2[[f]][[m.v.names[3]]][[as.character(k)]] <-
      m.f[[f]] %>% filter(l.bio == FALSE & l.l8 == TRUE & preds.n <= k)
    
    m.v2[[f]][[m.v.names[1]]][[as.character(k)]] <-
      m.f[[f]] %>% filter(preds.n <= k)
    
    
    
    
  }
}






m.max <- list()
m.max2 <- list()
m.max.alt <- list() # zachová ostatní sloupce - asi lepší
m.max.alt2 <- list() # zachová ostatní sloupce - asi lepší
for (f in names(m.f) ) {
  for (m.v.name in m.v.names) {
    for (k in 1:4) {
      m.max[[f]][[m.v.name]][[as.character(k)]] <-
        m.v[[f]][[m.v.name]][[as.character(k)]] %>% group_by(species) %>% summarise(auc.max = max(AUC))
      
      
      m.max2[[f]][[m.v.name]][[as.character(k)]] <-
        m.v2[[f]][[m.v.name]][[as.character(k)]] %>% group_by(species) %>% summarise(auc.max = max(AUC))
      
      
      m.max.alt[[f]][[m.v.name]][[as.character(k)]] <-
        m.v[[f]][[m.v.name]][[as.character(k)]] %>% group_by(species)  %>% slice_max(AUC , n = 1, with_ties = FALSE)
      
      m.max.alt2[[f]][[m.v.name]][[as.character(k)]] <-
        m.v2[[f]][[m.v.name]][[as.character(k)]] %>% group_by(species)  %>% slice_max(AUC , n = 1, with_ties = FALSE)
      
      
    }
  }
}

# m.max[["all"]][["all"]][[as.character(4)]]$auc.max
# m.max.alt[["all"]][["all"]][[as.character(4)]]$AUC)




m.max.median <- list()
for (f in names(m.f) ) {
  for (m.v.name in m.v.names) {
    for (k in 1:4) {
      m.max.median[[f]][[m.v.name]][[as.character(k)]] <-
        median(m.max[[f]][[m.v.name]][[as.character(k)]]$auc.max)
      
    }
  }
}
# xxx tabulku z all 1:4/bio,l8,all




# 118/74
# m.max
m.max.kostelec3 <- list()
m.max.kostelec3.median <- list()
m.max.kostelec3.thr <- list()

# kumulativní
m.max.kostelec3c <- list()
m.max.kostelec3c.median <- list()
m.max.kostelec3c.thr <- list()


create.tibble <- TRUE

cn <- c("l1_species", "l2_predsSets", "l3_predsComb", "spCountAuc75")


for(l1 in names(m.max)){
 
  for(l2 in names(m.max[[l1]])){
   
    # m.max.kostelec1 <- list()
    # m.max.kostelec2 <- list()
    for(l3 in names(m.max[[l1]][[l2]])){
    
      
      # m.max.kostelec1[[l3]] <- m.max[[l1]][[l2]][[l3]]$auc.max
      # m.max.kostelec2[[l3]] <- m.max2[[l1]][[l2]][[l3]]$auc.max
      
      
      m.max.kostelec3[[l1]][[l2]][[l3]] <- m.max[[l1]][[l2]][[l3]]$auc.max
      # m.max.kostelec3.median[[l1]][[l2]][[l3]] <- median(m.max[[l1]][[l2]][[l3]]$auc.max)
      m.max.kostelec3.thr[[l1]][[l2]][[l3]] <- sum(m.max[[l1]][[l2]][[l3]]$auc.max >= 0.75)

      
      m.max.kostelec3c[[l1]][[l2]][[l3]] <- m.max2[[l1]][[l2]][[l3]]$auc.max
      # m.max.kostelec3.median[[l1]][[l2]][[l3]] <- median(m.max[[l1]][[l2]][[l3]]$auc.max)
      m.max.kostelec3c.thr[[l1]][[l2]][[l3]] <- sum(m.max2[[l1]][[l2]][[l3]]$auc.max >= 0.75)
      
      
      row <- c(l1, l2, l3, m.max.kostelec3.thr[[l1]][[l2]][[l3]] )
      row2 <- c(l1, l2, l3, m.max.kostelec3c.thr[[l1]][[l2]][[l3]] )
      
      names(row) <- cn
      names(row2) <- cn
      if (create.tibble) {
        create.tibble <- FALSE
        tbl <- bind_rows(row)
        tbl2 <- bind_rows(row2)
        next
      } else {
        tbl %<>% add_row(bind_rows(row))
        tbl2 %<>% add_row(bind_rows(row2))
      }
      
      
      
      
      
      
    } 
    
    # png(paste0(path.igaD, "png/0-",paste(l1, l2),".png"),width = 400, height = 500)
    # par(mfrow= c(2, 1))
    # boxplot(m.max.kostelec1, main= paste(l1, l2), xlab= "třída kombinace prediktorů", ylab = "AUC")
    # boxplot(m.max.kostelec2, main= paste(l1, l2), xlab= "třída kombinace prediktorů (kumulativní)", ylab = "AUC")
    # dev.off()
    
    # png(paste0(path.igaD, "png/0-",paste(l1, l2),".png"),width = 400, height = 400)
    # 
    # boxplot(m.max.kostelec1, main= paste(l1, l2), xlab= "třída kombinace prediktorů", ylab = "AUC",
    #         ylim = c(0.5, 1))
    # 
    # dev.off()
    
  } 
  
  # png(paste0(path.igaD, "png/0-",paste(l1, l2),".png"),width = 400, height = 500)
  # par(mfrow= c(2, 1))
  # boxplot(m.max.kostelec1, main= paste(l1, l2), xlab= "třída kombinace prediktorů", ylab = "AUC")
  # boxplot(m.max.kostelec2, main= paste(l1, l2), xlab= "třída kombinace prediktorů (kumulativní)", ylab = "AUC")
  # dev.off()
  
}

# počet druhů které projdou tresholdem nad AUC 0.75 ve všech variantách
print(tbl)
# kumulativní
print(tbl2)

# saveRDS(tbl, file = paste0(path.igaD, "speciesCounts.rds"))
# saveRDS(tbl2, file = paste0(path.igaD, "speciesCounts-cumulative.rds"))
# write.csv(tbl, file = paste0(path.igaD, "speciesCounts.csv"))
# write.csv(tbl2, file = paste0(path.igaD, "speciesCounts-cumulative.csv"))

as.numeric(tbl2$spCountAuc75) -  as.numeric(tbl$spCountAuc75) 



png(paste0(path.igaD, "png/all-3x2.png"),width = 1200, height = 800)
par(mar=c(5, 5, 2.5, 2.5))
par(cex.lab=3) # is for y-axis

par(cex.axis=2) # is for x-axis
par(cex.main=2)

par(mfrow= c(2, 3))
boxplot(m.max.kostelec3[["all"]][["bio"]], ylim = c(0.5, 1), main= "Bioclim [8]", ylab = ">10 lokalit [118]")
boxplot(m.max.kostelec3[["all"]][["l8"]], ylim = c(0.5, 1), main= "Landsat 8 [15]")
boxplot(m.max.kostelec3[["all"]][["all"]], ylim = c(0.5, 1), main= "Bioclim+Landsat 8 [23]")


boxplot(m.max.kostelec3[["f30"]][["bio"]], ylim = c(0.5, 1), ylab = ">30 lokalit [74]")
boxplot(m.max.kostelec3[["f30"]][["l8"]], ylim = c(0.5, 1), xlab= "třída kombinace prediktorů")
boxplot(m.max.kostelec3[["f30"]][["all"]], ylim = c(0.5, 1))

dev.off()

# kumulativní
png(paste0(path.igaD, "png/all-3x2-cumulative.png"),width = 1200, height = 800)
par(mar=c(5, 5, 2.5, 2.5))
par(cex.lab=3) # is for y-axis

par(cex.axis=2) # is for x-axis
par(cex.main=2)

par(mfrow= c(2, 3))
boxplot(m.max.kostelec3c[["all"]][["bio"]], ylim = c(0.5, 1), main= "Bioclim [8]", ylab = ">10 lokalit [118]")
boxplot(m.max.kostelec3c[["all"]][["l8"]], ylim = c(0.5, 1), main= "Landsat 8 [15]")
boxplot(m.max.kostelec3c[["all"]][["all"]], ylim = c(0.5, 1), main= "Bioclim+Landsat 8 [23]")


boxplot(m.max.kostelec3c[["f30"]][["bio"]], ylim = c(0.5, 1), ylab = ">30 lokalit [74]")
boxplot(m.max.kostelec3c[["f30"]][["l8"]], ylim = c(0.5, 1), xlab= "třída kombinace prediktorů")
boxplot(m.max.kostelec3c[["f30"]][["all"]], ylim = c(0.5, 1))

dev.off()






##################################################################################
# SciCoffey
# Výsledky podle menšíhomnožství prediktorů omezených VIFem

raster_stack_100_na_23 <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/k2023/raster_stack_100_na_23.rds")

#writeRaster(raster_stack_100_na_23, "C:/Users/petr/Downloads/igaD/igaD/clean/k2023/raster_stack_100_na_23", "GTiff")


library(usdm)
ths <- c(3:6) * 0.1
vc.ths.n <- list()
vc.ths.p <- list()

for(th in ths){
  
  vc <- vifcor(raster_stack_100_na_23, th=th)
  vc.ths.n[[as.character(th)]] <-  vc@excluded
  vc.ths.p[[as.character(th)]] <- vc@results$Variables
  
  # vcPB <- usdmPB::vifcor(raster_stack_100_na_23, th=0.6)
  # saveRDS( vcPB, paste0(path.igaD, "vcPB.ths.n.RDS"))
  
}
vc.ths.n[["0.7"]] <- c()
vc.ths.p[["0.7"]] <- names(raster_stack_100_na_23)

#saveRDS(vc.ths.n, paste0(path.igaD, "vc.ths.n.RDS"))
#saveRDS(vc.ths.p, paste0(path.igaD, "vc.ths.p.RDS"))
vc.ths.n <- readRDS(paste0(path.igaD, "vc.ths.n.RDS"))
vc.ths.p <- readRDS(paste0(path.igaD, "vc.ths.p.RDS"))

vc.ths.n[["0.7"]] <- c("xxx")


m.v.vif <- list()

for (vf in names(vc.ths.n) ) {
for (f in names(m.f) ) {
  for (m.v.name in m.v.names) {
    for (k in 1:4) {
      print(vf)
      m.v.vif[[f]][[m.v.name]][[as.character(k)]][[vf]] <-
        m.v[[f]][[m.v.name]][[as.character(k)]] %>% filter(!str_detect(preds, paste(vc.ths.n[[vf]], collapse="|")))
     # m.v[[f]][[m.v.name]][[as.character(k)]] %>% filter(!grepl(vc.ths.n[[vf]],preds))
      
    }
  }
}
}







m.v.vif.s <- m.v.vif[["all"]][["all"]][[as.character(4)]]
# saveRDS(m.v.vif.s, paste0(path.igaD, "m.v.vif.s.RDS"))
# m.v.vif.s <- readRDS(paste0(path.igaD, "m.v.vif.s.RDS"))
# length(unlist(unique(m.v.vif.s$`0.7`$preds)))

# 
# median(m.v.vif.s$`0.7`$AUC)
# median(m.v.vif.s$`0.6`$AUC)
# median(m.v.vif.s$`0.3`$AUC)
# 
# mean(m.v.vif.s$`0.7`$AUC)
# mean(m.v.vif.s$`0.6`$AUC)
# mean(m.v.vif.s$`0.3`$AUC)
# 
# length(m.v.vif.s$`0.7`$AUC)
# length(m.v.vif.s$`0.6`$AUC)
# length(m.v.vif.s$`0.3`$AUC)
# 
# length(unique(m.v.vif.s$`0.7`$preds))
# length(unique(m.v.vif.s$`0.6`$preds))
# length(unique(m.v.vif.s$`0.3`$preds))



m.v.vif.max.l <- list()
m.v.vif.max <- list()
m.v.vif.max.median <- list()
for (vf in names(vc.ths.n) ) {

        m.v.vif.max[[vf]] <-
          m.v.vif.s[[vf]] %>% group_by(species) %>%  slice_max(AUC , n = 1, with_ties = FALSE) # summarise(auc.max = max(AUC))
        
        print(vf)
        print(length(unique( m.v.vif.max[[vf]]$preds)))
        
        m.v.vif.max.median[[vf]] <- mean(m.v.vif.max[[vf]]$AUC)
        m.v.vif.max.l[[vf]] <-  m.v.vif.max[[vf]]$AUC
}



### # Crutial role of predictors selection and muticolinearity
### # kumulativní nebo pevný počet prediktorů???? - obě varianty! - nechat vybrat?
### # Ukázat na Bio i L8 zvlášť a dohromady (nebo na NCEAS? - přidat vlastní prediktory - nebo jen nechat původní a s nimi kombinace)

m.v.vif.max.l.s <-m.v.vif.max.l[order(names(m.v.vif.max.l))]
boxplot(m.v.vif.max.l.s)

# tbl %>% group_by(species) %>% slice_max(aucs) %>% group_by(species) %>% arrange(species) 
# 
# ggplot(data = res,
#        mapping = aes(x = Habitat, y = auc_max, fill=Habitat)) + geom_boxplot(size = 1, outlier.size = 3, show.legend = FALSE) + 
#   theme_bw() + theme(text=element_text(size=30), 
#                      plot.caption = element_text(size=15, face = "italic"),
#                      plot.title = element_text(hjust = 0.5)) + 
#   labs(title = l.title.auc_max, caption = l.caption) +
#   xlab(l.habitat) + ylab(l.auc) +
#   scale_fill_manual(values=l.box_colors) +
#   scale_x_discrete(labels=l.habitat.type)




# varianty bio/l8/all //Habitat
# vybrat nejlepší modely ze všech variant a na nich udělat predikce, ty pak korelovat

###  
# how to diferentiate between noise and colinearity/correlation
# variogram, lisa, elsa, nuget

cor(k2023m$AUC, k2023m$Deviance)
cor(k2023m$AUC, k2023m$AUC.cv)
cor(k2023m$Deviance, k2023m$Deviance.cv)

# plot(k2023m$AUC, k2023m$Deviance)

boxplot(k2023m$Deviance)
boxplot(k2023m$AUC)
summary(k2023m)


auc <- list()
auc[[1]] <- c(0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85)
auc[[2]] <- c(0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65, 0.65)
auc[[3]] <- c(0.85, 0.85, 0.85, 0.85, 0.85, 0.65, 0.65, 0.65, 0.65, 0.65)
auc[[4]] <- c(0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
auc[[5]] <- c(0.05, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85, 0.85)
auc[[6]] <- c(0.05, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55)
auc[[7]] <- c(0.05, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55, 0.95)
auc[[8]] <- c(0.05, 0.05, 0.05, 0.65, 0.65, 0.65, 0.75, 0.75, 0.75, 0.95)
auc[[9]] <- c(0.05, 0.65, 0.65, 0.65, 0.65, 0.65, 0.75, 0.75, 0.75, 0.95)
lapply(auc, raster::cv)


