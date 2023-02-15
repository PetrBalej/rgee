# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "stringr", "sdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)




# wd <- "D:/PERSONAL_DATA/pb/rgee"
# wd <- "C:/Users/petr/Downloads/igaD/rgeeDP/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"
setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

# path.igaD <- "C:/Users/petr/Downloads/igaD/igaD/clean/k2023/"
path.igaD <- "/home/petr/Documents/igaD/k2023/"

# připojit - pro případ omezení 30-132 (ale jen 64 druhů)
spstat <- readRDS(paste0(wd, "/R/kostelec2023/NB/clean/lsd-PA-162sq-198sp118.rds"))




# scénáře a vliv na výsledky:
# 1) pevně dané prediktory vifcor 0.7 (biomean/cv) x landsat(mean/cv)) - musím dodělat vifcor=0.7 (zvlášť) a vybrat jen dotyčné prediktory!
# 2a) nesmysl omezevat velkým vifem (nechat vyšší korelaci, nevím na co druh a z kterých oblastí zabírá)
# 2b) nechat vybrat nejlepší kombinaci


# udělat i glmnet!!! (je až od 2 prediktorů v sdm library)
# GLM - je transparentní, žádné optimalizace ani magie - jen medián a kombinace prediktorů (3 fold cross-validation + 5 replication)



#  variance inflation factor (VIF) to avoid over-fitting !!! - najít citaca Araujo Standards
# Jak dokážu, že jsem nezpůsobil overfitting volbou korelovaných proměnných???

# Spatial filtering to reduce sampling bias can improve the performance of ecological niche models:
# To quantify overfitting and model performance, we calculated evaluation AUC, the difference between calibration and evaluation AUC (=AUCdiff), and omission rates.



# chybně jsem přidal názv y sloupců jen k "1-*.csv" - musím je načíst samostatně se záhlavím a pak zvlášť ostatní bez záhlaví
csv.first <- list.files(paste0(path.igaD, "../k2023-23"), ".*test\\.csv$", full.names = TRUE)

ct <- list(
  "modelID" = "i",
  "AUC" = "d",
  "COR" = "d",
  "Deviance" = "d",
  "Prevalence" = "d",
  "threshold" = "d",
  "sensitivity" = "d",
  "specificity" = "d",
  "TSS" = "d",
  "Kappa" = "d",
  "NMI" = "d",
  "phi" = "d",
  "ppv" = "d",
  "npv" = "d",
  "ccr" = "d",
  "prevalence" = "d",
  "species" = "f",
  "method" = "c",
  "replication" = "c",
  "replicationID" = "i",
  "success" = "l",
  "training" = "l",
  "test.dep" = "l",
  "test.indep" = "l",
  "preds" = "c"
)

first <- TRUE
for (ff in csv.first) {
  if (first) {
    first <- FALSE
    csv.out <- read_csv(
      ff,
      col_names = TRUE,
      col_types = ct
    )
  } else {
    csv.out %<>% add_row(read_csv(
      ff,
      col_names = TRUE,
      col_types = ct
    ))
  }
}


### 2x 24...
### nemám 26...

# csv.out$species %<>% as.factor
# csv.out$preds %<>% as.factor

# summary(csv.out)
# bylo by vhodné připojit i počet presencí z LSD 162...

# csv.out.t <- csv.out
# csv.out <- csv.out.t



saveRDS(csv.out, paste0(path.igaD, "k2023-all.rds"))
# write.csv(csv.out, paste0(path.igaD, "k2023-all.csv"), row.names=FALSE) # skoro 5 GB!!!

# unique(csv.out %>% dplyr::select(preds))

csv.out.selected <- csv.out %>% dplyr::select(AUC, COR, Deviance, Prevalence, sensitivity, specificity, TSS, prevalence, species, preds)
# saveRDS(csv.out.selected, paste0(path.igaD, "k2023-selected.rds"))


# zprůměrování a
# výběr nej kombinace

# 18,678,720 modelů (temp)
# summary(csv.out.selected)


csv.out.selected.g <- csv.out.selected %>% group_by(species, preds)



# mediány
csv.out.selected.g.tbl <- csv.out.selected.g %>%
  summarise(across(everything(), median),
    .groups = "drop"
  )

# CV
csv.out.selected.g.tbl.sd <- csv.out.selected.g %>%
  summarise(across(everything(), raster::cv),
    .groups = "drop"
  )



# unique(csv.out.selected.g.tbl %>% dplyr::select(preds))


# připravit filtry: 1) podle počtu prediktorů, 2) bio/l8/cv/mean a jejich kombinace
# počet; bio (01);  l8(01); počet_bio; počet_l8 - umožní dofiltrovat všechny situace
# + index/raw (l8)

# binární příznaky
csv.out.selected.g.tbl %<>% mutate(l.bio = ifelse(str_detect(preds, "wc_"), 1, 0)) %>%
  mutate(l.l8 = ifelse(str_detect(preds, "l8_"), 1, 0)) %>%
  mutate(l.cv = ifelse(str_detect(preds, "_cv_"), 1, 0)) %>%
  mutate(l.mean = ifelse(str_detect(preds, "_mean_"), 1, 0)) %>%
  mutate(l.raw = ifelse(str_detect(preds, "_raw_"), 1, 0)) %>%
  mutate(preds.n = str_count(preds, "\\+") + 1) # počet prediktorů
# # sporné, různé měsíce mohou být mezi sebou v určitých situacích schovány za jiný korelovaný měsíc
# mutate(l.month4 = ifelse(str_detect(preds, "l8_4"), 1, 0)) %>%
# mutate(l.month5 = ifelse(str_detect(preds, "l8_5"), 1, 0)) %>%
# mutate(l.month5 = ifelse(str_detect(preds, "l8_6"), 1, 0))

# indexů je hodně, zatím nevyjmenovávat



csv.out.selected.g.tbl$l.bio %<>% as.logical
csv.out.selected.g.tbl$l.l8 %<>% as.logical
csv.out.selected.g.tbl$l.cv %<>% as.logical
csv.out.selected.g.tbl$l.mean %<>% as.logical
csv.out.selected.g.tbl$l.raw %<>% as.logical
csv.out.selected.g.tbl$preds.n %<>% as.integer
csv.out.selected.g.tbl$preds %<>% as.factor




# filtrovat podle CV? - pokud bude velký, znamená to nestabilní výpočet AUC a dalších
# oveřit vztah mezi CV a počtem prediktorů (a počtu presencí podle druhů)
csv.out.selected.g.tbl.j <- csv.out.selected.g.tbl %>% left_join(csv.out.selected.g.tbl.sd, by = c("species", "preds"), suffix = c("", ".cv"))



# 1,245,249 (temp) kombinací druh a prediktor
# saveRDS(csv.out.selected.g.tbl, paste0(path.igaD, "k2023-selected-mean.rds"))

saveRDS(csv.out.selected.g.tbl.j, paste0(path.igaD, "k2023-selected-median.rds"))
write.csv(csv.out.selected.g.tbl.j, paste0(path.igaD, "k2023-selected-median.csv"), row.names = FALSE)

summary(csv.out.selected.g.tbl.j)

unique((csv.out.selected.g.tbl %>% filter(AUC >= 0.75))$species) # zatím 84 (mean) / 91 (median) ze 118


# 4kombinace už nepřináší žádné nové druhy nad 0.75AUC (jen +1)

# počet druhů (118)
unique(csv.out.selected.g.tbl$species)


# počet unikátních kombinací prediktorů (10902)
unique(csv.out.selected.g.tbl.j$preds.n)












# stejně udělat mediány i pro RF!!!
rf <- "/home/petr/Documents/igaD/k2023/rf"




# skript na predikce z vybranými prediktory (všechny druhy dohromady)!




df <- readRDS(paste0(wd, "/R/kostelec2023/NB/clean/lsd-PA-FINAL-57preds-118sp.df.rds"))
set.seed(85)
df <- df[sample(1:nrow(df)), ]
rownames(df) <- NULL


remove <- readRDS(paste0(wd, "/R/kostelec2023/NB/k2023/preds07-remove.rds"))[-1] # jedna záměna ve VIFu
# df <- df[, !(names(df) %in% remove)]
# pred.names.all <- names(df)[1:23]
# saveRDS(pred.names.all, paste0(path.igaD, "preds.23.rds"))











#
# znovuodvozené 23 prediktorů
#

# raster_stack_na <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/predictors/raster_stack_100_na.rds")
raster_stack_na <- stack("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/kostelec2023/NB/clean/raster_stack_100_na.grd")

# raster_stack_na.vifcor <- readRDS("C:/Users/petr/Downloads/igaD/igaD/clean/clean/vif/lsd-PA-162sq-198sp_bioclim-landsat8-cv-mean-100_vifcor.rds")
raster_stack_na.vifcor <- readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/kostelec2023/NB/clean/lsd-PA-162sq-198sp_bioclim-landsat8-cv-mean-100_vifcor.rds")

remove <- readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/kostelec2023/NB/k2023/preds07-remove.rds")[-1]

raster_stack_na.f23 <- dropLayer(raster_stack_na[[raster_stack_na.vifcor@results$Variables]], remove)
# names(raster_stack_na.f23)

# + propíše NA hodnoty napříč layery
raster_stack_na.f23 <- raster::mask(raster_stack_na.f23, sum(raster_stack_na.f23))
# znovu určí minMax hodnoty
raster_stack_na.f23 <- raster::setMinMax(raster_stack_na.f23)


# rr <- writeRaster(raster_stack_na.f23, "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/kostelec2023/NB/clean/raster_stack_100_na_23.grd", format = "raster", overwrite = TRUE)
# hdr(rr, format = "ENVI")
# saveRDS(raster_stack_na.f23, "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/kostelec2023/NB/clean/raster_stack_100_na_23.rds")






# SDM + predikce vybraných
p1 <- predict(m, newdata = raster_stack_na.th09, filename = "kostelec23-test.png")
plot(p1)