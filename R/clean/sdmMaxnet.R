tempdir_delete <- tempdir()
# Sys.setenv(TMP = "D:\\PERSONAL_DATA\\tempR")
# Sys.setenv(TEMP = "D:\\PERSONAL_DATA\\tempR")

start_time <- Sys.time()

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

# "C:/Program Files/R/R-4.2.1/bin/x64/Rscript.exe" "D:/PersonalWork/Balej/sdm/rgee/R/clean/sdmMaxnet.R" 132
# nastavit working directory
# wd <- "D:/PersonalWork/Balej/sdm/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

# path.igaD <- "D:/PersonalWork/Balej/sdm/"
path.igaD <- "/home/petr/Documents/igaD/"

# závisí na některých funkcích z:
source(paste0(getwd(), "/R/export_raster/functions.R"))

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "readxl", "abind", "stringr", "sdm", "maxnet")
# "googledrive" # kontrola při použití v ee_Initialize?
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

"%notin%" <- Negate("%in%")

# # nutný hack, zruší kontrolu duplikátů (všude), jinak odstraňuje duplicitní BG, které potřebuji
# #----------
# # remove duplicate records, and the rows that species columns contin NA OR all (or any) columns contain NA
# .dataClean <- function(x, nsp, ANY = TRUE) {
#   list(x, c(na = 0, duplicate = 0))
# }
# assignInNamespace(".dataClean", value = .dataClean, ns = "sdm")





results_name <- "2maxnet"
# kolik druhů má být v jedné skupině
speciesPerGroup <- 3
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- cmd_arg # 1



bgSampled <- readRDS(paste0(path.igaD, "clean/bias/bgSampled-variants.rds"))
# str(bgSampled, max.level=2)
ndopSampled <- readRDS(paste0(path.igaD, "clean/occurrences/ndopSampled_syn.rds"))
lsdSampled <- readRDS(paste0(path.igaD, "clean/occurrences/lsdSampled_syn.rds"))






### Jen hnízdící druhy: http://fkcso.cz/fk/ptacicr.html
fkcso_csv <- read_csv(paste0(wd, "/species/ndop/ptaci-hnizdeni-do-2020-fkcso.csv"))
fkcso <- fkcso_csv %>%
  filter(cat == "A") %>%
  filter(grepl("h,|H,", type))

fkcso <- synonyms_unite(fkcso)


lsdSampledUnique <- data.frame("species" = NA)
lsdSampledUnique <- data.frame(species = unique(lsdSampled$species))

species.selected <- lsdSampledUnique %>%
  filter(species %in% fkcso$species) %>%
  filter(species %notin% nepuvodni_problematicke()$nepuvodni) %>%
  filter(species %notin% nepuvodni_problematicke()$problematicke) %>%
  filter(species %notin% nepuvodni_problematicke()$nevhodne) %>%
  arrange(species) %>%
  dplyr::select(species)

species.selected <- as.vector(unlist(unique(species.selected)))


lsdSampled.ss <- lsdSampled %>% filter(species %in% species.selected) # 26406
ndopSampled.ss <- ndopSampled %>% filter(species %in% species.selected) # (118021?) 130654 na 87331



# počet POLE na species
lsdSampled.ss.prevalence <- lsdSampled.ss %>%
  filter(presence == 1) %>%
  group_by(species) %>%
  summarise(count = n_distinct(POLE)) %>%
  arrange(desc(count)) %>%
  filter(count >= 10 & count <= 150)

species.selected.prevalence <- sort(unlist(lsdSampled.ss.prevalence$species)) # seřadím dle abecedy, ne podle počtu nálezů, ať jsou skupiny různé

lsdSampled.ss <- lsdSampled %>% filter(species %in% species.selected.prevalence) # 26406 na 17334
ndopSampled.ss <- ndopSampled %>% filter(species %in% species.selected.prevalence) # (118021?) 130654 na 87331





# ``

# library(sdm)
# library(sdmPB)
# library(maxnet)
# library(devtools)
# install_local("D:/PersonalWork/Balej/sdm/sdm/sdm", force = TRUE, build=TRUE)
# # install_github("PetrBalej/sdm", force = TRUE, ref="pbAllowDuplicates")
# # install_github("cran/sdm", force = TRUE)

start_time <- Sys.time()
# , wtest="test.indep",
d <- list()
m <- list()
e <- list()
nbg <- 5000


pred.names.all <- names(lsdSampled)[-c(1, 2, 3, 25)]
# pred.names.all.comb <- comb_all(pred.names.all, 21)
pred.names.all.comb  <- list("1" = pred.names.all)

species.parts <- split(species.selected.prevalence, ceiling(seq_along(species.selected.prevalence) / speciesPerGroup))
results_name <- paste0(results_name, "-", species.part)




sigmas <- c(0.0, 0.001, 0.01, 0.03, 0.05, 0.08, 0.10, 0.12, 0.15, 0.20) * 1000 # c(0.0, 0.001, 0.01, 0.03, 0.05, 0.08, 0.10, 0.12, 0.15, 0.20) * 1000 # c(0.001, seq(0.01, 0.2, by = 0.01))
for (r in 1:3) { # !!!
  print("replikace -------------------------------------------------------")
  print(r)
  for (sp in unname(unlist(species.parts[species.part]))) {
    print("********************************************************************************************")
    print(sp)

    model.temp <- list()
    eval.temp <- list()


    for (sigma in sigmas) {
      print("sigma +++")
      print(sigma)
      for (pred.names in pred.names.all.comb) { #-c(1:21)   [-c(1:21)]  pred.names.all.comb[-c(1:21)]
        print(pred.names)
        train <- as.data.frame(ndopSampled.ss %>% filter(species == sp)) %>%
          dplyr::select(-c(POLE, geometry, species)) %>%
          dplyr::select(all_of(pred.names))


        pred.names.f <- paste(pred.names, collapse = "+")
        sp.name <- str_replace(sp, " ", "_")
        train[[sp.name]] <- 1
        frml <- as.formula(paste0(sp.name, "~", pred.names.f))

        test <- as.data.frame(lsdSampled.ss %>% filter(species == sp)) %>%
          dplyr::select(-c(POLE, geometry, species)) %>%
          dplyr::select(all_of(pred.names), presence)
        names(test)[names(test) == "presence"] <- sp.name

        if (sigma == 0) {
          bg <- bgSampled$random_na_noLSD[[as.character(nbg)]][[as.character(r)]]
        } else {
          bg <- bgSampled$bias_na_noLSD[[as.character(nbg)]][[as.character(r)]][[as.character(sigma)]]
        }

        bg <- as.data.frame(bg) %>% dplyr::select(all_of(pred.names))

        bg[[sp.name]] <- 0



        trainAll <- rbind(train, bg)


        if (inherits(try(
          # , regmult = 1
          m <- maxnet(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], f = maxnet.formula(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], classes = "lqp"))
        ), "try-error")) {
          print("1error")
          if (inherits(try(
            m <- maxnet(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], f = maxnet.formula(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], classes = "lq"))
          ), "try-error")) {
            print("2error")
            m <- maxnet(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], f = maxnet.formula(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], classes = "l"))
          }
        }

        p <- predict(m, test[, -ncol(test)], type = "cloglog")



        if (inherits(try(
          e <- evaluates(test[, ncol(test)], as.vector(p))
        ), "try-error")) {
          print("E3error")
          # Error in data.frame(criteria = th.criteria, th) :
          #  arguments imply differing number of rows: 10, 0
          e <- NA
        }



        model.temp[[sp.name]][[pred.names.f]][[as.character(sigma)]][[as.character(r)]] <- m

        eval.temp[[sp.name]][[pred.names.f]][[as.character(sigma)]][[as.character(r)]] <- e
      }
    }


    saveRDS(eval.temp, file = paste0(path.igaD, "clean/results/2-", sp.name, "---", results_name, "-eval-", as.character(r), ".rds"))
    saveRDS(model.temp, file = paste0(path.igaD, "clean/results/2-", sp.name, "---", results_name, "-model-", as.character(r), ".rds"))
    gc()
    # unlink(paste0(tempdir_delete, "/*"), recursive = TRUE, force = TRUE)
  }
}
end_time <- Sys.time()
print(end_time - start_time)
print(cmd_arg)