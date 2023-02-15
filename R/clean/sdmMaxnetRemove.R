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
} else {
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
  c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "readxl", "abind", "stringr", "sdm", "maxnet", "SDMtune")
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





results_name <- "0maxnetRemove"
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
replicates <- 3

pred.names.all <- names(lsdSampled)[-c(1, 2, 3, 25)]
# pred.names.all.comb <- comb_all(pred.names.all, 21)
pred.names.all.comb <- list("1" = pred.names.all)




species.parts <- split(species.selected.prevalence, ceiling(seq_along(species.selected.prevalence) / speciesPerGroup))
results_name <- paste0(results_name, "-", species.part)





# rekurzivní variable permutation importance s postupným odebíráním nejméně přispívajícího prediktoru, jen záloha, není to vhodná metoda...  + bias fitting
permImp_remove_last2 <- function(sp.name, ndopSampled.ss, lsdSampled.ss, bgSampled, pred.names, sigma, replicates, beta, nbg, path.igaD, data = NA) {
  sp <- sp.name

  for (r in 1:replicates) {
    print("repl:")
    print(r)
    train <- as.data.frame(ndopSampled.ss %>% filter(species == sp)) %>%
      dplyr::select(-c(POLE, geometry, species)) %>%
      dplyr::select(all_of(pred.names))



    sp.name <- str_replace(sp, " ", "_")
    train[[sp.name]] <- 1
    # frml <- as.formula(paste0(sp.name, "~", pred.names.f))

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



    pred.names.f <- paste(pred.names, collapse = "+")


    if (inherits(try(
      # , regmult = 1
      m <- maxnet(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], f = maxnet.formula(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], classes = "lqp"), regmult = beta)
    ), "try-error")) {
      print("1error")
      if (inherits(try(
        m <- maxnet(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], f = maxnet.formula(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], classes = "lq"), regmult = beta)
      ), "try-error")) {
        print("2error")
        m <- maxnet(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], f = maxnet.formula(p = trainAll[, ncol(trainAll)], data = trainAll[, -ncol(trainAll)], classes = "l"), regmult = beta)
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


    return(list("m" = m, "e" = e))
    stop()

    model.temp[[sp.name]][[pred.names.f]][[as.character(sigma)]][[as.character(r)]] <- m

    eval.temp[[sp.name]][[pred.names.f]][[as.character(sigma)]][[as.character(r)]] <- e
  }


  return(list("m" = m, "e" = e))


  ################################################################################################################### vv


  if (!is.list(data)) {
    data <- list()
  }
  n_layers <- nlayers(env.sentinel_bio)
  print(paste0("Počet použitých prediktorů/layerů:", n_layers))
  print(names(env.sentinel_bio))
  if (n_layers > 1) {
    n_layers <- as.character(n_layers)
    # m <- list()
    # for (r in 1:replicates) {
    #   m[[r]] <- ENMToolsPB::enmtools.glm(
    #     species = species.selected, env = env.sentinel_bio,
    #     test.prop = "checkerboard2",
    #     # nback = 10000,
    #     # bias = bias_czechia,
    #     bg.source = "points",
    #     corner = r,
    #     verbose = TRUE
    #   )
    # }

    # hledám bias raster s nejlepším AUC [start]
    ba <- list()
    ba.m <- list()
    for (adj in 0:9) {
      bias_czechia <- raster(paste0(path.igaD, "bias-ptaci-adj-0.", as.character(adj), "-kfme16.tif"))
      m <- list()
      for (r in 1:replicates) {
        m[[r]] <- ENMToolsPB::enmtools.glm(
          species.selected.ndop,
          eval = TRUE,
          env.sentinel_bio,
          test.prop = "checkerboard2",
          bg.source = "range",
          verbose = TRUE,
          bias = bias_czechia,
          nback = 10000,
          corner = ifelse(r == 4, r, NA),
          speciesInd = species.selected,
          nbackInd = species.selected,
          envInd = env.sentinel_bio
        )
      }


      ba[[as.character(adj)]] <- m.auc.sentinel_bio <- median(sapply(m, function(x) x$test.evaluation@auc))
      ba.m[[as.character(adj)]] <- m
    }

    data[[n_layers]]$adj.selected <- adj.selected <- names(base::which.max(unlist(ba)))

    m <- ba.m[[adj.selected]]

    # hledám bias raster s nejlepším AUC [end]



    data[[n_layers]]$auc <- median(sapply(m, function(x) x$test.evaluation@auc))



    cm <- lapply(m, function(x) performance(x$conf))
    cm.matrix <- abind(cm, along = 3)
    cm.perf <- apply(cm.matrix, c(1, 2), median)
    cm.perf.t <- as_tibble(cm.perf)

    data[[n_layers]] <- append(data[[n_layers]], as.list(cm.perf.t))


    enm_mxt_gbif.vip <- sapply(
      m, enmtools.vip # , nsim = 100
    )
    enm_mxt_gbif.vip.t <- lapply(enm_mxt_gbif.vip[seq(1, replicates * 2, 2)], function(x) {
      as_tibble(as.data.frame(t(as.matrix(unlist(purrr::transpose(x[, 2], x$Variable))))))
    })

    if (replicates == 1) {
      enm_mxt_gbif.vip.s <- enm_mxt_gbif.vip.t[[1]]
    } else {
      b_g <- enm_mxt_gbif.vip.t[[1]]

      for (n in 1:replicates) {
        if (n > 1) {
          b_g %<>% add_row(enm_mxt_gbif.vip.t[[n]])
        }
      }
      enm_mxt_gbif.vip.s <- b_g %>%
        summarise_if(is.numeric, mean, na.rm = TRUE)
    }

    data[[n_layers]]$permImp <- enm_mxt_gbif.vip.s


    # odstraním nejhorší prediktor
    predictors.all <- as_tibble(cbind(nms = names(enm_mxt_gbif.vip.s), t(enm_mxt_gbif.vip.s))) %>%
      mutate(across(V2, as.numeric)) %>%
      arrange(V2)

    predictor.bad <- predictors.all %>% slice_min(V2, n = 1)
    rs_names <- names(env.sentinel_bio)

    rs_ex <- match(substr(predictor.bad$nms, 1, nchar(predictor.bad$nms) - 11), rs_names) # odstraním příponu .Importance
    env.sentinel_bio.minus1 <- dropLayer(env.sentinel_bio, rs_ex)

    data[[n_layers]]$bad.name <- predictor.bad$nms
    data[[n_layers]]$bad.value <- predictor.bad$V2
    if (n_layers == 2) {
      print("end")
      return(data)
    } else {
      print("continue")
      return(permImp_remove_last(species.selected, species.selected.ndop, env.sentinel_bio.minus1, path.igaD, replicates, data))
    }
  } else {
    return(NA)
  }
}








sigmas <- c(0.0, 0.001, 0.01, 0.03, 0.05, 0.08, 0.10, 0.12, 0.15, 0.20) * 1000 # c(0.0, 0.001, 0.01, 0.03, 0.05, 0.08, 0.10, 0.12, 0.15, 0.20) * 1000 # c(0.001, seq(0.01, 0.2, by = 0.01))

betas <- c(0.1, 0.5, 1.00, 2.00, 5.00, 10.00)
data <- list()
for (sp in unname(unlist(species.parts[species.part]))) {
  print("********************************************************************************************")
  print(sp)
  sp.name <- str_replace(sp, " ", "_")
  model.temp <- list()
  eval.temp <- list()


  for (sigma in sigmas) {
    print("sigma +++")
    print(sigma)
    for (beta in betas) {
      print(beta)
      # pred.names.all.comb




      pred.names.n <- length(pred.names)

      pred.names.n.vi <- comb_k(pred.names.all, pred.names.n - 1)


      # 1) nejprve AUC pro pred.names
      # 2) pak AUC pro pred.names.n.vi
      # 3) vybrat z 2) nejlepší kombinaci (nejméně ztratila odstraněním nějakého rediktoru)
      # 4) pokračovat s 3) (rekurze)





      data[[sp.name]][[as.character(sigma)]][[as.character(beta)]] <- permImp_remove_last2(sp, ndopSampled.ss, lsdSampled.ss, bgSampled, pred.names.all, sigma, replicates, beta, nbg, path.igaD, data = NA)
      # xxxxxxxxxxxxxxxx



      if (pred.names.n == 2) {
        # ukončit a vybrat nej variantu se dvěma prediktory
        print("********************************************************")
        print("vrátit nej 2kombinaci!!!")
      }









      # str(data$Accipiter_gentilis, max.level=12)
      # result <- SDMmodel()
      #   model_object <- Maxnet(reg = 1, fc = "lqp", model = data[["Accipiter_gentilis"]][[as.character(sigma)]][[as.character(beta)]][["m"]])
      #   result@model <- model_object
      # vi <- varImp(result, permut = 5)

      ###
      # udělat comb_all pro 20+21 (odstranit <20) a pak vybrat nej AUC, a pokračovat se sníženým datasetem prediktorů dokud se AUC nesníží
      ###
    }
  }


  # saveRDS(eval.temp, file = paste0(path.igaD, "clean/results/2-", sp.name, "---", results_name, "-eval-", as.character(r), ".rds"))
  # saveRDS(model.temp, file = paste0(path.igaD, "clean/results/2-", sp.name, "---", results_name, "-model-", as.character(r), ".rds"))
  gc()
  # unlink(paste0(tempdir_delete, "/*"), recursive = TRUE, force = TRUE)
}

end_time <- Sys.time()
print(end_time - start_time)
print(cmd_arg)


data[["Accipiter_gentilis"]][[as.character(sigma)]][[as.character(beta)]][["m"]]

vi <- varImp(data[["Accipiter_gentilis"]][[as.character(sigma)]][[as.character(beta)]][["m"]], permut = 5)