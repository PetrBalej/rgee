tempdir_delete <- tempdir()
# Sys.setenv(TMP = "D:\\PERSONAL_DATA\\tempR")
# Sys.setenv(TEMP = "D:\\PERSONAL_DATA\\tempR")

start_time <- Sys.time()

# nastavit working directory
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

path.igaD <- "/home/petr/Documents/igaD/"

# závisí na některých funkcích z:
source(paste0(getwd(), "/R/export_raster/functions.R"))

# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
  c("sp", "rgdal", "mapview", "raster", "geojsonio", "stars", "httpuv", "tidyverse", "sf", "lubridate", "magrittr", "dplyr", "readxl", "abind", "stringr", "sdmPB")
# "googledrive" # kontrola při použití v ee_Initialize?
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

"%notin%" <- Negate("%in%")

results_name <- "test-mx1"
# kolik druhů má být v jedné skupině
speciesPerGroup <- 2
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- 1



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

species.selected.prevalence <- unlist(lsdSampled.ss.prevalence$species)

lsdSampled.ss <- lsdSampled %>% filter(species %in% species.selected.prevalence) # 26406 na 17334
ndopSampled.ss <- ndopSampled %>% filter(species %in% species.selected.prevalence) # (118021?) 130654 na 87331





# ``

# library(sdm)
library(sdmPB)
# library(devtools)
# install_local("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/sdm/sdm", force = TRUE, build=TRUE)
# # install_github("PetrBalej/sdm", force = TRUE, ref="pb")


start_time <- Sys.time()
# , wtest="test.indep",
d <- list()
m <- list()
nbg <- 5000


pred.names.all <- names(lsdSampled)[-c(1, 2, 3, 25)]
pred.names.all.comb <- comb_all(pred.names.all, 3)


species.parts <- split(species.selected.prevalence, ceiling(seq_along(species.selected.prevalence) / speciesPerGroup))
results_name <- paste0(results_name, "-", species.part)




sigmas <- c(0.0, 0.001, 0.01, 0.03, 0.05, 0.08, 0.10, 0.12, 0.15, 0.20) * 1000 # c(0.001, seq(0.01, 0.2, by = 0.01))

for (sp in unname(unlist(species.parts[species.part]))) {
  print(sp)
  for (r in 1:3) { # !!!
    print(r)
    for (sigma in sigmas) {
      print(sigma)
      for (pred.names in pred.names.all.comb) {
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

        d[[sp.name]][[pred.names.f]][[as.character(r)]][[as.character(sigma)]] <- data.temp <- sdmPB::sdmData(frml, train = train, bg = bg, test = test)
        m[[sp.name]][[pred.names.f]][[as.character(r)]][[as.character(sigma)]] <- sdmPB::sdm(frml,
          data = data.temp, methods = c("maxent"), feat = c("linear", "quadratic", "product"),
          removeDuplicates = FALSE, c("removeDuplicates=FALSE"),
          parallelSettings = NULL
        )
      }
    }
  }
  gc()
  # unlink(paste0(tempdir_delete, "/*"), recursive = TRUE, force = TRUE)
}
end_time <- Sys.time()
print(end_time - start_time)

saveRDS(d, file = paste0(path.igaD, "clean/results/", results_name, "-d.rds"))
saveRDS(m, file = paste0(path.igaD, "clean/results/", results_name, "-m.rds"))

# str(m8)
# library(shinyBS)
# sdmPB::gui(m8)

# age <- readRDS("/mnt/2AA56BAE3BB1EC2E/Downloads/delete/2-Accipiter_gentilis---2maxnet-1-eval-1.rds")




#
# 
#

start_time <- Sys.time()
# načtení RDS jednotlivých druhů, spojení do jednoho listu
rds_list <-
  list.files(
    path = paste0(path.igaD, "ws2/results"),
    pattern = paste0("[A-z]+_[a-z]+.*-eval-[0-9]+\\.rds$"),
    ignore.case = TRUE,
    full.names = TRUE
  )


RDS <- readRDS(rds_list[1])
for (f in rds_list[-1]) {
  rds <- readRDS(f)
  sp <- names(rds)
  sps <- names(RDS)
  if (sp %in% sps) {
    for (pred in names(rds[[sp]])) {
      for (sigma in names(rds[[sp]][[pred]])) {
        # přidávání replikací
        RDS[[sp]][[pred]][[sigma]] <- append(RDS[[sp]][[pred]][[sigma]], rds[[sp]][[pred]][[sigma]])
      }
    }
  } else {
    RDS <- append(RDS, rds)
  }
}


evals <- RDS
evals.out <- list()
create.tibble <- TRUE
for (sp.name in names(evals)) {
  print(sp.name)
  for (predComb in names(evals[[sp.name]])) {
    for (sigma in names(evals[[sp.name]][[predComb]])) {
      for (replication in names(evals[[sp.name]][[predComb]][[sigma]])) {
        if (inherits(try(
          {

            # groups
            groups.names <- c("species", "predsComb", "sigma", "replication")

            # stats
            stats <- evals[[sp.name]][[predComb]][[sigma]][[replication]]@statistics
            stats.values <- unlist(stats)
            stats.names <- names(stats.values)

            # threshold_based
            tb <- evals[[sp.name]][[predComb]][[sigma]][[replication]]@threshold_based
            tb.tss <- tb[tb$criteria == "max(se+sp)", ]
            tb.tss.values <- tb.tss
            tb.tss.names <- names(tb.tss)

            # LSD prevalence
            test.obs <- evals[[sp.name]][[predComb]][[sigma]][[replication]]@observed
            test.prevalence.names <- "test.prevalence"
            test.prevalence.values <- length(test.obs[test.obs == 1]) / length(test.obs)


            all.names <- c(groups.names, stats.names, tb.tss.names, test.prevalence.names)
            all.values <- c(sp.name, predComb, sigma, replication, stats.values, tb.tss.values, test.prevalence.values)

            names(all.values) <- all.names
            if (create.tibble) {
              tbl <- bind_rows(all.values)
              create.tibble <- FALSE
              next
            } else {
              tbl %<>% add_row(bind_rows(all.values))
            }
          },
          silent = TRUE
        ), "try-error")) {
          # předpoklad je, že už předtím proběhl alespoň jeden kompletní cyklus bez chyby
          all.names <- c(groups.names, stats.names, tb.tss.names)
          all.values <- rep(NA, length(all.names))
          names(all.values) <- all.names
          tbl %<>% add_row(bind_rows(all.values))
        }
      }
    }
  }
}
# hodilo by se ještě přidat info o počtu NDOP+LSD presencí - jejich "prevalence"
tbl$species %<>% as.factor
tbl$predsComb %<>% as.factor
tbl$sigma %<>% as.integer
tbl$replication %<>% as.integer

print(tbl)

summary(tbl)
saveRDS(tbl, file = paste0(path.igaD, "ws2/tbl-49-1.rds"))
end_time <- Sys.time()
print(end_time - start_time)


# dát do tibble se sloupci jednotlivých úrovní - pak půjde zgroupovat a vyhodnotit

# minBAR - zjištění ale jen nad nebiasovaným LSD, jinak nedává smysl
# udělat analýtu rozsahů využití prediktorů pro LSD a NDOP