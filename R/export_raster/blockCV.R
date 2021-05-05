# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "blockCV", "ggplot2", "sdm", "dismo")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)
export_path <-
    paste0(getwd(), "/tmp2/")

source(paste0(getwd(), "/rgee/R/export_raster/functions.R"))
# source(paste0(getwd(), "/rgee/R/export_raster/gbif.R"))
# source(paste0(getwd(), "/rgee/R/export_raster/ndop_divland.R"))
# source(paste0(getwd(), "/rgee/R/export_raster/prepare_occurrences.R"))
source(paste0(getwd(), "/rgee/R/export_raster/ndop_top.R"))



blocks <- st_read(paste0(wd, "/rgee/shp/blocks.shp"))


czechia <- st_read(paste0(wd, "/rgee/shp/ne_10m_admin_0_countries/czechia/cz_3035.shp"))


ndop_top <- ndop_top(paste0(getwd(), "/rgee/species/ndop/ndop-top-2021-03-21.xlsx"))
# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
# species <- pull(ndop_top %>% select(species1, species2))


# raster_stack <-
#   rasters_dir_stack(
#     paste0(
#       "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/1000"
#     ),
#     "tif"
#   )

rasters_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/1000/"
vif5 <- c(
    "l8_3-5_1000_MNDWI",
    "l8_6-8_1000_EVI",
    "l8_6-8_1000_MNDWI",
    "l8_9-11_1000_B5",
    "l8_9-11_1000_EVI",
    "l8_9-11_1000_MNDWI",
    "wc_1000_bio02",
    "wc_1000_bio03",
    "wc_1000_bio09",
    "wc_1000_bio13",
    "wc_1000_bio15"
)


vif5sapply <- lapply(vif5, function(x, rasters_path) {
    return(paste0(rasters_path, x, ".tif"))
}, rasters_path = rasters_path)

raster_stack <- stack(lapply(vif5sapply, raster))






# plot_ndop_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/ndop_100.csv")
plot_gbif_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/gbif_100.csv")


cc <- plot_gbif_csv %>% filter(species == "Coccothraustes coccothraustes")

cc_3035 <- cc %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs(raster_stack))
#   %>%
#   st_coordinates() %>%
#   as_tibble()
# cc_3035 <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/schuzka2-total-gbif-ndop2/species/100_gbif_Coccothraustes_coccothraustes_1.csv") %>% st_as_sf(coords = c("longitude", "latitude"), crs = 3035) %>% st_transform(crs(raster_stack))

pa <- pseudo.absence(as_Spatial(cc_3035), n = 1000)
pa_sf <- st_as_sf(pa$sample)

# plot(c(cc = cc_3035$geometry, pa = pa_sf$geometry), col=c("red","blue"))
# plot(cc_3035$geometry, pa_sf$geometry)


# st_write(cc_3035$geometry, paste0(export_path, "cc-p.shp"))
# st_write(pa_sf$geometry, paste0(export_path, "cc-pa.shp"))


# sjednocení CRS
blocks_3035 <- blocks %>% st_transform(crs(raster_stack))

# dodatečný ořez nálezů podle BB
cci <- st_intersection(cc_3035, blocks_3035)
# st_write(st_intersection(cc_3035, blocks_3035), paste0(export_path, "cc-pa-i6.shp"))

t0 <- pa_sf %>%
    mutate(Species = 0) %>%
    select(-KDE)

t1 <- cci %>%
    mutate(Species = 1) %>%
    select(-key, -species, -id, -type)

pb_data <- bind_rows(t0, t1)

st_crs(pb_data) <- st_crs(raster_stack)


sb <- spatialBlock(
    speciesData = pb_data,
    species = "Species", # "Species" / NULL
    blocks = blocks_3035,
    foldsCol = "id", # oboje dát jedničku??? - pak postrádá smysl spatial cross-fold
    k = 2,
    rasterLayer = raster_stack,
    selection = "predefined"
)

# sb$plots + geom_sf(data = pb_data, alpha = 0.5)




# sb <- spatialBlock(speciesData = pa_data,
#                    species = "Species",
#                    rasterLayer = raster_stack,
#                    theRange = 70000, # size of the blocks
#                    k = 5,
#                    selection = "random",
#                    iteration = 100, # find evenly dispersed folds
#                    # biomod2Format = TRUE,
#                    xOffset = 0, # shift the blocks horizontally
#                    yOffset = 0)






# loading the libraries
library(maxnet)
library(precrec)
# library(ggplot2)

# extract the raster values for the species points as a dataframe

mydata <- raster::extract(raster_stack, pb_data)
mydata <- as.data.frame(mydata)
# create a vector of 1 (for presence) and 0 (for background samples)
pb <- pb_data$Species

# extract the folds in spatialBlock object created
# in the previous section (with presence-background data)
# the foldID only works for spatialBlock and envBlock folds
folds <- sb$foldID

# create an empty vector to store the AUC of each fold
AUCs <- vector(mode = "numeric")
for (k in seq_len(2)) {
    # extracting the training and testing indices
    # this way only works with foldID
    trainSet <- which(folds != k) # training set indices
    testSet <- which(folds == k) # testing set indices
    # fitting a maxent model using linear, quadratic and hinge features
    mx <- maxnet(
        p = pb[trainSet],
        data = mydata[trainSet, ],
        maxnet.formula(
            p = pb[trainSet],
            data = mydata[trainSet, ],
            classes = "default"
        )
    )
    testTable <- pb_data[testSet, ] # a table for testing predictions and reference data
    testTable$pred <- predict(mx, mydata[testSet, ], type = "cloglog") # predict the test set
    # calculate area under the ROC curve
    precrec_obj <- evalmod(scores = testTable$pred, labels = testTable$Species)
    AUCs[k] <- auc(precrec_obj)[1, 4] # extract AUC-ROC
}

print(mean(AUCs))
mxt <- maxent(x = raster_stack, p = as_Spatial(t1), a = NULL, factors = NULL, removeDuplicates = TRUE, nbg = 1000, args = "replicates=1")
pr <- predict(mxt, raster_stack)


raster_stack_crop <- crop(raster_stack, extent(czechia))
raster_stack_mask <- mask(raster_stack_crop, czechia)

writeRaster(raster_stack_mask, paste0(export_path, "raster_stack_crop.tif"), format = "GTiff", overwrite = TRUE)




###
# performance <- function(confusion.matrix) {
#     tp <- confusion.matrix[1, 1]
#     fp <- confusion.matrix[1, 2]
#     fn <- confusion.matrix[2, 1]
#     tn <- confusion.matrix[2, 2]
#     TPR <- tp / (tp + fn)
#     TNR <- tn / (tn + fp)
#     FPR <- fp / (fp + tn)
#     FNR <- fn / (fn + tp)
#     Sensitivity <- TPR
#     Specificity <- TNR
#     TSS <- Sensitivity + Specificity - 1
#     Jaccard <- TPR / (FNR + TPR + FPR)
#     Sorensen <- 2 * TPR / (FNR + 2 * TPR + FPR)
#     F_measure <- 2 * Jaccard
#     OPR <- fp / (tp + fp)
#     UPR <- 1 - Sensitivity
#     return(data.frame(TPR = TPR, TNR = TNR, FPR = FPR, FNR = FNR, Sensitivity = Sensitivity, Specificity = Specificity, TSS = TSS, Jaccard = Jaccard, Sorensen = Sorensen, F_measure = F_measure, OPR = OPR, UPR = UPR))
# }
# ###
# sdm.package.evaluation <- function(fit.model) {
#     th <- mean(getEvaluation(fit.model, stat = "threshold", opt = 2)[, 2])
#     # cm1 <- as.table(sdm:::.cmx(o = as.vector(fit.model@models$p = as.vector(ifelse(fit.model@cm2 <- as.table(sdm:::.cmx(o = as.vector(fit.model@models$p = as.vector(ifelse(fit.model@cm3 <- as.table(sdm:::.cmx(o = as.vector(fit.model@models$p = as.vector(ifelse(fit.model@cm4 <- as.table(sdm:::.cmx(o = as.vector(fit.model@models$p = as.vector(ifelse(fit.model@cm5 <- as.table(sdm:::.cmx(o = as.vector(fit.model@models$p = as.vector(ifelse(fit.model@eval <- getEvaluation(fit.model, stat= c("AUC", "Kappa"))
#     # perf <- rbind(data.frame(performance(data.frame(data.frame(TPR = mean(perf$TPR), TNR = mean(perf$TNR), FPR = mean(perf$FPR), FNR = mean(perf$FNR), Sensitivity = mean(perf$Sensitivity),Specificity = mean(perf$Specificity), AUC = mean(eval$AUC), Kappa = mean(eval$Kappa),TSS = mean(perf$TSS),Jaccard = mean(perf$Jaccard), Sorensen = mean(perf$Sorensen), F_measure = mean(perf$F_measure), OPR = mean(perf$OPR),UPR = mean(perf$UPR))
# }
# ###
# sdm.package.evaluation(m0)







performance <- function(confusion.matrix) {
    tp <- confusion.matrix[1, 1]
    fp <- confusion.matrix[1, 2]
    fn <- confusion.matrix[2, 1]
    tn <- confusion.matrix[2, 2]
    TPR <- tp / (tp + fn)
    TNR <- tn / (tn + fp)
    FPR <- fp / (fp + tn)
    FNR <- fn / (fn + tp)
    Sensitivity <- TPR
    Specificity <- TNR
    TSS <- Sensitivity + Specificity - 1
    Jaccard <- TPR / (FNR + TPR + FPR)
    Sorensen <- 2 * TPR / (FNR + 2 * TPR + FPR)
    F_measure <- 2 * Jaccard
    OPR <- fp / (tp + fp)
    UPR <- 1 - Sensitivity
    data.frame(
        TPR = TPR, TNR = TNR, FPR = FPR, FNR = FNR, Sensitivity = Sensitivity, Specificity = Specificity,
        TSS = TSS, Jaccard = Jaccard, Sorensen = Sorensen, F_measure = F_measure, OPR = OPR, UPR = UPR
    )
}




sdm.package.evaluation <- function(fit.model) {
    th <- mean(getEvaluation(fit.model, stat = "threshold", opt = 2)[, 2])

    cm1 <- as.table(sdm:::.cmx(
        o = as.vector(fit.model@models$occ$maxent$`1`@evaluation$test.dep@observed),
        p = as.vector(ifelse(fit.model@models$occ$maxent$`1`@evaluation$test.dep@predicted[] >= th, 1, 0))
    ))

    cm2 <- as.table(sdm:::.cmx(
        o = as.vector(fit.model@models$occ$maxent$`2`@evaluation$test.dep@observed),
        p = as.vector(ifelse(fit.model@models$occ$maxent$`2`@evaluation$test.dep@predicted[] >= th, 1, 0))
    ))

    cm3 <- as.table(sdm:::.cmx(
        o = as.vector(fit.model@models$occ$maxent$`3`@evaluation$test.dep@observed),
        p = as.vector(ifelse(fit.model@models$occ$maxent$`3`@evaluation$test.dep@predicted[] >= th, 1, 0))
    ))

    cm4 <- as.table(sdm:::.cmx(
        o = as.vector(fit.model@models$occ$maxent$`4`@evaluation$test.dep@observed),
        p = as.vector(ifelse(fit.model@models$occ$maxent$`4`@evaluation$test.dep@predicted[] >= th, 1, 0))
    ))

    cm5 <- as.table(sdm:::.cmx(
        o = as.vector(fit.model@models$occ$maxent$`5`@evaluation$test.dep@observed),
        p = as.vector(ifelse(fit.model@models$occ$maxent$`5`@evaluation$test.dep@predicted[] >= th, 1, 0))
    ))

    eval <- getEvaluation(fit.model, stat = c("AUC", "Kappa"))

    perf <- rbind(
        data.frame(performance(cm1)), (performance(cm2)), data.frame(performance(cm3)),
        data.frame(performance(cm4)), data.frame(performance(cm5))
    )

    return(data.frame(
        TPR = mean(perf$TPR), TNR = mean(perf$TNR), FPR = mean(perf$FPR), FNR = mean(perf$FNR), Sensitivity = mean(perf$Sensitivity),
        Specificity = mean(perf$Specificity), AUC = mean(eval$AUC), Kappa = mean(eval$Kappa), TSS = mean(perf$TSS),
        Jaccard = mean(perf$Jaccard), Sorensen = mean(perf$Sorensen), F_measure = mean(perf$F_measure), OPR = mean(perf$OPR),
        UPR = mean(perf$UPR)
    ))
}

# sdm.package.evaluation(m0)




# fuknce geog.overlap 
# raster.overlap z ENMtools místo jeho niche.overlap.geog?
niche.overlap.geog <- function(raster1, raster2, na.rm = TRUE) {
    pred1 <- as.data.frame(raster1)
    pred2 <- as.data.frame(raster2)
    p1 <- pred1 / sum(pred1, na.rm = na.rm)
    p2 <- pred2 / sum(pred2, na.rm = na.rm)
    SchoenerD <- 1 - 0.5 * sum(abs(p1 - p2), na.rm = na.rm)
    HellingerDist <- sqrt(sum((sqrt(p1) - sqrt(p2))^2, na.rm = na.rm))
    WarrenI <- 1 - ((HellingerDist^2) / 2)
    minus <- raster1 - raster2
    square <- minus^2
    mean <- cellStats(square, "mean")
    RMSE <- sqrt(mean)
    raster.stack <- stack(raster1, raster2)
    raster.matrix <- na.omit(as.matrix(raster.stack))
    cor <- as.data.frame(cor(raster.matrix, method = "spearman"))
    Correlation <- cor[1, 2]
    data.frame(SchoenerD = SchoenerD, WarrenI = WarrenI, HellingerDist = HellingerDist, RMSE = RMSE, SpearmanCorrelation = Correlation)
}


#  # niche envi.overlap -> Package ENM TOOLS
# env_stck <- setMinMax(Bioall)
# S6.Maxent_Envi.Overlap <- env.overlap(model.train, S6.model.train, env_stck, tolerance= 0.01)