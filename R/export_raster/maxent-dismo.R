options(java.parameters = c("-Xmx20g"))
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "blockCV", "ggplot2", "dismo", "ENMTools", "data.table", "rmaxent")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)
export_path <-
    paste0(getwd(), "/tmp3/")

source(paste0(getwd(), "/rgee/R/export_raster/functions.R"))
# source(paste0(getwd(), "/rgee/R/export_raster/gbif.R"))
# source(paste0(getwd(), "/rgee/R/export_raster/ndop_divland.R"))
# source(paste0(getwd(), "/rgee/R/export_raster/prepare_occurrences.R"))
source(paste0(getwd(), "/rgee/R/export_raster/ndop_top.R"))


# Youden’s index == the True Skill Statistic.



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

performanceC <- function(ev) {
    TPR <- ev@TPR
    TNR <- ev@TNR
    FPR <- ev@FPR
    FNR <- ev@FNR
    Sensitivity <- TPR
    Specificity <- TNR
    TSS <- Sensitivity + Specificity - 1
    Jaccard <- TPR / (FNR + TPR + FPR)
    Sorensen <- 2 * TPR / (FNR + 2 * TPR + FPR)
    F_measure <- 2 * Jaccard

    data.frame(
        TPR = mean(TPR), TNR = mean(TNR), FPR = mean(FPR), FNR = mean(FNR), Sensitivity = mean(Sensitivity), Specificity = mean(Specificity),
        TSS = mean(TSS), Jaccard = mean(Jaccard), Sorensen = mean(Sorensen), F_measure = mean(F_measure)
    )
}

# performanceR <- function(confusion.matrix) {
#     tp <- confusion.matrix[, 1]
#     fp <- confusion.matrix[, 2]
#     fn <- confusion.matrix[, 3]
#     tn <- confusion.matrix[, 4]
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
#     data.frame(
#         TPR = mean(TPR), TNR = mean(TNR), FPR = mean(FPR), FNR = mean(FNR), Sensitivity = mean(Sensitivity), Specificity = mean(Specificity),
#         TSS = mean(TSS), Jaccard = mean(Jaccard), Sorensen = mean(Sorensen), F_measure = mean(F_measure)
#     )
# }

sdm.package.evaluation <- function(fit.model) {
    th <- mean(sdm::getEvaluation(fit.model, stat = "threshold", opt = 2)[, 2])

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

    eval <- sdm::getEvaluation(fit.model, stat = c("AUC", "Kappa"))

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
niche.overlap.geog <- function(raster1, raster2, na.rm = TRUE, suffix = "1") {
    pred1 <- as.data.frame(raster1)
    pred2 <- as.data.frame(raster2)
    p1 <- pred1 / sum(pred1, na.rm = na.rm)
    p2 <- pred2 / sum(pred2, na.rm = na.rm)
    SchoenerD <- 1 - 0.5 * sum(abs(p1 - p2), na.rm = na.rm)
    HellingerDist <- sqrt(sum((sqrt(p1) - sqrt(p2))^2, na.rm = na.rm))
    WarrenI <- 1 - ((HellingerDist^2) / 2)
    minus <- raster1 - raster2
    square <- minus^2
    mean <- raster::cellStats(square, "mean")
    RMSE <- sqrt(mean)
    raster.stack <- stack(raster1, raster2)
    raster.matrix <- na.omit(as.matrix(raster.stack))
    cor <- as.data.frame(cor(raster.matrix, method = "spearman"))
    Correlation <- cor[1, 2]
    DF <- data.frame(SchoenerD = SchoenerD, WarrenI = WarrenI, HellingerDist = HellingerDist, RMSE = RMSE, SpearmanCorrelation = Correlation)

    colnames(DF) <- paste(colnames(DF), suffix, sep = "_")
    return(DF)
}


# Schoener’s D ranges between 0 and 1 and provides a measure of the similarity of two modeling outputs in the geographic space.










blocks <- st_read(paste0(wd, "/rgee/shp/blocks.shp"))
blocks_erased_cz <- st_read(paste0(wd, "/rgee/shp/blocks_erased_cz.shp"))

czechia <- st_read(paste0(wd, "/rgee/shp/ne_10m_admin_0_countries/czechia/cz_3035.shp"))


ndop_top <- ndop_top(paste0(getwd(), "/rgee/species/ndop/ndop-top-2021-03-21.xlsx"))
# pouze průnik NDOP a GBIF (totožné druhy odlišných názvů se nenapárují, Anas platyrhynchos+Turdus merula nelze kvůli vysokému počtu nálezů spočítat spThin)
# species <- pull(ndop_top %>% select(species1, species2))
species <- ndop_top %>%
    select(species1_, species2_, species1, species2, species1_short, species2_short, Nálezů) %>%
    filter(Nálezů < 10000) # 70000)

# raster_stack <-
#   rasters_dir_stack(
#     paste0(
#       "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/1000"
#     ),
#     "tif"
#   )

filename_csv <- "gOverlap_3v_10000X.csv"
write_csv(
    data.frame(
        SchoenerD_1 = "SchoenerD_1",
        WarrenI_1 = "WarrenI_1",
        HellingerDist_1 = "HellingerDist_1",
        RMSE_1 = "RMSE_1",
        SpearmanCorrelation_1 = "SpearmanCorrelation_1",

        species2_ = "species2_",
        species1_ = "species1_",
        species1 = "species1",
        species2 = "species2",
        species1_short = "species1_short",
        species2_short = "species2_short",
        recnum = "recnum",
        pixel_size = "pixel_size",
        occ_count_gbif = "occ_count_gbif",
        occ_count_ndop = "occ_count_ndop",
        presence_gbif = "presence_gbif",
        auc_gbif = "auc_gbif",
        presence_ndop = "presence_ndop",
        auc_ndop = "auc_ndop",
        presence_all = "presence_all",
        auc_all = "auc_all",

        SchoenerD_2 = "SchoenerD_2",
        WarrenI_2 = "WarrenI_2",
        HellingerDist_2 = "HellingerDist_2",
        RMSE_2 = "RMSE_2",
        SpearmanCorrelation_2 = "SpearmanCorrelation_2",

        SchoenerD_3 = "SchoenerD_3",
        WarrenI_3 = "WarrenI_3",
        HellingerDist_3 = "HellingerDist_3",
        RMSE_3 = "RMSE_3",
        SpearmanCorrelation_3 = "SpearmanCorrelation_3",

        TPR_gbif = "TPR_gbif", TNR_gbif = "TNR_gbif", FPR_gbif = "TNR_gbif", FNR_gbif = "FNR_gbif",
        Sensitivity_gbif = "Sensitivity_gbif", Specificity_gbif = "Specificity_gbif",
        TSS_gbif = "TSS_gbif", Jaccard_gbif = "Jaccard_gbif", Sorensen_gbif = "Sorensen_gbif",
        F_measure_gbif = "F_measure_gbif",


        TPR_ndop = "TPR_ndop", TNR_ndop = "TNR_ndop", FPR_ndop = "TNR_ndop", FNR_ndop = "FNR_ndop",
        Sensitivity_ndop = "Sensitivity_ndop", Specificity_ndop = "Specificity_ndop",
        TSS_ndop = "TSS_ndop", Jaccard_ndop = "Jaccard_ndop", Sorensen_ndop = "Sorensen_ndop",
        F_measure_ndop = "F_measure_ndop",


        TPR_all = "TPR_all", TNR_all = "TNR_all", FPR_all = "TNR_all", FNR_all = "FNR_all",
        Sensitivity_all = "Sensitivity_all", Specificity_all = "Specificity_all",
        TSS_all = "TSS_all", Jaccard_all = "Jaccard_all", Sorensen_all = "Sorensen_all",
        F_measure_all = "F_measure_all"
    ),
    paste0(export_path, filename_csv),
    append = TRUE
)

# plot(enms$`10000`$Anthus_campestris[[2]]$p)

px_size <- c(10000) # 100, 200, 1000, 2000, 5000, 10000
replicates <- 1

enms <- list()

for (px_size_item in px_size) {
    rasters_path <- paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/seasons/", px_size_item, "/")
    vif5 <- c(
        paste0("l8_3-5_", px_size_item, "_MNDWI"),
        paste0("l8_6-8_", px_size_item, "_EVI"),
        paste0("l8_6-8_", px_size_item, "_MNDWI"),
        paste0("l8_9-11_", px_size_item, "_B5"),
        paste0("l8_9-11_", px_size_item, "_EVI"),
        paste0("l8_9-11_", px_size_item, "_MNDWI"),
        paste0("wc_", px_size_item, "_bio02"),
        paste0("wc_", px_size_item, "_bio03"),
        paste0("wc_", px_size_item, "_bio09"),
        paste0("wc_", px_size_item, "_bio13"),
        paste0("wc_", px_size_item, "_bio15")
    )


    vif5sapply <- lapply(vif5, function(x, rasters_path) {
        return(paste0(rasters_path, x, ".tif"))
    }, rasters_path = rasters_path)

    raster_stack <- stack(lapply(vif5sapply, raster::raster))


    # sjednocení CRS
    blocks_3035 <- blocks %>% st_transform(crs(raster_stack))
    blocks_erased_cz_3035 <- blocks_erased_cz %>% st_transform(crs(raster_stack))
    czechia_3035 <- czechia %>% st_transform(crs(raster_stack))

    # ořez původního raster_sttack na ČR pro lokální SDM
    raster_stack_crop <- crop(raster_stack, extent(czechia_3035))
    raster_stack_mask_czechia <- mask(raster_stack_crop, czechia_3035)



    plot_ndop_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/ndop_300.csv")
    plot_gbif_csv <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp/gbif_300.csv")




    ptaci_ndop_distinct <- plot_ndop_csv %>%
        group_by(species) %>%
        summarise(count = n_distinct(key)) %>%
        arrange(desc(count)) %>%
        filter(count >= 100)
    # %>%
    # filter(count < 200)
    ptaci_gbif_distinct <- plot_gbif_csv %>%
        group_by(species) %>%
        summarise(count = n_distinct(key)) %>%
        arrange(desc(count)) %>%
        filter(count >= 100)
    # %>%
    # filter(count < 200)

    plot_ndop_csv_100 <- plot_ndop_csv %>% filter(species %in% ptaci_ndop_distinct$species)
    plot_gbif_csv_100 <- plot_gbif_csv %>% filter(species %in% ptaci_gbif_distinct$species)



    for (sindex in 1:nrow(species)) { # sp in ptaci_gbif_distinct$species
        # foreach(sindex = 1:nrow(species), .combine=combine, .packages=c('dismo', "rJava")) %dopar% {
        # species1_, species2_, species1, species2, species1_short, species2_short, Nálezů
        sp <- species[sindex, 3]
        print("********************************************************** ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
        print(sp)
        cc_gbif <- plot_gbif_csv_100 %>% filter(species == as.character(sp))
        cc_ndop <- plot_ndop_csv_100 %>% filter(species == as.character(sp))
        sp_gbif_count <- ptaci_gbif_distinct %>%
            filter(species == as.character(sp)) %>%
            select(count)
        sp_ndop_count <- ptaci_ndop_distinct %>%
            filter(species == as.character(sp)) %>%
            select(count)
        # DDDprint("**********************************************************")

        if (count(cc_ndop) == 0 | count(cc_gbif) == 0) {
            print(paste0(species[sindex, 3], " - XXX - ", count(cc_ndop), "/", count(cc_gbif)))
            next
        } else {
            print(paste0(species[sindex, 3], " - *** - ", count(cc_ndop), "/", count(cc_gbif)))
        }


        # cc_gbif <- plot_gbif_csv_100
        # cc_ndop <- plot_ndop_csv_100


        # cc_3035 <- cc %>%
        #     st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
        #     st_transform(crs(raster_stack))


        cc_gbif_3035 <- cc_gbif %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
            st_transform(crs(raster_stack))


        cc_ndop_3035 <- cc_ndop %>%
            st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
            st_transform(crs(raster_stack))



        #   %>%
        #   st_coordinates() %>%
        #   as_tibble()
        # cc_3035 <- read_csv("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/export/schuzka2-total-gbif-ndop2/species/100_gbif_Coccothraustes_coccothraustes_1.csv") %>% st_as_sf(coords = c("longitude", "latitude"), crs = 3035) %>% st_transform(crs(raster_stack))

        # generovat si pseudo absence raději sám, nebo to nechat na maxentu?!? - dělá je vůbec maxent?


        # pa <- pseudo.absence(as_Spatial(cc_3035), n = 1000)
        # pa_sf <- st_as_sf(pa$sample)

        # plot(c(cc = cc_3035$geometry, pa = pa_sf$geometry), col=c("red","blue"))
        # plot(cc_3035$geometry, pa_sf$geometry)


        # st_write(cc_3035$geometry, paste0(export_path, "cc-p.shp"))
        # st_write(pa_sf$geometry, paste0(export_path, "cc-pa.shp"))



        # dodatečný ořez nálezů podle BB
        # cci <- st_intersection(cc_3035, blocks_3035)
        # st_write(st_intersection(cc_3035, blocks_3035), paste0(export_path, "cc-pa-i6.shp"))


        cci_gbif_3035 <- st_intersection(cc_gbif_3035, blocks_3035)
        cci_ndop_3035 <- st_intersection(cc_ndop_3035, czechia_3035)
        cci_all_3035 <- cci_gbif_3035 %>%
            select(key, species, geometry) %>%
            add_row(cci_ndop_3035 %>% select(key, species, geometry))
        # pseudoabsence raději sám
        # při použití masky musím zohlednit celkový počet pixelů rasteru? asi se mi nepodaří vygenerovat 10t absencí do 10km rasteru? nebo ano?
        # pabs_gbif_3035 <- st_as_sf(pseudo.absence(as_Spatial(cci_gbif_3035), n = 1000, window = "extent", Mask = raster_stack@layers[[1]])$sample)

        # !!! nemůžu ani takto apriori generovat pseudoabsence na značně biasovaném datasetu, neboť mi je umístí i do málo prosamplovaných míst, ikdyž tam reálně může být vysoký predikovaný výskyt
        # potřeboval bych je generovat až dodatečně na základě znalosti výsledku modelu po použití bias rasteru
        # pabs_gbif_3035_p <- st_as_sf(pseudo.absence(as_Spatial(cci_gbif_3035), n = 10000, window = "extent")$sample) %>%
        #     st_set_crs(crs(raster_stack)) %>%
        #     st_transform(crs(raster_stack))
        # pabs_ndop_3035_p <- st_as_sf(pseudo.absence(as_Spatial(cci_ndop_3035), n = 1000, window = "extent")$sample) %>%
        #     st_set_crs(crs(raster_stack)) %>%
        #     st_transform(crs(raster_stack))
        # pabs_all_3035_p <- st_as_sf(pseudo.absence(as_Spatial(cci_all_3035), n = 10000, window = "extent")$sample) %>%
        #     st_set_crs(crs(raster_stack)) %>%
        #     st_transform(crs(raster_stack))
        # # kontrola/výřez průniků vygěnerovaných pseodoabsencí
        # pabs_gbif_3035 <- st_intersection(pabs_gbif_3035_p, blocks_3035) %>%
        #     st_set_crs(crs(raster_stack)) %>%
        #     st_transform(crs(raster_stack))
        # pabs_ndop_3035 <- st_intersection(pabs_ndop_3035_p, czechia_3035) %>%
        #     st_set_crs(crs(raster_stack)) %>%
        #     st_transform(crs(raster_stack))
        # pabs_all_3035 <- st_intersection(pabs_all_3035_p, blocks_3035) %>%
        #     st_set_crs(crs(raster_stack)) %>%
        #     st_transform(crs(raster_stack))
        # st_write(pabs_gbif_3035, paste0(export_path, "pabs_gbif_3035-1000.shp"))
        # st_write(as_Spatial(cci_gbif_3035), paste0(export_path, "cci_gbif_3035.shp"))

        # mean(ev@TPR + ev@TNR) - 1
        # mean(ev@TPR) + mean(ev@TNR) - 1
        # ev@t[which.max(ev@TPR + ev@TNR)] - 1

        # t0 <- pa_sf %>%
        #     mutate(Species = 0) %>%
        #     select(-KDE)

        # t1 <- cci %>%
        #     mutate(Species = 1) %>%
        #     select(-key, -species, -id, -type)

        # pb_data <- bind_rows(t0, t1)

        # st_crs(pb_data) <- st_crs(raster_stack)

        # ukládat si do proměnné výsledky mxt_ a pred_ ?


        # Může pomoct ??? - nutné před voláním dismo
        # options(java.parameters = c("-Xss2560k", "-Xmx2g")) # -mx7000m
        # options(java.parameters = c("-Xmx10g")) # -mx7000m


        ###
        ### gbif
        ###
        print("GBIF")
        pp <- as.data.frame(as_Spatial(cci_gbif_3035))[c("coords.x1", "coords.x2")]

        colnames(pp)[1] <- "Longitude"
        colnames(pp)[2] <- "Latitude"
        enm_species <- enmtools.species(range = raster_stack[[1]], species.name = as.character(sp), presence.points = pp)


        # bias_1 <- sp.kde(x = as_Spatial(cci_gbif_3035), bw = 10000,
        #                        newdata = raster_stack[[1]], standardize = TRUE,
        #  					  scale.factor = 10000  )
        bias <- raster("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/bias_tg_Chlidonias-hybrida_1_gbif.tif")

        # bias_2 <- sp.kde(x = as_Spatial(cci_gbif_3035), bw = 100000,
        #                newdata = raster_stack[[1]], standardize = TRUE,
        # 			  scale.factor = 10000  )
        # writeRaster(bias_1, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/bias_tg_",sp, "_2_gbif.tif"), format = "GTiff", overwrite = TRUE)
        bias2 <- raster("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/bias_tg_Chlidonias-hybrida_2_gbif.tif") # 100000 10000 - totožné pro všešchny rozlišení rasterů?


        # bias_1 <- sp.kde(
        #     x = as_Spatial(cci_gbif_3035), bw = 10000,
        #     newdata = raster_stack[[1]], standardize = TRUE,
        #     scale.factor = 10000
        # )

        check.bg(species = enm_species, env = raster_stack, nback = 10000, bias = bias)

        check.species(enm_species)

        repl <- 2

        # enm_mxt_gbif1 <- replicate(repl,
        #     enmtools.maxent(
        #         enm_species,
        #         raster_stack,
        #         test.prop = 0.2,
        #         verbose = TRUE
        #     ),
        #     simplify = FALSE
        # )


        # names(enm_mxt_gbif1) <- paste0("rep", 1:repl)

        # enm_mxt_gbif1.r <- stack(sapply(enm_mxt_gbif1, function(x) x$suitability))


        # enm_mxt_gbif1.r.m <- calc(enm_mxt_gbif1.r, fun = mean)
        # writeRaster(enm_mxt_gbif1.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", repl, "_1_gbif.tif"), format = "GTiff", overwrite = TRUE)



        enm_mxt_gbif3 <- replicate(repl, enmtools.maxent(
            enm_species,
            raster_stack,
            test.prop = 0.2,
            verbose = TRUE,
            bias = bias2
        ),
        simplify = FALSE
        )

        # names(enm_mxt_gbif3) <- paste0("rep", 1:repl)
        enm_mxt_gbif3.r <- stack(sapply(enm_mxt_gbif3, function(x) x$suitability))
        enm_mxt_gbif3.r.m <- calc(enm_mxt_gbif3.r, fun = mean)
        writeRaster(enm_mxt_gbif3.r.m, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/r/mean_suitability_", sp, "_", repl, "_3_gbif.tif"), format = "GTiff", overwrite = TRUE)

        # možnost vypsat si VIP jednotlivých proměnných - vypisuje se po PÁRECH (výsledky+grafika) pr každé opakování!!!
        # - [[1]] obsahuje statistiky (tibble: Variable, Importance, StDev), [[2]] pak grafický výstup (ggplot) zobrajující přispění
        # str(enm_mxt_gbif3.vip[[1]], max.level = 2) # možnost vypsat si VIP jednotlivých proměnných
        enm_mxt_gbif3.vip <- sapply(enm_mxt_gbif3, enmtools.vip)


        # # # # # # # # # # # # # # # #
        # simuluje occurence points!!! - je použít k novému otestování: třeba pomocí blockCV (nebo novým modelem?) takto to dělal Gabbas??? - pak uvádím úplně nové statistiky modelované již z těchto PPM bodů?!
        # # # # # # # # # # # # # # # #
        # po párech X a Y souřadnice
        # str(enm_mxt_gbif3.sim[,1], max.level = 1) / první sada x,y souřadnic: enm_mxt_gbif3.sim[,1]$x enm_mxt_gbif3.sim[,1]$y
        # str(enm_mxt_gbif3.sim[,2], max.level = 1)
        enm_mxt_gbif3.sim <- sapply(enm_mxt_gbif3, sim.points, n.points = 10000)
        # as.data.frame(enm_mxt_gbif3.sim[,2])


        # str(enm_mxt_gbif3, max.level = 1)
        # str(enm_mxt_gbif3[[1]]$test.evaluation)

        # interactive.plot( enm_mxt_gbif3)

        # v <- visualize.enm(enm_mxt_gbif, raster_stack, layers = c=("wc_1000_bio09"))
        # v$suit.plot
        # v$background.plot

        stop()

        mxt_gbif <- maxent(x = raster_stack, p = as_Spatial(cci_gbif_3035), a = as_Spatial(pabs_gbif_3035), factors = NULL, removeDuplicates = TRUE, nbg = 10000, args = c(paste0("replicates=", replicates), "perspeciesresults=FALSE", "appendtoresultsfile=TRUE", "threads=4"))
        pred_gbif <- predict(mxt_gbif, raster_stack) # pří více replicates udělat mean(pr)
        pred_gbif_mean <- mean(pred_gbif)
        # ev <- sdm::getEvaluation(mxt_gbif, stat = c("AUC", "Kappa"))
        # ev <- sdm.package.evaluation(mxt_gbif)
        #  bg <- randomPoints(raster_stack, 1000)


        ev_gbif <- list()
        presence_gbif <- list()
        perf_gbif <- list()
        for (repl in 1:replicates) {


            # Při více replikacích přístup přes # mxt_gbif@models[[1]]
            # @models[[1]]

            # nemělo by se p= dělat nad foldama zvlášť?
            ev_gbif[[repl]] <- dismo::evaluate(p = as_Spatial(cci_gbif_3035), a = as_Spatial(pabs_gbif_3035), model = mxt_gbif, x = raster_stack)
            presence_gbif[[repl]] <- count(mxt_gbif@presence)[[1]]
            # DDDprint("ev ----------------------------------------------------------------------------------------------------")
            # DDDprint(ev_gbif[[repl]])
            # th[[repl]]  <- threshold(ev[[repl]])
            # #DDDprint(th)

            perf_gbif[[repl]] <- performanceC(ev_gbif[[repl]])
        }
        auc_gbif <- mean(sapply(ev_gbif, function(x) {
            x@auc
        }))
        # DDDprint(auc_gbif)

        presence_gbif <- mean(unlist(presence_gbif))
        # DDDprint(presence_gbif)

        perf_gbif <- colMeans(x = rbindlist(perf_gbif))
        # colnames(perf ) <- paste(colnames(perf ), 1, sep = "_")
        # DDDprint(perf_gbif)

        perf2_gbif <- transpose(as.data.frame(perf_gbif))
        colnames(perf2_gbif) <- paste(names(perf_gbif), "gbif", sep = "_")
        # DDDprint(perf2_gbif)
        # sapply( ev, function(x){ threshold(x)['spec_sens'] } )


        #         #DDDprint(count(mxt_gbif@presence)) # uložit
        # #DDDprint(count(mxt_gbif@models[[1]]@presence)) # uložit

        # threshold at maximum kappa
        # ev@t[which.max(ev@kappa)]

        # threshold at maximum of the sum of the sensitivity (true positive rate) and specificity (true negative rate)
        # ev@t[which.max(ev@TPR + ev@TNR)]



        # mxt_gbif2 <- enmtools.maxent(
        #   as_Spatial(cci_gbif_3035),
        #   raster_stack,
        #   test.prop = "block",
        #   nback = 1000,
        #   env.nback = 10000
        # )




        # ořez výsledné predikce
        pred_gbif_mean_crop <- crop(pred_gbif_mean, extent(czechia_3035))
        pred_gbif_mean_mask_czechia <- mask(pred_gbif_mean_crop, czechia_3035)



        ###
        ### ndop
        ###
        print("NDOP")
        # writeRaster(raster_stack_mask, paste0(export_path, "raster_stack_crop.tif"), format = "GTiff", overwrite = TRUE)
        mxt_ndop <- maxent(x = raster_stack_mask_czechia, p = as_Spatial(cci_ndop_3035), a = as_Spatial(pabs_ndop_3035), factors = NULL, removeDuplicates = TRUE, nbg = 10000, args = c(paste0("replicates=", replicates), "perspeciesresults=FALSE", "appendtoresultsfile=TRUE", "threads=4"))
        pred_ndop <- predict(mxt_ndop, raster_stack_mask_czechia) # pří více replicates udělat mean(pr)
        pred_ndop_mean <- mean(pred_ndop)



        ev_ndop <- list()
        presence_ndop <- list()
        perf_ndop <- list()
        for (repl in 1:replicates) {

            # nemělo by se p= dělat nad foldama zvlášť?
            ev_ndop[[repl]] <- dismo::evaluate(p = as_Spatial(cci_ndop_3035), a = as_Spatial(pabs_ndop_3035), model = mxt_ndop, x = raster_stack)
            presence_ndop[[repl]] <- count(mxt_ndop@presence)[[1]]
            # DDDprint("ev ----------------------------------------------------------------------------------------------------")
            # DDDprint(ev_ndop[[repl]])
            # th[[repl]]  <- threshold(ev[[repl]])
            # #DDDprint(th)

            perf_ndop[[repl]] <- performanceC(ev_ndop[[repl]])
        }
        auc_ndop <- mean(sapply(ev_ndop, function(x) {
            x@auc
        }))
        # DDDprint(auc_ndop)

        presence_ndop <- mean(unlist(presence_ndop))
        # DDDprint(presence_ndop)

        perf_ndop <- colMeans(x = rbindlist(perf_ndop))
        # colnames(perf ) <- paste(colnames(perf ), 1, sep = "_")
        # DDDprint(perf_ndop)

        perf2_ndop <- transpose(as.data.frame(perf_ndop))
        colnames(perf2_ndop) <- paste(names(perf_ndop), "ndop", sep = "_")
        # DDDprint(perf2_ndop)








        no1 <- niche.overlap.geog(pred_gbif_mean_mask_czechia, pred_ndop_mean, "1")
        # DDDprint(no1)
        ###
        # + i AUC hodnoty a rozdíly!
        # + i model GBIF+NDOP!!!
        ###


        ###
        ### all (ndpop+gbif)
        ###
        print("ALL")
        mxt_all <- maxent(x = raster_stack, p = as_Spatial(cci_all_3035), a = as_Spatial(pabs_all_3035), factors = NULL, removeDuplicates = TRUE, nbg = 10000, args = c(paste0("replicates=", replicates), "perspeciesresults=FALSE", "appendtoresultsfile=TRUE", "threads=4"))
        pred_all <- predict(mxt_all, raster_stack) # pří více replicates udělat mean(pr)
        pred_all_mean <- mean(pred_all)




        ev_all <- list()
        presence_all <- list()
        perf_all <- list()
        for (repl in 1:replicates) {

            # nemělo by se p= dělat nad foldama zvlášť?
            ev_all[[repl]] <- dismo::evaluate(p = as_Spatial(cci_all_3035), a = as_Spatial(pabs_all_3035), model = mxt_all, x = raster_stack)
            presence_all[[repl]] <- count(mxt_all@presence)[[1]]
            # DDDprint("ev ----------------------------------------------------------------------------------------------------")
            # DDDprint(ev_all[[repl]])
            # th[[repl]]  <- threshold(ev[[repl]])
            # #DDDprint(th)

            perf_all[[repl]] <- performanceC(ev_all[[repl]])
        }
        auc_all <- mean(sapply(ev_all, function(x) {
            x@auc
        }))
        # DDDprint(auc_all)

        presence_all <- mean(unlist(presence_all))
        # DDDprint(presence_all)

        perf_all <- colMeans(x = rbindlist(perf_all))
        # colnames(perf ) <- paste(colnames(perf ), 1, sep = "_")
        # DDDprint(perf_all)

        perf2_all <- transpose(as.data.frame(perf_all))
        colnames(perf2_all) <- paste(names(perf_all), "all", sep = "_")
        # DDDprint(perf2_all)






        no2 <- niche.overlap.geog(pred_all_mean, pred_gbif_mean, "2")
        # DDDprint(no2)




        # ořez výsledné predikce
        pred_gbif_mean_crop_cz <- crop(pred_gbif_mean, extent(blocks_erased_cz_3035))
        pred_gbif_mean_mask_crop_cz <- mask(pred_gbif_mean_crop_cz, blocks_erased_cz_3035)

        pred_all_mean_crop_cz <- crop(pred_all_mean, extent(blocks_erased_cz_3035))
        pred_gbif_all_mask_crop_cz <- mask(pred_all_mean_crop_cz, blocks_erased_cz_3035)
        png(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/", sp, "_gbif_bias.png"))
        plot(pred_gbif_mean_mask_crop_cz)
        dev.off()
        png(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp3/i/", sp, "_all_bias.png"))
        plot(pred_gbif_all_mask_crop_cz)
        dev.off()
        no3 <- niche.overlap.geog(pred_gbif_mean_mask_crop_cz, pred_gbif_all_mask_crop_cz, "3")
        # DDDprint(no3)




        s2 <- species[sindex, 6]

        if (species[sindex, 6] == "nana") {
            s2 <- NA
        }
        no1$species2_ <- species[sindex, 2]
        no1$species1_ <- species[sindex, 1]
        no1$species1 <- species[sindex, 3]
        no1$species2 <- species[sindex, 4]
        no1$species1_short <- species[sindex, 5]
        no1$species2_short <- s2
        no1$recnum <- species[sindex, 7]
        #   no$Q1 <- csvgee_1[1, 6] - csvgee_1[1, 9]
        #   no$Q2 <- csvgee_1[1, 6] - csvgee_2[1, 5]

        # + AUC (a další?) hodnoty
        # + evidovat i počty záznamů z GBIF a NDOP!!!

        no1$pixel_size <- px_size_item
        no1$occ_count_gbif <- sp_gbif_count
        no1$occ_count_ndop <- sp_ndop_count
        no1$presence_gbif <- presence_gbif
        no1$auc_gbif <- auc_gbif
        no1$presence_ndop <- presence_ndop
        no1$auc_ndop <- auc_ndop
        no1$presence_all <- presence_all
        no1$auc_all <- auc_all


        write_csv(bind_cols(no1[1, ], no2[1, ], no3[1, ], perf2_gbif[1, ], perf2_ndop[1, ], perf2_all[1, ]), paste0(export_path, filename_csv), append = TRUE)

        # chybí jestě evaluate...
        enms[[as.character(px_size_item)]][[as.character(no1$species1_)]] <- list(list(m = mxt_gbif, p = pred_gbif, e = ev_gbif), list(m = mxt_ndop, p = pred_ndop, e = ev_ndop), list(m = mxt_all, p = pred_all, e = ev_all))

        # go2 <- raster.overlap(pred_gbif_mean_mask_czechia, pred_ndop_mean)

        # env_stck <- setMinMax(raster_stack_mask_czechia)
        # eo <- env.overlap(mxt_ndop, mxt_gbif, env_stck, tolerance = 0.01)
        #  # niche envi.overlap -> Package ENM TOOLS
        # env_stck <- setMinMax(Bioall)
        # S6.Maxent_Envi.Overlap <- env.overlap(model.train, S6.model.train, env_stck, tolerance= 0.01)
    }
}

# Error in .local(x, p, ...) :
#   more than half of the absence points have NA predictor values

# str(enms[["10000"]][["Picus_viridis"]][[1]][["m"]])


# saveRDS(enms, file = "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/enms_1000.rds")

# detailní výsledky - fitovaný model(y) (první ze 3 modelů)
# enms[["10000"]][["Bubo_bubo"]][[1]][[1]]@models[[1]]@results
# detailní výsledky - průměrný raster predikcí
# enms[["10000"]][["Bubo_bubo"]][[1]][[2]]
# detailní výsledky - evaluate
# enms[["10000"]][["Bubo_bubo"]][[1]][[3]]




# # # #
# # Dodatečné získání infa o přispění jednotlivých prediktorů
# # # #

# # 0)
# # # získá názvy proměnných
# no_model <- 3
# model_res <- enms[["10000"]][["Bubo_bubo"]][[1]][[1]]@models[[no_model]]@results
# nms <- names(model_res[, 1])
# # odf <- data.frame(keyName=nms, value=model_res, row.names=NULL)
# odf <- data.frame(model_res)
# names(odf) <- nms
# write_csv(transpose(odf), "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/delete_10000_buboBubo_jeden_modelll.csv", append = TRUE)

# # newdf <- rbind(df, de)


# px_size <- 1000
# # jen hlavička
# model_res <- enms[[as.character(px_size)]][["Bubo_bubo"]][[2]][[1]]@models[[no_model]]@results
# nms <- names(model_res[, 1])
# odf <- transpose(data.frame(nms))
# odf$species <- "species"
# odf$pixel_size <- "px_size"
# write_csv(odf, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/predictors_ndop_", px_size, ".csv"), append = TRUE)


# species <- names(enms[[as.character(px_size)]])
# for (sp in species) {
#     model_res <- enms[[as.character(px_size)]][[as.character(sp)]][[2]][[1]]@models[[1]]@results
#     odf <- transpose(data.frame(model_res))
#     odf$species <- sp
#     odf$pixel_size <- px_size
#     write_csv(odf, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/predictors_ndop_", px_size, ".csv"), append = TRUE)
# }


# #

# # 1)
# title <- paste0("Permutation importance - NDOP Czechia ", px_size) # Preds contribution /  Permutation importance
# # png(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/",title, ".png"))

# p_1000 <- read_csv(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/predictors_ndop_", px_size, ".csv"))
# p_1000_prep <- p_1000 %>%
#     filter(Test.AUC >= 0.7) %>%
#     select(species, ends_with(".permutation.importance")) # .permutation.importance   .contribution
# names(p_1000_prep) <- gsub(x = names(p_1000_prep), pattern = "\\.permutation\\.importance", replacement = "")

# p_1000_prep$species <- as.factor(p_1000_prep$species)

# d <- as.matrix(p_1000_prep %>% select(-species))
# boxplot(d, use.cols = TRUE, las = 2, cex.axis = 0.5, main = title, ylim = c(0, 90))

# # dev.off()



# # 2)
# #-----------------------
# library(rstatix)

# p_1000_ndop <- read_csv(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/predictors_ndop_", 1000, ".csv")) %>% mutate(db = "NDOP")
# p_1000_gbif <- read_csv(paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/predictors_", 1000, ".csv")) %>% mutate(db = "GBIF")

# p_1000 <- bind_rows(p_1000_ndop, p_1000_gbif)

# p_1000_ndop <- p_1000 %>%
#     group_by(db) %>%
#     select(species, ends_with(".permutation.importance")) %>% # .permutation.importance   .contribution
#     get_summary_stats(type = "common") %>%
#     filter(db == "NDOP")
# p_1000_gbif <- p_1000 %>%
#     group_by(db) %>%
#     select(species, ends_with(".permutation.importance")) %>%
#     get_summary_stats(type = "common") %>%
#     filter(db == "GBIF")

# p_1000_r <- list()
# p_1000_r["ndop_wc"] <- sum(p_1000_ndop %>% filter(str_detect(variable, "^wc")) %>% select(mean))
# p_1000_r["ndop_l8"] <- sum(p_1000_ndop %>% filter(str_detect(variable, "^l8")) %>% select(mean))

# p_1000_r["gbif_wc"] <- sum(p_1000_gbif %>% filter(str_detect(variable, "^wc")) %>% select(mean))
# p_1000_r["gbif_l8"] <- sum(p_1000_gbif %>% filter(str_detect(variable, "^l8")) %>% select(mean))



# # p_1000_s <- list()
# # p_1000_s["ndop_wc"] <- sum(transpose(p_1000 %>% group_by(db) %>% select(species, ends_with(".contribution")) %>% filter(db == "NDOP") %>% select(starts_with("wc")) %>% summarise(across(where(is.numeric), sum)) %>% select(-db)))
# # p_1000_s["ndop_l8"] <- sum(transpose(p_1000 %>% group_by(db) %>% select(species, ends_with(".contribution")) %>% filter(db == "NDOP") %>% select(starts_with("l8")) %>% summarise(across(where(is.numeric), sum)) %>% select(-db)))
# # p_1000_s["gbif_wc"] <- sum(transpose(p_1000 %>% group_by(db) %>% select(species, ends_with(".contribution")) %>% filter(db == "GBIF") %>% select(starts_with("wc")) %>% summarise(across(where(is.numeric), sum)) %>% select(-db)))
# # p_1000_s["gbif_l8"] <- sum(transpose(p_1000 %>% group_by(db) %>% select(species, ends_with(".contribution")) %>% filter(db == "GBIF") %>% select(starts_with("l8")) %>% summarise(across(where(is.numeric), sum)) %>% select(-db)))


# # 3) dopočet součtů hodnot wc a l8 a jejich poměru
# p_1000_ndop <- p_1000 %>%
#     filter(db == "NDOP") %>%
#     select(db, px_size, species, ends_with(".permutation.importance")) %>%
#     mutate(wc = rowSums(select(., starts_with("wc")))) %>%
#     mutate(l8 = rowSums(select(., starts_with("l8"))))
# p_1000_gbif <- p_1000 %>%
#     filter(db == "GBIF") %>%
#     select(db, px_size, species, ends_with(".permutation.importance")) %>%
#     mutate(wc = rowSums(select(., starts_with("wc")))) %>%
#     mutate(l8 = rowSums(select(., starts_with("l8"))))
# p_1000_a <- bind_rows(p_1000_gbif, p_1000_ndop) %>% mutate(wc_l8 = wc / l8)

# # transpose(data.frame(names(p_1000_a))) # hlavička - názvy sloupců
# write_csv(p_1000_a, paste0("/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/tmp2/predictors_1000_10000.csv"), append = TRUE)