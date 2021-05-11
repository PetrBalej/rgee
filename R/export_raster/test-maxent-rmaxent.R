
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "ggplot2", "dismo", "rmaxent", "ENMToolsRMaxent", "microbenchmark", "rasterVis", "viridis")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip

# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")

setwd(wd)


export_path <- paste0(getwd(), "/test-rmaxent/")


# https://github.com/danlwarren/ENMTools/blob/master/R/enmtools.maxent.R#L448
predict.enmtools.maxent <- function(object, env, maxpts = 1000, clamp = TRUE, ...) {

    # Make a plot of habitat suitability in the new region
    suitability <- raster::predict(env, object$model, type = "prob")

    # I'm actually not sure this is doing anything - I think maxent models are clamped by default
    # if (clamp == TRUE) {
    #     # Adding env (skipped for MX otherwise)
    #     this.df <- as.data.frame(rbind(object$model@presence, object$model@absence))

    #     env <- clamp.env(this.df, env)
    #     clamped.suitability <- raster::predict(env, object$model)
    #     clamping.strength <- clamped.suitability - suitability
    #     suitability <- clamped.suitability
    # }

    # suit.points <- data.frame(rasterToPoints(suitability))
    # colnames(suit.points) <- c("Longitude", "Latitude", "Suitability")

    # suit.plot <- ggplot(data = suit.points, aes_string(y = "Latitude", x = "Longitude")) +
    #     geom_raster(aes_string(fill = "Suitability")) +
    #     scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Suitability")) +
    #     coord_fixed() +
    #     theme_classic()

    # if (!is.na(object$species.name)) {
    #     title <- paste("Maxent model projection for", object$species.name)
    #     suit.plot <- suit.plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    # }

    # this.threespace <- threespace.plot(object, env, maxpts)

    output <- list(
        # suitability.plot = suit.plot,
        suitability = suitability
        # clamping.strength = clamping.strength
        # ,
        # threespace.plot = this.threespace
    )
    return(output)
}



#
# https://github.com/johnbaums/rmaxent
#
occ_file <- system.file("ex/bradypus.csv", package = "dismo")
occ <- read.table(occ_file, header = TRUE, sep = ",")[, -1]

# musím odstranit poslední kateg. proměnnou biome
pred_files <- list.files(system.file("ex", package = "dismo"), "\\.grd$", full.names = TRUE)[1:8]
predictors <- stack(pred_files)


me <- maxent(predictors, occ,
    # factors = "biome",
    args = c("hinge=false", "threshold=false")
)

r1 <- raster::predict(predictors, me, type = "prob")
r2 <- raster::predict(predictors, me, type = "response")
# r3 <- stats::predict(me, predictors, type = "response")

# prediction <- project(me, predictors)



me_prep <- list(model = me, species.name = "Species Name")

library(microbenchmark)
timings <- microbenchmark(
    rmaxent = pred_rmaxent <- rmaxent::project(me, predictors),
    dismo = pred_dismo <- predict(me, predictors),
    enmtools = pred_enmtools <- predict.enmtools.maxent(me_prep, predictors),
    raster = pred_raster <- raster::predict(predictors, me),
    times = 3
)

print(timings, signif = 2)


# print(all.equal(values(pred_rmaxent$prediction_logistic), values(pred_dismo)))
# nutné použít defaultní cloglog, ne jak v ukázce

print(all.equal(values(pred_rmaxent$prediction_cloglog), values(pred_dismo)))

print(all.equal(values(pred_enmtools$suitability), values(pred_dismo)))
print(all.equal(values(pred_raster), values(pred_dismo)))

# print(parse_lambdas(me))


# lim <- limiting(predictors, me)
# print(
#     levelplot(lim, col.regions = rainbow) +
#         layer(sp.points(SpatialPoints(occ), pch = 20, col = 1))
# )