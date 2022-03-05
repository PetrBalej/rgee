required_packages <-
    c("raster", "tidyverse", "sf", "sp", "lubridate", "magrittr", "dplyr", "spatialEco", "dismo", "ENMToolsPB", "spatstat", "purrr", "abind") # "rmaxent", "blockCV", "ggplot2", "MASS", "data.table", "virtualspecies" (convertToPA - problematické definovat parametry v reálném světě...)
install.packages(setdiff(required_packages, rownames(installed.packages())))
# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)

## cesty k souborům, předpoklad je stáhnutí celého repozitáře https://github.com/PetrBalej/rgee/archive/master.zip
# domovský adresář (nebo jiný), z něhož se odvodí další cesty
# wd <- path.expand("~")
# wd <- "G:/balej/iga/rgee"
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
setwd(wd)

# export_path <- "G:/balej/iga/vse-v-jednom"
export_path <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/vse-v-jednom/UN"

df <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(df) <- c(
    "species", "px", "pxv",
    "md", "md.max", "md.d.max", "md.cor.max", "md.rmse.max",
    "d", "d.max",
    "cor", "cor.max",
    "rmse", "rmse.max",
    "ndop.auc"
)

for (px in c(500, 1000, 2000, 5000, 10000)) {
    px <- px
    for (pxv in 1:10) {
        if (px == 500 && pxv == 8) {
            # není vygenerováno...proč?
            next
        }
        pxv <- pxv

        rds <-
            list.files(
                path = paste0(export_path, "/outputs/rds"),
                # původní: "^glm_fmt_", d, "_.*\\.rds$"
                # "^glm_fmt_", d, "_.*[0-9]{4,5}-[0-9]{1,2}\\.rds$" - vše bez 500
                pattern = paste0("^enmsr_glm_UN_", px, pxv, "_", px, "_4_[0-9]+_scottIso-adj-0.65\\.rds$"),
                ignore.case = TRUE,
                full.names = TRUE
            )
        ndopCheckerboard2 <- readRDS(rds)

        n8.D <- readRDS(paste0(export_path, "/outputs/fitting/glm_fmt_gbif.D_", px, "-", pxv, ".rds"))
        n8.rmse <- readRDS(paste0(export_path, "/outputs/fitting/glm_fmt_gbif.rmse_", px, "-", pxv, ".rds"))
        n8.cor <- readRDS(paste0(export_path, "/outputs/fitting/glm_fmt_gbif.cor_", px, "-", pxv, ".rds"))

        pv8.D.names <- names(n8.D[[as.character(px)]])

        for (name in pv8.D.names) {
            pv8.D <- n8.D[[as.character(px)]][[as.character(name)]]
            pv8.cor <- n8.cor[[as.character(px)]][[as.character(name)]]
            pv8.rmse <- n8.rmse[[as.character(px)]][[as.character(name)]]

            ndopCheckerboard2.auc <- ndopCheckerboard2[[as.character(px)]][[as.character(name)]]$auc.ndop.te

            pv8multiply <- pv8.D * pv8.cor * (1 - pv8.rmse)

            md <- data.frame(nms = pv8.D$nms, V2 = pv8multiply$V2)
            md.max <- md[which.max(md$V2), ]

            pv8.D.max <- pv8.D[which.max(pv8.D$V2), ]
            pv8.cor.max <- pv8.cor[which.max(pv8.cor$V2), ]
            pv8.rmse.max <- pv8.rmse[which.max(pv8.rmse$V2), ]

            pv8.D.selected <- pv8.D[pv8.D$nms == md.max$nms, ]$V2
            pv8.cor.selected <- pv8.cor[pv8.cor$nms == md.max$nms, ]$V2
            pv8.rmse.selected <- 1 - pv8.rmse[pv8.rmse$nms == md.max$nms, ]$V2

            # plot(pv8.D$nms, pv8multiply$V2,
            #     pch = 19, cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.7, cex.sub = 0.7,
            #     main = paste0(name, ", px: ", px, ", D=", round(pv8.D.selected, 2), ", cor=", round(pv8.cor.selected, 2), ", rmse=", 1 - round(pv8.rmse.selected, 2)),
            #     sub = paste0("D*cor*(1-RMSE) ; ideal kernel width: ", md.max$nms, "; max: ", round(md.max$V2, 2))
            # )
            # points(md.max, pch = 19, col = "red")

            df[nrow(df) + 1, ] <- c(
                name, px, pxv,
                md.max$nms, round(md.max$V2, 4), round(pv8.D.selected, 4), round(pv8.cor.selected, 4), round(pv8.rmse.selected, 4),
                pv8.D.max$nms, round(pv8.D.max$V2, 4),
                pv8.cor.max$nms, round(pv8.cor.max$V2, 4),
                pv8.rmse.max$nms, round(pv8.rmse.max$V2, 4),
                round(ndopCheckerboard2.auc, 4)
            )
            # select treshold: 0.7*0.8*0.8=0.448
        }
    }
}


df <- as_tibble(df) %>% type_convert(
    col_types = cols(
        species = "f", px = "f",
        md = "d", md.max = "d", md.d.max = "d", md.cor.max = "d", md.rmse.max = "d",
        d = "d", d.max = "d",
        cor = "d", cor.max = "d",
        rmse = "d", rmse.max = "d",
        ndop.auc = "d"
    )
)
stop()

auc.min <- 0.7
# souhrny
summary(df %>% filter(ndop.auc > auc.min) %>% filter(px == 2000) %>% filter(md.max > 0.45))
summary(df %>% filter(ndop.auc > auc.min) %>% filter(md.max > 0.45))

df %>%
    group_by(species) %>%
    filter(ndop.auc > auc.min) %>%
    # filter(px == 10000) %>%
    filter(md.max > 0.45)
unique(df %>% filter(ndop.auc > auc.min) %>% filter(md.max > 0.45) %>% select(species))

# počet unikátních druhů, které nepořebovaly bias korekci! (kernel width 0 ): 3 druhy, jeden 3 rozlišení
df %>%
    group_by(species) %>%
    filter(ndop.auc > auc.min) %>%
    filter(md == 0.00) %>%
    filter(md.max > 0.45)
unique(df %>% filter(ndop.auc > auc.min) %>% filter(md == 0.00) %>% filter(md.max > 0.45) %>% select(species))

# # A tibble: 5 x 6
# # Groups:   species [3]
#   species               px      pxv    md md.max ndop.auc
#   <fct>                 <fct> <dbl> <dbl>  <dbl>    <dbl>
# 1 Himantopus himantopus 500       5     0  0.548    0.941
# 2 Limosa limosa         500       6     0  0.541    0.905
# 3 Himantopus himantopus 2000      5     0  0.598    0.950
# 4 Himantopus himantopus 10000     5     0  0.472    0.901
# 5 Merops apiaster       10000     7     0  0.506    0.858


# nebo jen malá bias korekce
df %>%
    group_by(species) %>%
    filter(ndop.auc > auc.min) %>%
    filter(md <= 0.20) %>%
    filter(md.max > 0.45)
unique(df %>% filter(ndop.auc > auc.min) %>% filter(md <= 0.20) %>% filter(md.max > 0.45) %>% select(species))


# # A tibble: 17 x 6
# # Groups:   species [10]
#    species                 px      pxv    md md.max ndop.auc
#    <fct>                   <fct> <dbl> <dbl>  <dbl>    <dbl>
#  1 Himantopus himantopus   500       5  0     0.548    0.941
#  2 Limosa limosa           500       6  0     0.541    0.905
#  3 Himantopus himantopus   1000      5  0.2   0.590    0.963
#  4 Himantopus himantopus   2000      5  0     0.598    0.950
#  5 Limosa limosa           2000      6  0.2   0.552    0.945
#  6 Botaurus stellaris      5000      2  0.2   0.481    0.795
#  7 Dendrocoptes medius     5000      4  0.2   0.474    0.760
#  8 Himantopus himantopus   5000      5  0.2   0.598    0.930
#  9 Limosa limosa           5000      6  0.2   0.522    0.842
# 10 Merops apiaster         5000      7  0.2   0.624    0.881
# 11 Netta rufina            5000      7  0.2   0.502    0.843
# 12 Panurus biarmicus       5000      7  0.2   0.545    0.862
# 13 Recurvirostra avosetta  5000      8  0.2   0.491    0.871
# 14 Crex crex               10000     4  0.15  0.510    0.784
# 15 Himantopus himantopus   10000     5  0     0.472    0.901
# 16 Merops apiaster         10000     7  0     0.506    0.858
# 17 Nucifraga caryocatactes 10000     7  0.2   0.550    0.753


df45 <- df %>%
    filter(ndop.auc > auc.min) %>%
    filter(md.max > 0.45)

hist(df45$md)

hist(df$md.max)


# par(new = TRUE
# plot(pv8.D, col = "red")
# par(new = TRUE)
# plot(pv8.cor, col = "darkgreen")
# par(new = TRUE)
# plot(pv8.rmse$nms, 1 - pv8.rmse$V2, col = "blue")