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

speciesPerGroup <- 40
# kterou skupinu vyberu pro modelování (při rozdělení paralelních výpočtů do více konzolí)
species.part <- cmd_arg # 1

# "C:/Program Files/R/R-4.2.1/bin/x64/Rscript.exe" "D:/PersonalWork/Balej/sdmKostelec2023/kostelec-kombinace-prediktoru-lsd-glm-sdm-partOutput23.R" 1
#  Rscript "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee/R/kostelec2023/ws2/kostelec-kombinace-prediktoru-lsd-glm-sdm-partOutput23-varImp.R" 1
# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <-
    c("tidyverse", "sf", "lubridate", "magrittr", "dplyr", "raster", "readxl", "abind", "raster", "sdm")
install.packages(setdiff(required_packages, rownames(installed.packages())))

# načte všechny požadované knihovny jako dělá jednotlivě library()
lapply(required_packages, require, character.only = TRUE)




# wd <- "D:/PERSONAL_DATA/pb/rgee"
# wd <- "D:/PersonalWork/Balej/sdmKostelec2023/rgee" # samsung500ntfs # paste0(path.expand("~"), "/Downloads/rgee2/rgee")
wd <- "/mnt/2AA56BAE3BB1EC2E/Downloads/rgee2/rgee"

setwd(wd)

source(paste0(getwd(), "/R/export_raster/functions.R"))

# path.igaD <- "D:/PersonalWork/Balej/sdmKostelec2023/k2023-23/"
path.igaD <- "/home/petr/Documents/igaD/k2023/"

df <- readRDS(paste0(wd, "/R/kostelec2023/NB/clean/lsd-PA-FINAL-57preds-118sp.df.rds"))
set.seed(85)
df <- df[sample(1:nrow(df)), ]
rownames(df) <- NULL

remove <- readRDS(paste0(wd, "/R/kostelec2023/NB/k2023/preds07-remove.rds"))[-1] # jedna záměna ve VIFu

df <- df[, !(names(df) %in% remove)]

species.selected.prevalence <- names(df)[24:141]

species.parts <- split(species.selected.prevalence, ceiling(seq_along(species.selected.prevalence) / speciesPerGroup))


species.parts.f <- paste(species.parts[[species.part]], collapse = "+")

pred.names.all <- names(df)[1:23] # 1:15 16:23



#  d <- sdm::sdmData(as.formula(paste0(species.parts.f, "~", pred.names.all.f, "+coords(x+y)")), train = df)

# m <- sdm::sdm(~.,data=d,methods=c('glm'),replication=c('cv'),cv.folds=5)


varImpSelect <- function(df, sp, preds, first = TRUE) {
    preds.n <- length(preds)
    print(preds)
    print(preds.n)
    if (preds.n > 0) {
        pred.names.all.f <- paste(preds, collapse = "+")


        d <- sdm::sdmData(as.formula(paste0(sp, "~", pred.names.all.f, "+coords(x+y)")), train = df)

        m <- sdm::sdm(as.formula(paste0(sp, "~", pred.names.all.f)), data = d, methods = c("glm"), replication = c("cv"), cv.folds = 3, n = 5, seed = TRUE)

        if (preds.n > 1) {
            vi <- sdm::getVarImp(m, wtest = "test.dep")

            vi.remove <- vi@varImportanceMean$AUCtest %>%
                slice_min(AUCtest, n = 1, with_ties = FALSE) %>%
                dplyr::select(variables)
        }




        df.merge.train <- merge(
            getEvaluation(m,
                wtest = "training",
                stat = c("AUC", "COR", "Deviance", "obs.prevalence", "threshold", "sensitivity", "specificity", "TSS", "Kappa", "NMI", "phi", "ppv", "npv", "ccr", "prevalence")
            ),

            getModelInfo(m),
            by = "modelID"
        )


        df.merge.test <- merge(
            getEvaluation(m,
                wtest = "test.dep",
                stat = c("AUC", "COR", "Deviance", "obs.prevalence", "threshold", "sensitivity", "specificity", "TSS", "Kappa", "NMI", "phi", "ppv", "npv", "ccr", "prevalence")
            ),

            getModelInfo(m),
            by = "modelID"
        )

        df.merge.train$preds <- pred.names.all.f
        df.merge.test$preds <- pred.names.all.f


        write.table(df.merge.train, file = paste0(path.igaD, "../k2023-23-varImp/", species.part, "-train.csv"), append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = first)
        write.table(df.merge.test, file = paste0(path.igaD, "../k2023-23-varImp/", species.part, "-test.csv"), append = TRUE, quote = TRUE, sep = ",", row.names = FALSE, col.names = first)



        if (first) {
            first <- FALSE
        }

        # saveRDS(m, paste0(path.igaD, preds.k, "---", species.part, "---",  pred.names.f,".rds"))

        m <- list()
        gc()

        if (preds.n > 1) {
            varImpSelect(df, sp, preds[!preds == vi.remove$variables], first = FALSE)
        }
    }
}
first <- TRUE
for (sp in species.parts[[species.part]]) {
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print(sp)
    varImpSelect(df, sp, pred.names.all, first)
    if (first) {
        first <- FALSE
    }
}