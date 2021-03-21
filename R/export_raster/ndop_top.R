ndop_top <-
    function(ndop_top_xls, gbif_csv) {

        # https://portal.nature.cz/nd/x_nd_statistiky.php?akce=seznam&opener=&vztazne_id=0

        required_packages <-
            c("tidyverse", "magrittr", "dplyr")
        install.packages(setdiff(required_packages, rownames(installed.packages())))

        # načte všechny požadované knihovny jako dělá jednotlivě library()
        lapply(required_packages, require, character.only = TRUE)

        library(readxl)

        xls <- readxl::read_excel(ndop_top_xls)

        synonyms <- list(
            "Spatula clypeata" = "Anas clypeata",
            "Phylloscopus sibilatrix" = "Phylloscopus sibillatrix",
            "Spatula querquedula" = "Anas querquedula",
            "Mareca penelope" = "Anas penelope",
            "Calidris pugnax" = "Philomachus pugnax",
            "Dryobates minor" = "Dendrocopos minor",
            "Acanthis cabaret" = "Acanthis flammea"
        )
        # Totéž nutno už při výběru z NDOP-u?
        # Nutno sjednotit některé poddruhy s druhy Acanthis cabaret do Acanthis flammea...
        xls %<>%
            filter(skupina == "ptáci") %>%
            filter("Nálezů" > 1000) %>%
            # select(`Druh/počet záznamů`) %>%
            rowwise() %>%
            mutate(species1 = str_split(`Druh/počet záznamů`, " - ")[[1]][1]) %>%
            filter(!grepl("sp.", species1)) %>%
            filter(!grepl("/", species1)) %>%
            filter(!grepl("f. domestica", species1)) %>%
            # ponechá jen první dvě slova (odstraní poddruhové a další názvy)
            mutate(
                species1 = paste0(
                    str_split(species1, " ")[[1]][1],
                    " ",
                    str_split(species1, " ")[[1]][2]
                )
            ) %>%
            mutate(
                species1_short = tolower(
                    paste0(
                        substr(str_split(species1, " ")[[1]][1], 1, 3),
                        substr(str_split(species1, " ")[[1]][2], 1, 3)
                    )
                )
            ) %>%
            mutate(
                species2 = ifelse(
                    is.null(synonyms[[species1]]), NA, synonyms[[species1]]
                )
            ) %>%
            mutate(
                species2_short = tolower(
                    paste0(
                        substr(str_split(species2, " ")[[1]][1], 1, 3),
                        substr(str_split(species2, " ")[[1]][2], 1, 3)
                    )
                )
            ) %>%
            mutate(species1_ = gsub(" ", "_", species1)) %>%
            mutate(species2_ = gsub(" ", "_", species2))

        ptaci_ndop_top_species <- xls %>%
            distinct(species1, species2)

        return(xls)

        print("vícenásobné taxony - vhodné sloučit?")
        multi_species <- xls %>%
            dplyr::count(species1) %>%
            filter(n > 1)
        print(multi_species)

        if (!is.null(gbif_csv)) {
            set_cols1 <-
                cols(
                    gbifID = "c",
                    coordinateUncertaintyInMeters = "d",
                    coordinatePrecision = "d",
                    day = "i",
                    month = "i",
                    year = "i"
                )

            ptaci_gbif <- readr::read_tsv(gbif_csv, col_types = set_cols1)

            ptaci_gbif_distinct_species <- ptaci_gbif %>%
                count(species) %>%
                filter(n > 100) %>%
                select(species)
            # %>% dplyr::distinct(species)


            join_anti <- ptaci_ndop_top_species %>%
                anti_join(ptaci_gbif_distinct_species, by = c("species1" = "species"))
            join <- ptaci_ndop_top_species %>%
                inner_join(ptaci_gbif_distinct_species, by = c("species1" = "species"))
            print(join_anti)
            print(join)
        }
    }

# ndop_top <- ndop_top(
#     paste0(getwd(), "/species/ndop/ndop-top-2021-03-21.xlsx"),
#     paste0(getwd(), "/../new-species/gbif/0209125-200613084148143-redukce4.csv")
# )