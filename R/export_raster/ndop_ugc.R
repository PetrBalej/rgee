ndop_ugc <-
    function(years_range = list(from = "2017-01-01", to = "2019-12-31"),
             season_months_range = list(from = 4, to = 7),
             import_path_ndop = "/../ndop/csv",
             res_crs = 5514,
             presicion = 100) {
        # kontrola (do)instalace všech dodatečně potřebných balíčků
        required_packages <-
            c("tidyverse", "sf", "lubridate", "magrittr", "dplyr")
        install.packages(setdiff(required_packages, rownames(installed.packages())))

        # načte všechny požadované knihovny jako dělá jednotlivě library()
        lapply(required_packages, require, character.only = TRUE)

        # # # # # # # # # # # # # # # # # # # # # #
        # nastavení základních parametrů [start]  #
        # # # # # # # # # # # # # # # # # # # # # #

        ## časové rozsahy

        # Rozmezí datumů (sezóny a let) musí být stejné jako u filtru prediktorů! Ideálně přebírat společnou hodnotu jednoho parametru?

        # rozsah snímků od/do
        # years_range <- list(from = '2017-01-01', to = '2019-12-31')

        # rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
        # season_months_range <- list(from = 4, to = 7)


        # adresář pro exportované csv z NDOP pro další zpracování (pomocí QGIS pluginu https://github.com/OpenGeoLabs/qgis-ndop-downloader)
        # import_path_ndop <- paste0(getwd(), "/../ndop/csv")

        # # # # # # # # # # # # # # # # # # # # # #
        # nastavení základních parametrů [konec]  #
        # # # # # # # # # # # # # # # # # # # # # #

        col_types <- cols_only(
            DATUM_DO = col_date("%Y%m%d"),
            DATUM_OD = col_date("%Y%m%d"),
            ZDROJ = "c", SITMAP = "i", X = "d", Y = "d", DAT_SADA = "c",
            CXPRESNOST = "c", PROJEKT = "c", KAT_TAX = "f", DRUH = "f",
            GARANCE = "c", VALIDACE = "c", ID_NALEZ = "n", NEGATIV = "i",
            VEROH = "i"
        )

        # načte všechny *.csv z import_path_ndop
        csv_ndop <-
            list.files(
                path = import_path_ndop,
                pattern = "*.csv",
                full.names = T
            ) %>%
            map_df(~ read_csv(., col_types = cols(.default = "c"))) %>%
            filter(X != "<i>Skrytá lokalizace</i>") %>%
            filter(CXPRESNOST != "") %>%
            distinct(ID_NALEZ, .keep_all = TRUE) %>%
            # filter(DAT_SADA != "iNaturalist - data ČR") %>%
            # filter(AUTOR != "iNaturalist uživatel") %>%
            type_convert(
                col_types = col_types,
                locale = locale("cs", decimal_mark = ",")
            )

        # vypíše špatně rozparsované řádky
        # problems(csv_ndop)

        # csv_ndop <- as_tibble(csv_ndop) # netřeba?

        # VEROH: 0, 1, 3
        # VALIDACE: věrohodný záznam, méně věrohodný záznam, záznam k opravě
        # NEGATIV: 0, 1
        # & GARANCE == "Garantováno" & VEROH == 0 & NEGATIV == 0
        # VEROH == 1 odpovídá GARANCE == "Garantováno" ???

        # přesnost by měla být nižší hodnota než velikost cellsize prediktoru => měnit dynamicky?

        # základní dofiltrovaní nálezů z NDOPu
        csv_ndop_filter <- csv_ndop %>%
            filter(
                DATUM_OD >= years_range$from & DATUM_OD <= years_range$to &
                    DATUM_DO >= years_range$from & DATUM_DO <= years_range$to &
                    between(
                        month(DATUM_OD),
                        season_months_range$from,
                        season_months_range$to
                    ) &
                    between(
                        month(DATUM_DO),
                        season_months_range$from,
                        season_months_range$to
                    ) &
                    (VEROH == 1 | VALIDACE == "věrohodný záznam") &
                    NEGATIV == 0 &
                    CXPRESNOST <= presicion
            )


        if (is.null(res_crs)) {
            res_crs <- 5514
        }

        if (res_crs != 5514) {
            csv_ndop_filter %<>%
                mutate(
                    csv_ndop_filter %>% st_as_sf(coords = c("X", "Y"), crs = 5514) %>%
                        st_transform(res_crs) %>%
                        st_coordinates() %>% as_tibble()
                )
        }

        if (res_crs == 3035 || res_crs == 5514) {
            csv_ndop_filter$X %<>% as.integer
            csv_ndop_filter$Y %<>% as.integer
        } else {
            options(pillar.sigfig = 8) # jen pro případnou vizualizaci
        }

        # přidání sloupců s WGS84 souřadnicemi, výběr záznamů z polygonu a potřebných sloupců
        csv_ndop_filter %<>%
            dplyr::select(ID_NALEZ, DRUH, X, Y, KAT_TAX) %>%
            rename(
                key = ID_NALEZ,
                species = DRUH,
                cat = KAT_TAX
            )

        return(csv_ndop_filter)
    }
# res <- ndop_ugc(list(from = "2010-01-01", to = "2020-12-31"), list(from = 1, to = 12), "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/zal")
# print(as_tibble(res), n = 10)
# print(res %>% group_by(cat) %>% summarise(count = n_distinct(key)) %>% arrange(desc(count)))