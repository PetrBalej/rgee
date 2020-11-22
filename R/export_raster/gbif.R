# kontrola (do)instalace všech dodatečně potřebných balíčků
required_packages <- c("tidyverse", "rgbif", "sf")
install.packages(setdiff(required_packages, rownames(installed.packages())))

library(tidyverse)
library(rgbif)
library(sf)

# # # # # # # # # # # # # # # # # # # # # #
# nastavení základních parametrů [start]  #
# # # # # # # # # # # # # # # # # # # # # #

## výběr regionu

# definice obálek (bounding box) různě velkých území pro testování
sz_cechy <- list(xmin = 13.0, xmax = 13.5, ymin = 50.0, ymax = 50.5)
cesko <- list(xmin = 12.0, xmax = 19.0, ymin = 48.5, ymax = 51.5)
str_evropa <- list(xmin = 8.5, xmax = 22.0, ymin = 46.0, ymax = 53.5)

# výběr konkrétního území
bb <- sz_cechy

## časové rozsahy

# Rozmezí datumů (sezóny a let) musí být stejné jako u filtru prediktorů! Ideálně přebírat společnou hodnotu jednoho parametru?

# rozsah snímků od/do
years_range <- list(from = '2017-01-01', to = '2019-12-31')

# rozsah jedné sezóny v měsících (podvýběr z vybraného období years_range výše)
season_months_range <- list(from = 4, to = 7)

# # # # # # # # # # # # # # # # # # # # # #
# nastavení základních parametrů [konec]  #
# # # # # # # # # # # # # # # # # # # # # #

xmin <- bb$xmin
xmax <- bb$xmax
ymin <- bb$ymin
ymax <- bb$ymax

boundingBox <- cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin))
boundingBox_wkt <- st_as_text(st_polygon(list(boundingBox)))


# issue
# occ_search() vs occ_data()

res <- occ_data(scientificName ='Lacerta agilis', geometry = boundingBox_wkt, limit = 10)

print(as_tibble(res$data), n = 10)
