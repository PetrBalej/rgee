#!/bin/bash

# sjednocení .asc rasterů pro MaxEnt

echo "start"

pathdown=$(cd ../ && pwd)
pathasc="/ndop/csv-top5/"
patht=${pathdown}${pathasc}

echo ${patht}

header="PORADI,AUTOR,DATUM_DO,DATUM_OD,ZDROJ,EVD,KATASTR,ID_LOKAL,SITMAP,NAZ_LOKAL,CXLOKAL_POZN,X,Y,CXLOKAL_Z,DAT_SADA,ZAPSAL,CXPRESNOST,PROJEKT,REDLIST,KAT_TAX,DRUH,GARANCE,VYHLASKA,VALIDACE,ID_NALEZ,NEGATIV,ODHAD,POCET,POKRYVN,POCITANO,POP_BIOT,UMIST_NAL,POZNAMKA,REL_POC,STRUKT_POZN,TAX_POZN,VEROH"

ext="_tab.csv"
# species0="Rana_temporaria"
# species0="Rana_dalmatina"
# species0="Lissotriton_vulgaris"
# species0="Bufo_bufo"
# species0="Bombina_bombina"
species0="Rana_temporaria"
species1=${species0}${ext}
species2="${species0}2${ext}"

awk '/<html>/{p=1;print}/<\/html>/{p=0}!p' ${patht}${species1} > ${patht}${species2}

sed -i 's/<html>//' ${patht}${species2}
sed -i 's/<\/html>//' ${patht}${species2}
sed -i 's/<!doctype html>//' ${patht}${species2}
sed -i '1s/^/'${header}'/' ${patht}${species2}

echo "end"

