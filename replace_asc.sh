#!/bin/bash

# sjednocení .asc rasterů pro MaxEnt

echo "start"

pathdown=$(cd ../ && pwd)
pathasc="/export/str_evr_1km/"
patht=${pathdown}${pathasc}

echo ${patht}

# správné hodnoty (L8)
xllc="8.498062588"
yllc="45.993742547"
cs="0.00898315284109043"

# hodnoty k nahrazení (WC)
xllcwc="8.493496067"
yllcwc="45.991534231"
cswc="0.00898315284109042"

# hodnoty k nahrazení (SRTM)
xllcsrtm="8.493357178"
yllcsrtm="45.995403609"
cssrtm="0.00898315284109043"


# nahrazení

find ${patht} -name \*.asc -exec sed -i "s/XLLCORNER ${xllcwc}/XLLCORNER ${xllc}/g" {} \;
find ${patht} -name \*.asc -exec sed -i "s/YLLCORNER ${yllcwc}/YLLCORNER ${yllc}/g" {} \;

find ${patht} -name \*.asc -exec sed -i "s/XLLCORNER ${xllcsrtm}/XLLCORNER ${xllc}/g" {} \;
find ${patht} -name \*.asc -exec sed -i "s/YLLCORNER ${yllcsrtm}/YLLCORNER ${yllc}/g" {} \;

find ${patht} -name \*.asc -exec sed -i "s/CELLSIZE ${cswc}/CELLSIZE ${cs}/g" {} \;
find ${patht} -name \*.asc -exec sed -i "s/CELLSIZE ${cssrtm}/CELLSIZE ${cs}/g" {} \;

echo "end"