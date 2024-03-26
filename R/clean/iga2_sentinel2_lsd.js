var lsd_pa_polygons = ee.FeatureCollection("projects/bioatlas-1356/assets/iga2/lsd_pa_polygons");

//
// filtrování oblačnosti a kvalitry převzato z nápovědy (Examples) 
// v levém menu, zdrojem má být  https://github.com/google/earthengine-api
// tam ale nenalezeno
//

/**
 * Function to mask clouds using the Sentinel-2 QA band
 * @param {ee.Image} image Sentinel-2 image
 * @return {ee.Image} cloud masked Sentinel-2 image
 */
function maskS2clouds(image) {
    var qa = image.select('QA60');

    // Bits 10 and 11 are clouds and cirrus, respectively.
    var cloudBitMask = 1 << 10;
    var cirrusBitMask = 1 << 11;

    // Both flags should be set to zero, indicating clear conditions.
    var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
        .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

    return image.updateMask(mask).divide(10000);
}


//
// načtení vybraných kvadrátů z shapefilů pro obě verze velikostí (0Rad+2Rad z https://data.nature.cz/sds/6)
//

// vzor pro načtení shp a csv z vlastního Assets:
// var csvSelected2Rad = ee.FeatureCollection("projects/bioatlas-1356/assets/iga2/SitMap_0Rad-selected");


// nutno u 0Rad přetypovat POLE z csv na string, jinak nebude typově odpovídat při výběru KFME čtverců
var POLE_String = function (f) {
    return f.set({ POLE_String: ee.String(f.get("POLE_original")) })
}

//
// funkce pro vytvoření kolekcí a z nich výběr bandů a výpočet indexů s prefixem
//

function sentinelSelection(i, prefix) {
    var image = ee.Image(i);
    var p = ee.String(prefix);
    var imgBands = image.select([
        "B3", // green
        "B8" // NIR
    ], [p.cat("_B3"), p.cat("_B8")]);

    // evi, ndvi, mndwi
    var evi = image.expression("2.5 * ((b(8) - b(4)) / (b(8) + 6 * b(6) - 7.5 * b(2) + 1))").rename(p.cat("_EVI")); // https://custom-scripts.sentinel-hub.com/custom-scripts/sentinel/sentinel-2/
    var ndvi = image.normalizedDifference(["B8", "B4"]).rename(p.cat("_NDVI"));
    var mndwi = image.normalizedDifference(["B11", "B4"]).rename(p.cat("_MNDWI"));

    return imgBands.addBands([ndvi, evi, mndwi]);
}


function sentinelCollection(monthStart, monthEnd) {
    // březen až září 2018-2021 
    return ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
        .filterDate('2018-01-01', '2021-12-31')
        .filter(ee.Filter.calendarRange(monthStart, monthEnd, 'month'))
        // Pre-filter to get less cloudy granules.
        .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
        .map(maskS2clouds);

}

var img = ee.Image();

var sc39 = sentinelCollection(3, 9);
var sc33 = sentinelCollection(3, 3).median();
var sc44 = sentinelCollection(4, 4).median();
var sc55 = sentinelCollection(5, 5).median();
var sc66 = sentinelCollection(6, 6).median();
var sc77 = sentinelCollection(7, 7).median();
var sc88 = sentinelCollection(8, 8).median();
var sc99 = sentinelCollection(9, 9).median();


var imgs = [
    sentinelSelection(sc39.min(), "m00_min"),
    sentinelSelection(sc39.max(), "m00_max"),
    sentinelSelection(sc33, "m03_median"),
    sentinelSelection(sc44, "m04_median"),
    sentinelSelection(sc55, "m05_median"),
    sentinelSelection(sc66, "m06_median"),
    sentinelSelection(sc77, "m07_median"),
    sentinelSelection(sc88, "m08_median"),
    sentinelSelection(sc99, "m09_median")
];

var imgRes = img.addBands(imgs);


//
// Exporty do CSV a rasteru
//


var stats_median = imgRes.reduceRegions({
    collection: lsd_pa_polygons,
    reducer: ee.Reducer.median()
});
Export.table.toDrive({
    collection: stats_median,
    description: 'stats_median',
    fileFormat: 'CSV'
});


var stats_stdDev = imgRes.reduceRegions({
    collection: stats_median,
    reducer: ee.Reducer.stdDev()
});
Export.table.toDrive({
    collection: stats_stdDev,
    description: 'stats_stdDev',
    fileFormat: 'CSV'
});


// raster
Export.image.toDrive({
    image: imgRes.toFloat().clip(lsd_pa_polygons),
    description: 'median_bands',
    region: lsd_pa_polygons,
    scale: 30
});



// jen kontrola, že mám vybrané kvadráty k dispozici
Map.setCenter(15, 50, 8);
// Map.addLayer(imgs[0]);

