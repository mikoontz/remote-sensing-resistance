/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var l5sr = ee.ImageCollection("LANDSAT/LT05/C01/T1_SR"),
    sn = ee.FeatureCollection("ft:1vdDUTu09Rkw5qKR_DSfmFX-b_7kqy4E-pjxg9Sq6");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var landsat = ee.Image(l5sr.filterDate("1985-07-27", "1986-07-27").
                select(["B1"]).
                first());

var mask = ee.Image(1).clip(sn);

var template_img = landsat.
                    unmask().
                    addBands(mask).
                    rename(["B1", "mask"]).
                    select(["mask"]).
                    clip(sn);
              
Map.addLayer(mask);                    
Map.addLayer(template_img);

Map.addLayer(landsat.clip(sn.geometry().bounds()), {min: -2000, max: 10000, palette: ["ff0000", "00ff00"]});
Map.centerObject(sn);

Export.image.toDrive({image: template_img,
                      description: "sierra_nevada_30m_landsat_template",
                      fileNamePrefix: "sierra_nevada_30m_landsat_template",
                      scale: 30,
                      region: sn.geometry().getInfo()['coordinates'],
                      maxPixels: 3000000000,
                      folder: "ee"
});