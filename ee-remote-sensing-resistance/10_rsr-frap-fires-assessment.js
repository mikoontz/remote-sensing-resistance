/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var perim16 = ee.FeatureCollection("users/mkoontz/fire_perim_16_1"),
    sn = ee.FeatureCollection("users/mkoontz/SierraEcoregion_Jepson"),
    perim17 = ee.FeatureCollection("users/mkoontz/fire17_1"),
    fire17_1_sn_ypmc = ee.FeatureCollection("users/mkoontz/fire17_1_sn_ypmc"),
    fire18_1_sn_ypmc = ee.FeatureCollection("users/mkoontz/fire18_1_sn_ypmc");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var rsr = require('users/mkoontz/ee-remote-sensing-resistance:rsr-functions.js');

// Purpose: Take the image of model variables and return values of those variables from 
// randomly sampled points within those perimeters
// This is good for collecting the data that will be used for analysis of severity vs. heterogeneity hypotheses

// Data availability:
//  Landast 4 SR: August 22, 1982 and December 14, 1993
//  Landsat 5 SR: January 1, 1984 to May 5, 2012
//  Landsat 7 SR: January 1, 1999 and December 21, 2017
//  Landsat 8 SR: April 11, 2013 and January 3, 2018

var latest_fire_year = 2018;

// For sampling within perimeters, how many samples in each fire for each cover class?
var non_conifer_samps = 0;
var conifer_samps = 100;
var timeWindow = 48;
var resample_method = 'none';

// How many FRAP fires might be usable for this analysis?

var fire17_1_sn_ypmc = fire17_1_sn_ypmc.map(function (feature) {
  feature = feature.set("alarm_date", feature.get("alrm_dt"))
                   .set("comments", feature.get("commnts"))
                   .set("cont_date", feature.get("cont_dt"))
                   .set("c_method", feature.get("c_methd"))
                   .set("fire_name", feature.get("fire_nam"))
                   .set("gis_acres", feature.get("gis_crs"))
                   .set("objective", feature.get("objectv"))
                   .set("report_ac", feature.get("reprt_c"));
  
  feature = feature.set("alrm_dt", null)
                   .set("cont_dt", null)
                   .set("commnts", null)
                   .set("c_methd", null)
                   .set("fire_nam", null)
                   .set("gis_crs", null)
                   .set("objectv", null)
                   .set("reprt_c", null);
  
  return feature;
});

var sats_string = '4578';
var sats = ee.List(sats_string.split(''));

// This code will assess all of the fires at once. It won't work if there are texture 
// calculations involved in the operation [see below; that's only because of the Rough 
// Fire!], so we only use the assess_whole_fire_lite() version to just pull out
// a few of the key covariates and response variables

var all_fire_assessments = fire18_1_sn_ypmc.map( rsr.assess_whole_fire_lite({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var all_fires_strat_samps = all_fire_assessments.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                      non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var all_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp";

Export.table.toDrive({
  'collection': all_fires_strat_samps.flatten(),
  'description': all_fire_samps_description,
  'folder': 'ee',
  'fileNamePrefix': all_fire_samps_description,
  'fileFormat': 'GeoJSON'
});

// Export the metadata

var all_fire_assessment_description = "fires-strat-samples_metadata_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_all";

var fire18_1_sn_ypmc_metadata = fire18_1_sn_ypmc.map(function (ftr) {return ee.Feature(null, ftr.toDictionary())});

Export.table.toDrive({
  'collection': fire18_1_sn_ypmc_metadata,
  'description': all_fire_assessment_description,
  'folder': 'ee',
  'fileNamePrefix': all_fire_assessment_description,
  'fileFormat': 'CSV'
});


// var gray_levels = 255; // number of bins (this value plus 1) for NDVI used for Geary's C and Gray Level Co-occurrance Matrix
// Used 10000 as gray_levels [esentially] in the past and got uninformative values
// of texture; probably because there were so many possible values of 'gray' that there 
// was *no* co-occurance 
// Used 25 as gray_levels (thus, 26 bins of NDVI) and got some texture values indicating
// *complete* smoothness across the image. Perhaps too *few* bins this time such that many
// pixels in the images showed no texture at all


// var all_fires = perim17
//                     .filterBounds(sn)
//                     .filter(ee.Filter.neq('alarm_date', null))
//                     .filter(ee.Filter.gt('alarm_date', ee.Date('1982-08-22').millis()))
//                     .filter(ee.Filter.lt('alarm_date', ee.Date('2017-12-31').millis()));

// Number of possible fires within Sierra Nevada from 1984 to 2017 that 
// burned at least partially in yellow pine/mixed-conifer
// print(fire17_1_sn_ypmc.size());


// We can do these exports using smaller time bounds to avoid timing out the Earth Engine machinery
// Use all the satellites (and the date ranges where there isn't any imagery from some satellites should
// be appropriately ignored)

// Batches represent fires within bins of approximately 5 years 
// Burned/unburned RBR threshold for 48-day window, bicubic interpolation calibration curve: 0.04509658

// var assessment_args = {'timeWindow': timeWindow, 'resample_method': resample_method, 'sats': sats, 'gray_levels': gray_levels};
// var samps_args = {'conifer_samps': conifer_samps, 'non_conifer_samps': non_conifer_samps};

// // Features with alarm dates before 1985-01-01
// var batch01_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('1985-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch01_fire_assessments = ee.ImageCollection(batch01_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch01_strat_samps = batch01_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch01_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_01";

// // Features with alarm dates later than or equal to 1985-01-01, but less than 1990-01-01
// var batch02_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('1990-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('1985-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch02_fire_assessments = ee.ImageCollection(batch02_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch02_strat_samps = batch02_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch02_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_02";

// // Features with alarm dates later than or equal to 1990-01-01, but less than 1995-01-01
// var batch03_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('1995-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('1990-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch03_fire_assessments = ee.ImageCollection(batch03_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch03_strat_samps = batch03_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch03_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_03";

// // Features with alarm dates later than or equal to 1995-01-01, but less than 2000-01-01
// var batch04_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2000-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('1995-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch04_fire_assessments = ee.ImageCollection(batch04_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch04_strat_samps = batch04_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch04_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_04";

// // Features with alarm dates later than or equal to 2000-01-01, but less than 2005-01-01
// var batch05_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2005-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2000-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch05_fire_assessments = ee.ImageCollection(batch05_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch05_strat_samps = batch05_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch05_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_05";

// // Features with alarm dates later than or equal to 2005-01-01, but less than 2010-01-01
// var batch06_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2010-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2005-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch06_fire_assessments = ee.ImageCollection(batch06_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch06_strat_samps = batch06_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch06_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_06";

// // Features with alarm dates later than or equal to 2010-01-01, but less than 2015-01-01
// var batch07_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2015-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2010-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch07_fire_assessments = ee.ImageCollection(batch07_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch07_strat_samps = batch07_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch07_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_07";

// // Features with alarm dates later than or equal to 2015-01-01, but less than 2015-07-01
// var batch08_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2015-07-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2015-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch08_fire_assessments = ee.ImageCollection(batch08_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch08_strat_samps = batch08_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch08_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_08";

// // A method to filter out non-polygons from a GeometryCollection and recombine them back into a Multipolygon
// // while also retaining the properties of the original feature

// // var filter_polygons = function(ftr) {
// //   var geometries = ftr.geometry().geometries();
  
// //   geometries = geometries
// //   .map(function (geo) {
// //     var augmented_geo = ee.Geometry(geo);
// //     return ee.Feature(augmented_geo).set('geoType', augmented_geo.type());
// //   });
  
// //   var polygons = ee.FeatureCollection(geometries).filter(ee.Filter.eq('geoType', 'Polygon')).geometry();
  
// //   return ee.Feature(polygons).copyProperties(ftr);
  
// // };

// // var rough2 = rough.map(filter_polygons);

// // Features with alarm dates later than or equal to 2015-07-01, but less than 2015-07-15
// // IMPORTANTLY, We leave out the Rough Fire of 2015 because it misbehaves unless downloaded
// // separately
// var batch09_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2016-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2015-07-01').millis())).filterMetadata('system:index', 'not_contains', '0000a936957321da93b5').map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch09_fire_assessments = ee.ImageCollection(batch09_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch09_strat_samps = batch09_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch09_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_09";

// // Features with alarm dates later than or equal to 2016-01-01, but less than 2016-07-01
// var batch10_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2016-07-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2016-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch10_fire_assessments = ee.ImageCollection(batch10_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch10_strat_samps = batch10_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch10_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_10";

// // Features with alarm dates later than or equal to 2016-07-01, but less than 2017-01-01
// var batch11_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2017-01-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2016-07-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch11_fire_assessments = ee.ImageCollection(batch11_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch11_strat_samps = batch11_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch11_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_11";

// // Features with alarm dates later than or equal to 2017-01-01, but less than 2017-07-01
// var batch12_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.lt('alarm_date', ee.Date('2017-07-01').millis())).filter(ee.Filter.gte('alarm_date', ee.Date('2017-01-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch12_fire_assessments = ee.ImageCollection(batch12_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch12_strat_samps = batch12_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch12_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_12";

// // Features with alarm dates later than or equal to 2017-07-01
// var batch13_fire_assessments = fire17_1_sn_ypmc.filter(ee.Filter.gte('alarm_date', ee.Date('2017-07-01').millis())).map(rsr.assess_whole_fire(assessment_args), true);
// // Include this next line to mask the image of all pixels that didn't burn based on best calibration
// batch13_fire_assessments = ee.ImageCollection(batch13_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var batch13_strat_samps = batch13_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var batch13_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_13";

// // Export all the samples
// Export.table.toDrive({
//   'collection': batch01_strat_samps.flatten(),
//   'description': batch01_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch01_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch02_strat_samps.flatten(),
//   'description': batch02_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch02_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch03_strat_samps.flatten(),
//   'description': batch03_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch03_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch04_strat_samps.flatten(),
//   'description': batch04_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch04_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch05_strat_samps.flatten(),
//   'description': batch05_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch05_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch06_strat_samps.flatten(),
//   'description': batch06_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch06_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch07_strat_samps.flatten(),
//   'description': batch07_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch07_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch08_strat_samps.flatten(),
//   'description': batch08_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch08_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch09_strat_samps.flatten(),
//   'description': batch09_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch09_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch10_strat_samps.flatten(),
//   'description': batch10_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch10_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch11_strat_samps.flatten(),
//   'description': batch11_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch11_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch12_strat_samps.flatten(),
//   'description': batch12_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch12_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// Export.table.toDrive({
//   'collection': batch13_strat_samps.flatten(),
//   'description': batch13_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': batch13_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// // Just the Rough fire, which seems to misbehave if it is with any other fire
// // This one should go with batch09 or with the `all` batch that is successfully 
// // exported below
// var rough = fire17_1_sn_ypmc.filterMetadata('system:index', 'contains', '0000a936957321da93b5');
// var rough_img = rough.map(rsr.assess_whole_fire(assessment_args), true);
// var rough_strat_samps = rough_img.map(rsr.get_stratified_samps(samps_args), true);
// var rough_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_rough";

// Export.table.toDrive({
//   'collection': rough_strat_samps.flatten(),
//   'description': rough_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': rough_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });


// // Trying for all the fires except the problematic Rough Fire of 2015
// var all_fire_assessments = fire17_1_sn_ypmc.filterMetadata('system:index', 'not_contains', '0000a936957321da93b5').map(rsr.assess_whole_fire(assessment_args), true);
// all_fire_assessments = ee.ImageCollection(all_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// var all_strat_samps = all_fire_assessments.map(rsr.get_stratified_samps(samps_args), true);
// var all_fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_all";

// Export.table.toDrive({
//   'collection': all_strat_samps.flatten(),
//   'description': all_fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': all_fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// // Exporting all metadata for the image collection (including the Rough Fire of 2015)
// var all_fire_assessments = fire17_1_sn_ypmc.map(rsr.assess_whole_fire(assessment_args), true);
// all_fire_assessments = ee.ImageCollection(all_fire_assessments).map(function(img) {return img.updateMask(img.select('RBR').gte(0.04509658))});
// all_fire_assessment_description = "fires-strat-samples_metadata_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_all";

// Export.table.toDrive({
//   'collection': all_fire_assessments,
//   'description': all_fire_assessment_description,
//   'folder': 'ee',
//   'fileNamePrefix': all_fire_assessment_description,
//   'fileFormat': 'CSV'
// });

