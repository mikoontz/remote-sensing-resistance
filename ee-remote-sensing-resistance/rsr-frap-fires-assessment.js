/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var perim16 = ee.FeatureCollection("users/mkoontz/fire_perim_16_1"),
    sn = ee.FeatureCollection("users/mkoontz/SierraEcoregion_Jepson"),
    perim17 = ee.FeatureCollection("users/mkoontz/fire17_1"),
    fire17_1_sn_ypmc = ee.FeatureCollection("users/mkoontz/fire17_1_sn_ypmc");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var rsr = require('users/mkoontz/ee-remote-sensing-resistance:rsr-functions.js');

// Purpose: Take the image of model variables and return values of those variables from 
// randomly sampled points within those perimeters
// This is good for collecting the data that will be used for analysis of severity vs. heterogeneity hypotheses

var latest_fire_year = 2017;

// For sampling within perimeters, how many samples in each fire for each cover class?
var non_conifer_samps = 0;
var conifer_samps = 100;
var timeWindow = 48;
var resample_method = 'none';

// How many FRAP fires might be usable for this analysis?

var all_fires = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('1982-08-22').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2017-12-31').millis()));

// 2143 possible fires within Sierra Nevada between these dates (through 2016)
// 2294 possible fires within Sierra Nevada through 2017
print(all_fires.size());
// Some may not have imagery available at their location, some may not contain any conifer forest,
// so the final number of sampled fires may be quite a bit less than this.

// Number of possible fires within Sierra Nevada from 1984 to 2017 that 
// burned at least partially in yellow pine/mixed-conifer
print(fire17_1_sn_ypmc.size());

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

var all_fire_assessments = fire17_1_sn_ypmc.map( rsr.assess_whole_fire_lite({ timeWindow: timeWindow,
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

// Data availability:
//  Landast 4 SR: August 22, 1982 and December 14, 1993
//  Landsat 5 SR: January 1, 1984 to May 5, 2012
//  Landsat 7 SR: January 1, 1999 and December 21, 2017
//  Landsat 8 SR: April 11, 2013 and January 3, 2018

// Use Landsat 4 and 5 for fires between start of Landsat 4 and just before
// Landsat 7 data become available. Then use Landsat 5, 7, and 8 for the
// start of Landsat 7 until present

// LANDSAT 4 AND 5

var sats_string = '45';
var sats = ee.List(sats_string.split(''));

// Now get all the data for the FRAP perimeter database
// Filter to features with non-null alarm dates
// Filter to features that are represented by only Landsat 4 and 5
// To do this, recall that the post-fire image collection end date
// is one year after the day before the fire started. So to avoid
// including fires that could have Landsat 7 contribute to their 
// assessment, be sure to end the filter one year prior to the start
// of Landsat 7 data availability
var early_target_fires = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('1982-08-22').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('1997-12-31').millis()));

// How many fires in the Sierra Nevada that have non-null alarm_dates and alarm_dates during the Landsat era? 2117
// print(target_fires.size());

var early_fire_assessments = early_target_fires.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var early_fires_strat_samps = early_fire_assessments.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                       non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var fire_samps_description = "fires-strat-samples_" + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp";

Export.table.toDrive({
  'collection': early_fires_strat_samps.flatten(),
  'description': fire_samps_description,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description,
  'fileFormat': 'GeoJSON'
});

// LANDSAT 5, 7, and 8

sats_string = '578';
sats = ee.List(sats_string.split(''));

// Now get all the data for the FRAP perimeter database
// Filter out features with null alarm dates
// Filter to features that are represented by Landsat 5, 7,
// and 8 after the Landsat 7 period starts. Only a few months
// of the Landsat 8 imagery might be used, but this will complete
// all of the possible data use of the Landsat 5 series
var mid_target_fires = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('1998-01-01').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2012-06-06').millis()));

var mid_fire_assessments = mid_target_fires.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var mid_fires_strat_samps = mid_fire_assessments.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                       non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var fire_samps_description = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp";

Export.table.toDrive({
  'collection': mid_fires_strat_samps.flatten(),
  'description': fire_samps_description,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description,
  'fileFormat': 'GeoJSON'
});


// LANDSAT 7 AND 8

sats_string = '78';
sats = ee.List(sats_string.split(''));

// Now get all the data for the FRAP perimeter database
// Filter out features with null alarm dates
// Filter to features that are represented by only Landsat 7 and 8
// To do this, recall that the post-fire image collection end date
// is one year after the day before the fire started. So to avoid
// including fires that could have Landsat 8 contribute to their 
// assessment, be sure to end the filter one year prior to the start
// of Landsat 8 data availability.

// var late_target_fires = perim
//                     .filterBounds(sn)
//                     .filter(ee.Filter.neq('alarm_date', null))
//                     .filter(ee.Filter.gt('alarm_date', ee.Date('2012-06-07').millis()))
//                     .filter(ee.Filter.lt('alarm_date', ee.Date('2018-01-06').millis()));

// var late_fire_assessments = late_target_fires.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
//                                                                   resample_method: resample_method,
//                                                                   sats: sats
// }),
//                                           true);

// var late_fires_strat_samps = late_fire_assessments.map(rsr.get_stratified_samps({conifer_samps: 50,
//                                                                       non_conifer_samps: 10}),
//                                               true);
                                                                    
// var fire_samps_description = "fires-strat-samples_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp";

// Export.table.toDrive({
//   'collection': late_fires_strat_samps.flatten(),
//   'description': fire_samps_description,
//   'folder': 'ee',
//   'fileNamePrefix': fire_samps_description,
//   'fileFormat': 'GeoJSON'
// });

// I keep getting a "User memory limit exceeded" error for this final run. Probably becuase 
// there are way more Landsat 8 scenes that need band renaming and selecting
// Let's try breaking this into X smaller batches

var late_target_fires_1 = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('2012-06-07').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2013-06-06').millis()));

var late_fire_assessments_1 = late_target_fires_1.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);


var late_fires_strat_samps_1 = late_fire_assessments_1.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                       non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    

var fire_samps_description_1 = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_1";

Export.table.toDrive({
  'collection': late_fires_strat_samps_1.flatten(),
  'description': fire_samps_description_1,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description_1,
  'fileFormat': 'GeoJSON'
});

// Batch 2

var late_target_fires_2 = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('2013-06-07').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2014-06-06').millis()));

var late_fire_assessments_2 = late_target_fires_2.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var late_fires_strat_samps_2 = late_fire_assessments_2.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                      non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var fire_samps_description_2 = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_2";

Export.table.toDrive({
  'collection': late_fires_strat_samps_2.flatten(),
  'description': fire_samps_description_2,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description_2,
  'fileFormat': 'GeoJSON'
});

// Batch 3

var late_target_fires_3 = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('2014-06-07').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2015-06-06').millis()));

var late_fire_assessments_3 = late_target_fires_3.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);


var late_fires_strat_samps_3 = late_fire_assessments_3.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                      non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    

var fire_samps_description_3 = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_3";

Export.table.toDrive({
  'collection': late_fires_strat_samps_3.flatten(),
  'description': fire_samps_description_3,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description_3,
  'fileFormat': 'GeoJSON'
});

// Batch 4

var late_target_fires_4 = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('2015-06-07').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2016-06-06').millis()));


var late_fire_assessments_4 = late_target_fires_4.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var late_fires_strat_samps_4 = late_fire_assessments_4.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                      non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var fire_samps_description_4 = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_4";

Export.table.toDrive({
  'collection': late_fires_strat_samps_4.flatten(),
  'description': fire_samps_description_4,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description_4,
  'fileFormat': 'GeoJSON'
});

// Batch 5

var late_target_fires_5 = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('2016-06-07').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2017-06-06').millis()));


var late_fire_assessments_5 = late_target_fires_5.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var late_fires_strat_samps_5 = late_fire_assessments_5.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                      non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var fire_samps_description_5 = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_5";

Export.table.toDrive({
  'collection': late_fires_strat_samps_5.flatten(),
  'description': fire_samps_description_5,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description_5,
  'fileFormat': 'GeoJSON'
});

// Batch 6

var late_target_fires_6 = perim17
                    .filterBounds(sn)
                    .filter(ee.Filter.neq('alarm_date', null))
                    .filter(ee.Filter.gt('alarm_date', ee.Date('2017-06-07').millis()))
                    .filter(ee.Filter.lt('alarm_date', ee.Date('2018-06-06').millis()));


var late_fire_assessments_6 = late_target_fires_6.map( rsr.assess_whole_fire({ timeWindow: timeWindow,
                                                                  resample_method: resample_method,
                                                                  sats: sats
}),
                                          true);

var late_fires_strat_samps_6 = late_fire_assessments_6.map(rsr.get_stratified_samps({conifer_samps: conifer_samps,
                                                                      non_conifer_samps: non_conifer_samps}),
                                              true);
                                                                    
var fire_samps_description_6 = "fires-strat-samples_"  + latest_fire_year + "_" + timeWindow + "-day-window_L" + sats_string + "_" + resample_method + "-interp_6";

Export.table.toDrive({
  'collection': late_fires_strat_samps_6.flatten(),
  'description': fire_samps_description_6,
  'folder': 'ee',
  'fileNamePrefix': fire_samps_description_6,
  'fileFormat': 'GeoJSON'
});

