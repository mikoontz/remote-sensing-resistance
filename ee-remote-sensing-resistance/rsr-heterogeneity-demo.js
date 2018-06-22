/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var perim = ee.FeatureCollection("users/mkoontz/fire_perim_16_1"),
    sn = ee.FeatureCollection("ft:1vdDUTu09Rkw5qKR_DSfmFX-b_7kqy4E-pjxg9Sq6"),
    bcp_poly = /* color: #98ff00 */ee.Geometry.Polygon(
        [[[-121.713066454027, 40.08782334785536],
          [-121.71340977562176, 40.08670702933947],
          [-121.71413933401004, 40.085787694463],
          [-121.71478306200027, 40.08440866886889],
          [-121.71439682520736, 40.08316095497444],
          [-121.71551262039009, 40.081880382726666],
          [-121.71637092437618, 40.08197888913974],
          [-121.7173579739615, 40.08066545858582],
          [-121.71924624273237, 40.07974604212327],
          [-121.71984705552438, 40.078531079897104],
          [-121.72057661391489, 40.077020556088634],
          [-121.71898875153937, 40.07725042056776],
          [-121.71834502355, 40.07665933890742],
          [-121.71744380436485, 40.07567419140558],
          [-121.71649966998001, 40.07505025728292],
          [-121.71529804439797, 40.075739868347995],
          [-121.71336686042787, 40.07537864437436],
          [-121.71207940444765, 40.07557567586957],
          [-121.71104943966338, 40.07583838364334],
          [-121.70959032288584, 40.076593662847905],
          [-121.70821703650722, 40.077053393914674],
          [-121.70650042853413, 40.07754595947079],
          [-121.70564212454752, 40.07774298469569],
          [-121.70482673576021, 40.07751312187834],
          [-121.70341053418235, 40.07761163460999],
          [-121.70229473900048, 40.078399731328766],
          [-121.7023376542005, 40.07922065571102],
          [-121.70276680619435, 40.07997589740123],
          [-121.70263806059683, 40.08053411413622],
          [-121.70233765420215, 40.08112516216152],
          [-121.70160809581392, 40.08122366966727],
          [-121.70079270702684, 40.081026654514474],
          [-121.70053521583151, 40.081519191328866],
          [-121.70049230063302, 40.08234007810131],
          [-121.70023480943792, 40.08306245027407],
          [-121.70113602862358, 40.08332512916323],
          [-121.70169392621506, 40.08394898746403],
          [-121.70388260137963, 40.08467134256442],
          [-121.7055562941519, 40.08431016597127],
          [-121.7072299869244, 40.08450717162262],
          [-121.70787371491366, 40.083390798725006],
          [-121.70868910370041, 40.08365347634778],
          [-121.70941866208841, 40.084540005842285],
          [-121.70907534049411, 40.08519668691381],
          [-121.71156442205228, 40.087757682566746]]]),
    bcp_point = /* color: #ffc82d */ee.Geometry.Point([-121.711639, 40.080928]),
    suppressed_poly = /* color: #0b4a8b */ee.Geometry.Polygon(
        [[[-121.70898914337158, 40.04286059086708],
          [-121.70722961425781, 40.04341911365563],
          [-121.70594215393066, 40.04345196779481],
          [-121.70405387878418, 40.043221988488014],
          [-121.70229434967041, 40.0433862595006],
          [-121.6999340057373, 40.04341911365563],
          [-121.69821739196777, 40.04253204591295],
          [-121.69774532318115, 40.04167782162284],
          [-121.69718742370605, 40.04197351585749],
          [-121.69718742370605, 40.042630609565435],
          [-121.69641494750977, 40.0433862595006],
          [-121.69551372528076, 40.044568999107135],
          [-121.69517040252686, 40.046967269209425],
          [-121.694655418396, 40.04828135403209],
          [-121.69349670410156, 40.050121030225725],
          [-121.69332504272461, 40.052814752212896],
          [-121.69890403747559, 40.05297900010992],
          [-121.70212268829346, 40.053143247611104],
          [-121.70366764068604, 40.05393163010686],
          [-121.70568466186523, 40.05465430604978],
          [-121.70559883117676, 40.052683353610306],
          [-121.7068862915039, 40.05110655061948],
          [-121.70903205871582, 40.04959541352257],
          [-121.71164989471436, 40.0490369413344],
          [-121.71229362487793, 40.047394349555404],
          [-121.71272277832031, 40.0459488360482],
          [-121.71083450317383, 40.04430616985667]]]),
    suppressed_point = /* color: #0b4a8b */ee.Geometry.Point([-121.7056149, 40.0481623]),
    mixed_con = ee.Image("users/mkoontz/mixed_conifer"),
    five_high = ee.FeatureCollection("users/mkoontz/five-high"),
    five_low = ee.FeatureCollection("users/mkoontz/five-low");
/***** End of imports. If edited, may not auto-convert in the playground. *****/
var rsr = require('users/mkoontz/ee-remote-sensing-resistance:rsr-functions.js');

// Purpose: Take the image of model variables and return values of those variables 
// at points
// This is  good for giving a sense of what the heterogeneity measurements look 
// like and comparing to an image from Google Earth

// Map.addLayer(five_high.filter(ee.Filter.eq('conifer_forest', 1)), {color: 'red'}, 'Five highest (1 pixel radius NDVI) heterogeneity samples');
// Map.addLayer(five_low.filter(ee.Filter.eq('conifer_forest', 1)), {color: 'blue'}, 'Five lowest (1 pixel radius NDVI) heterogeneity samples');


var timeWindow = 3*16;
var resample_method = 'bicubic';
var sats_string = '45';
var sats = ee.List(sats_string.split(''));

// Heterogeneity measurments
var bcp_poly_ftr = ee.Feature(bcp_poly, {alarm_date: ee.Date('2004-12-31').millis()});

var bcp_img = ee.Image(rsr.get_variables(bcp_poly_ftr, timeWindow, resample_method, sats)).clip(bcp_poly);

var het_1_viz = { min: 0.036, max: 0.29, 
                palette:['008000', 'ffff00',  'ffA500', 'ff0000']};

Map.addLayer(bcp_img.select(['het_ndvi_1']), het_1_viz, 'Heterogeneity of NDVI in a 90m x 90m square');
Map.addLayer(bcp_img.select(['het_ndvi_1', 'het_ndvi_2', 'het_ndvi_3', 'het_ndvi_4', 'RBR', 'elev']), {}, 'Beaver Creek Pinery image');

var bcp_point_ftr = ee.Feature(bcp_point, { alarm_date: ee.Date('2004-12-31').millis(),
                                            type: 'fire_use'});
var sup_point_ftr = ee.Feature(suppressed_point, {  alarm_date: ee.Date('2004-12-31').millis(),
                                                    type: 'suppressed'});

var pt_col_a = ee.FeatureCollection([bcp_point_ftr, sup_point_ftr]);

Map.addLayer(mixed_con, {}, 'mixed conifer');
Map.addLayer(sn, {}, 'sierra nevada');
Map.addLayer(pt_col_a, {color: 'red'}, 'Test points-- Group A');
Map.centerObject(pt_col_a);

var pt_stats_a = pt_col_a.map(rsr.calibrate_cbi({ timeWindow: timeWindow,
                                              resample_method: resample_method,
                                              sats: sats
}),
                          true);
print(pt_stats_a);

Export.table.toDrive({collection: pt_stats_a,
                      description: 'heterogeneity-demo-a',
                      folder: 'ee',
                      fileNamePrefix: 'heterogeneity-demo-a',
                      fileFormat: 'GeoJSON'});
                      
// var hi_het_point_ftr = ee.Feature(ee.Geometry.Point([-118.636323, 36.747634]), { alarm_date: ee.Date('2009-05-25').millis(),
//                                             type: 'hi_het'});
// var low_het_point_ftr = ee.Feature(ee.Geometry.Point([-118.778086, 36.761583]), {  alarm_date: ee.Date('2009-05-25').millis(),
//                                                     type: 'low_het'});

// var pt_col_b = ee.FeatureCollection([hi_het_point_ftr, low_het_point_ftr]);
// var pt_stats_b = pt_col_b.map(rsr.calibrate_cbi({ timeWindow: 1,
//                                               resample_method: 'bicubic'}),
//                           true);
// Map.addLayer(pt_col_b, {color: 'red'}, 'Test points-- Group B');
// print(pt_stats_b);

// Export.table.toDrive({collection: pt_stats_b,
//                       description: 'heterogeneity-demo-b',
//                       folder: 'ee',
//                       fileNamePrefix: 'heterogeneity-demo-b',
//                       fileFormat: 'GeoJSON'});
                      