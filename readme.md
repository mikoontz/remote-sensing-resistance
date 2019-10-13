Remote sensing resistance
================

This repository represents the entirety of the "remote sensing resistance" project which seeks to address the question: Does heterogeneity in forest structure make a forest resistant to wildfire? That is, does greater heterogeneity decrease wildfire severity when a fire inevitably occurs?

We rely on Google Earth Engine and R as our GIS. All Earth Engine code is in the ee-remote-sensing-resistance folder, which is itself a repository housed in Google's version control system. We mirror it here such that this repository remains a complete representation of the project.

The written manuscript is in the ms folder. The primary document is a .Rmd file, which is rendered as a .docx and as a .pdf. The .pdf is viewable on GitHub. I'm hoping this setup will allow programming-savy co-authors to interact with the code and the writing through the same version control framework, but also allow some flexibility for a more-typical "track changes on a Microsoft Word document" sort of relationship with the work.

## Data sources

### Jepson Ecoregion data for delineating "Sierra Nevada"

"Jepson Flora Project (eds.) 2016. Jepson eFlora, http://ucjeps.berkeley.edu/eflora/ [accessed on Mar 07, 2016]"

Contact Dr. David Baxter for a GIS layer.

### Fire Return Interval Departure data source for designating "yellow pine/mixed-conifer" (ypmc)

https://www.fs.usda.gov/detail/r5/landmanagement/gis/?cid=STELPRDB5327836

### Composite Burn Index (CBI) data sources

Zhu, Z.; C. Key; D. Ohlen; N. Benson. 2006. Evaluate Sensitivities of Burn-Severity Mapping Algorithms for Different Ecosystems and Fire Histories in the United States. Final Report to the Joint Fire Science Program, Project JFSP 01-1-4-12, October 12, 2006. 35pp. [link](https://archive.usgs.gov/archive/sites/www.nrmsc.usgs.gov/science/fire/cbi/plotdata.html)

Sikkink, Pamela G.; Dillon, Gregory K.; Keane,Robert E.; Morgan, Penelope; Karau, Eva C.; Holden, Zachary A.; Silverstein, Robin P. 2013. Composite Burn Index (CBI) data and field photos collected for the FIRESEV project, western United States. Fort Collins, CO: Forest Service Research Data Archive. [link](https://doi.org/10.2737/RDS-2013-0017)

## Reproducing the analysis

All scripts are numbered in the order of these steps. Note that some steps
involve uploading assets to Earth Engine, so these steps won't have a script
associated with them. In this case, there will be a discontinuity in the 
numbering of the scripts to highlight that there will be a step (of some kind)
to complete before moving on.

Raw data are found in "data/data_raw/". Data carpentry 
(aka munging/wrangling/cleaning) steps can be found in "data/data_carpentry/". 
The resulting data products from carpentry steps are stored in 
"data/data_output/".

Analyses scripts are found in "analyses/". Intermediate output (e.g., a summary
table from a model, a .rds file representing an R object of a long-running 
model) can be found in "analyses/analyses_output/"

1. data/data_carpentry/01_convert-jepson-ecoregions.R
2. In Earth Engine, run the 02_create-raster-template.js script to create a 
template raster co-registered with the Landsat product that will be used for
the yellow pine/mixed-conifer mask.
3. data/data_carpentry/03_create-ypmc-mask.R
4. data/data_carpentry/04_subset-frap-perimeter-database.R
5. data/data_carpentry/05_clean-cbi-data.R
6. Upload the CBI data output from 05_clean-cbi-data.R to Earth Engine. 
The Earth Engine asset is publicly available at: 
ee.FeatureCollection("users/mkoontz/cbi_sn")

7. In Earth Engine, run the 07_rsr-sn-cbi-calibration.js script to generate (and
export) eight .geoJSON files with the CBI plot information along with a number of
spectral severity calculations. Each .geoJSON file represents one combination of
interpolation type (bicubic or bilinear) and four time windows used to collate
the pre- and post-fire imagery (16, 32, 48, and 64 days). The resulting .geoJSON
files should be downloaded from your Google Drive and saved in:
"data/data_output/ee_cbi-calibration/"

8. analyses/08_cbi-k-fold-cross-validation.R

9. Upload the fire perimeters containing some yellow pine/mixed-conifer (ypmc) 
pixels output from the 04_subset-frap-perimeter-database.R script to Earth 
Engine. The Earth Engine asset is publicly available at: 
ee.FeatureCollection("users/mkoontz/fire18_1_sn_ypmc")

10. Using the best interpolation, spectral severity. and time window from the 
cross-fold validation, calculate the fire severity, vegetation characteristics,
and regional climate characteristics that we will use for modelling by running
the 10_rsr-frap-fires-assessment.js script in Earth Engine. The resulting 
.geoJSON should be downloaded from your Google Drive and saved as:
"data/data_output/ee_fire-samples/fires-strat-samples_2018_48-day-window_L4578_none-interp.geojson"

11. Prepare the fire samples for analysis by running 
"analyses/11_configure-fire-samples.R"

12. Build the primary analysis models for the paper (four separate models-- one
for each neighborhood window size) by running 
"analyses/12_probability-of-high-severity-build-models.R"

13. Summarize the posterior distributions of the estimated coefficients of
the models built in step 12 by running 
"analyses/13_probability-of-high-severity.R"

14. Perform model comparisons between models built at different scales to 
determine the primary scale of effect of the heterogeneity. Run the
"analyses/14_model-comparison.R" script.

15. Calculate summary information for the CBI calibration step by running
"analyses/15_cbi-summary-stats.R"

16. Compare the currently best-available YPMC wildfire dataset (USFS Region 5
Geospatial) to what we have developed by calculating some additional attribute
information. Run "data/data_carpentry/16_ypmc-pixel-count-usfs-r5.R"