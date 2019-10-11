Remote sensing resilience
================

This repository represents the entirety of the "remote sensing resilience" project which seeks to address the question: Does heterogeneity in forest structure make a forest resistant to wildfire? That is, does greater heterogeneity decrease wildfire severity when a fire inevitably occurs?

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

Data carpentry steps can be found in data/data_carpentry and the scripts are
numbered in the order they should be executed in.

- 01_convert-jepson-ecoregions.R
- 02_create-ypmc-mask.R
- 03_subset-frap-perimeter-database.R
- 04_clean-cbi-data.R

Next, upload the CBI data output from 04_clean-cbi-data.R to Earth Engine. 
The Earth Engine asset is publicly available at: 
ee.FeatureCollection("users/mkoontz/cbi_sn")

In Earth Engine, run the rsr-sn-cbi-calibration.js script to generate (and
export) eight .geoJSON files with the CBI plot information along with a number of
spectral severity calculations. Each .geoJSON file represents one combination of
interpolation type (bicubic or bilinear) and four time windows used to collate
the pre- and post-fire imagery (16, 32, 48, and 64 days). The resulting .geoJSON
files should be downloaded from your Google Drive and saved in:
"data/data_output/ee_cbi-calibration/"

The next step is calibrating the spectral data using k-fold cross validation. 
The script is located here:

analyses/01_cbi-k-fold-cross-validation.R

Upload the fire perimeters containing some yellow pine/mixed-conifer (ypmc) 
pixels output from the 03_subset-frap-perimeter-database.R script to Earth 
Engine. The Earth Engine asset is publicly available at: 
ee.FeatureCollection("users/mkoontz/fire18_1_sn_ypmc")

Using the best interpolation, spectral severity. and time window from the 
cross-fold validation, calculate the fire severity, vegetation characteristics,
and regional climate characteristics that we will use for modelling by running
the rsr-frap-fires-assessment.js script in Earth Engine.
