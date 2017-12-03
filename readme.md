Remote sensing resilience
================

This repository represents the entirety of the "remote sensing resilience" project which seeks to address the question: Does heterogeneity in forest structure make a forest resistant to wildfire? That is, does greater heterogeneity decrease wildfire severity when a fire inevitably occurs?

We rely on Google Earth Engine and R as our GIS. All Earth Engine code is in the ee-remote-sensing-resistance folder, which is itself a repository housed in Google's version control system. We mirror it here such that this repository remains a complete representation of the project.

The written manuscript is in the ms folder. The primary document is a .Rmd file, which is rendered as a .docx and as a .pdf. The .pdf is viewable on GitHub. I'm hoping this setup will allow programming-savy co-authors to interact with the code and the writing through the same version control framework, but also allow some flexibility for a more-typical "track changes on a Microsoft Word document" sort of relationship with the work.

Composite Burn Index (CBI) data sources:

Zhu, Z.; C. Key; D. Ohlen; N. Benson. 2006. Evaluate Sensitivities of Burn-Severity Mapping Algorithms for Different Ecosystems and Fire Histories in the United States. Final Report to the Joint Fire Science Program, Project JFSP 01-1-4-12, October 12, 2006. 35pp. [link](https://archive.usgs.gov/archive/sites/www.nrmsc.usgs.gov/science/fire/cbi/plotdata.html)

Sikkink, Pamela G.; Dillon, Gregory K.; Keane,Robert E.; Morgan, Penelope; Karau, Eva C.; Holden, Zachary A.; Silverstein, Robin P. 2013. Composite Burn Index (CBI) data and field photos collected for the FIRESEV project, western United States. Fort Collins, CO: Forest Service Research Data Archive. [link](https://doi.org/10.2737/RDS-2013-0017)