Leaves
======

data and code for "Thanatological Age in Human Populations" (under review)

----------------------------------
This repository contains data and R code to reproduce all calculations and figures in the article.
All this is under the ```PlosOne branch```, where the ```/R/``` folder is of interest

R scripts are divided into two groups:

1) ```R/RiffeetalFunctions/``` is a pseudo R package, which you can install and load in R live from github using:

```r
library(devtools)
install_github("Leaves", subdir = "PlosOne/R/RiffeetalFunctions", username = "timriffe")
library(RiffeetalFunctions)
```

Where needed, this sort of thing is explained in script headers. 

2) the other scripts with descriptive names divide up the data preparation and figure production, and complementary
calculations cited in the paper. There are many extras as well. These scripts have been annotated heavily at times
to aid in understanding what's going on. Questions may be directed to Tim Riffe.

To run these scripts, get a copy of this repository on your local system.
you can either clone this repository using git, or you can download this repository as a zip file and unpack it.

This page will be maintained as time permits to make it clearer for those not used to working in git or R, 
and organized if necessary.

