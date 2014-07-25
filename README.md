Leaves
======

This repository contains data and code for a paper with  [Jeroen Spijker](http://www.ced.uab.es/index.php?module=pagesetter&func=viewpub&tid=12&pid=21) and [John MacInnes](http://www.sps.ed.ac.uk/staff/sociology/macinnes_john), "Thanatological Age in Human Populations" (under review). It is likely too early to cite, but you may do so, and you may also use whatever you like here, with attribution:

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Thanatological Age in Human Populations</span> by <a xmlns:cc="http://creativecommons.org/ns#" href="https://sites.google.com/site/timriffepersonal/" property="cc:attributionName" rel="cc:attributionURL">Timothy L. M. Riffe</a> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
----------------------------------

This repository contains data and R code to reproduce all calculations and figures in the article.
All this is under the ```PlosOne``` branch, where the ```/R/``` folder is of interest

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

