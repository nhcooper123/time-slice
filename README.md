# Time for a rethink: time sub-sampling methods in disparity-through-time analyses.
Author(s): [Thomas Guillerme](https://github/TGuillerme) and [Natalie Cooper](mailto:natalie.cooper.@nhm.ac.uk)  

This repository contains all the code and data used in the manuscript [Link to final published pdf will be here]().

To cite the paper: 
> Thomas Guillerme \& Natalie Cooper. 2018. Time for a rethink: time sub-sampling
methods in disparity-through-time analyses. [Palaeontology - hopefully!].

To cite this repo: 
> Thomas Guillerme \& Natalie Cooper. 2018. GitHub: nhcooper123/time-slice: Release for publication. Zenodo. http://doi.org/10.5281/zenodo.1172000.

[![DOI](https://zenodo.org/badge/102496441.svg)](https://zenodo.org/badge/latestdoi/102496441)

![alt text](https://github.com/nhcooper123/time-slice/raw/master/stratigraphiccake.png)

> Many thanks to Dr Jones for the image :)


## Data
These analyses are based on data from four previous studies. 
They can be accessed from the links below, but for reproducibility purposes the data can also be found in the `data/` folder in this repo.

* [Beck2014](http://rspb.royalsocietypublishing.org/content/281/1793/20141278)
* [Brusatte2014](http://datadryad.org/resource/doi:10.5061/dryad.84t75) 
* [Bapst2016](http://datadryad.org/resource/doi:10.5061/dryad.n2g80) 
* [Wright2017](http://datadryad.org/resource/doi:10.5061/dryad.6hb7j) 

If you use the data **please cite the Dryad data packages and papers they originally came from**.

## Analyses
All code used to run analyses and make figures and tables is included in the `analyses/` folder. Before starting remember to either set your working directory to the **time-slice** folder on your computer, or open an RStudio project from that folder. 
All of these files are in RMarkdown format, so you can click the `Knit` button in RStudio and create HTML versions with the code and output.
This code is not as tidy or functionified as we would like as we were a bit pushed for time.

* **01-extract-data-for-analyses.Rmd** This script takes the raw data from the papers and processes them for use in the disparity-through-time analyses. This includes manipulating matrices and phylogenetic trees. All data is saved so if you're pushed for time or computational power we recommend skipping this step and using the data in the `data/processed` folder.
* **02-time-slice-analyses.Rmd** This script performs disparity-through-time analyses on the processed data according to several scenarios and methods described in the paper.
* **03-statistical-analyses.Rmd** This script uses the outputs from **02-time-slice-analyses.Rmd** to determine how the different time sub-sampling methods influence biological conclusions about the datasets. It tests for systematic differences in disparity-through-time (paired Wilcoxon tests), changes in disparity peaks, and changes in conclusions about the effects of mass extinctions on disparity.
* **04-table-datasets.Rmd** Reproduces Table 1 (except the references).
* **05-figure-dtt.Rmd** Reproduces Figure 3.
* **06-table-wilcox.Rmd** Reproduces Table 2, and Table A1. 
* **07-figure-peaks.Rmd** Reproduces Figure 4.
* **08-figure-extinction.Rmd** Reproduces Figure 5.
* **09-appendix-trees-figures.Rmd** Builds plots of phylogenies using the R package `strap` to add stratigraphic information (Appendix S1: Figures A1-A4).
* **10-appendix-figure-dtt.Rmd** Reproduces Appendix S2: Figures A1 and A2.
* **11-appendix-figure-peaks.Rmd** Reproduces Appendix S2: Figures A3 and A4.
* **12-appendix-disparity-tables.Rmd** Produces the detailed disparity outputs tables. Currently not in the appendices as they are around 100 pages long!

## Other folders
* `functions/` contains functions required by the code in the `analyses/` folder.
* `outputs/` contains all the outputs from the code in the `analyses/` folder except for the figures and tables which can be found in `manuscript/figures` and `manuscript/tables`.
* `manuscript/` contains the LaTeX version of the manuscript and appendices, plus style template, bibliography, figures and tables.

## Checkpoint for reproducibility
To rerun all the code with packages as they existed on CRAN at time of our analyses we recommend using the `checkpoint` package, and running this code prior to the analysis:

```{r}
checkpoint("2018-01-06")
```

## Session Info
For reproducibility purposes, here is the output of `devtools::session_info()` used to perform the analyses in the publication.

	Session info ----------------------------------------------------------------------------

	 setting  value                       
	 version  R version 3.4.3 (2017-11-30)
	 system   x86_64, darwin15.6.0        
	 ui       RStudio (1.1.383)           
	 language (EN)                        
	 collate  en_IE.UTF-8                 
	 tz       Europe/Dublin               
	 date     2018-01-06                  

	Packages --------------------------------------------------------------------------------
	 package           * version  date       source                              
	 ade4                1.7-10   2017-12-15 cran (@1.7-10)                      
	 animation           2.5      2017-03-30 CRAN (R 3.4.0)                      
	 ape               * 5.0      2017-10-30 CRAN (R 3.4.2)                      
	 assertthat          0.2.0    2017-04-11 CRAN (R 3.4.0)                      
	 base              * 3.4.3    2017-12-07 local                               
	 bindr               0.1      2016-11-13 CRAN (R 3.4.0)                      
	 bindrcpp            0.2      2017-06-17 CRAN (R 3.4.0)                      
	 broom             * 0.4.3    2017-11-20 CRAN (R 3.4.3)                      
	 cellranger          1.1.0    2016-07-27 CRAN (R 3.4.0)                      
	 Claddis           * 0.2      2017-09-05 Github (TGuillerme/Claddis@43c717b) 
	 cli                 1.0.0    2017-11-05 CRAN (R 3.4.2)                      
	 cluster             2.0.6    2017-03-10 CRAN (R 3.4.3)                      
	 clusterGeneration   1.3.4    2015-02-18 CRAN (R 3.4.0)                      
	 coda                0.19-1   2016-12-08 CRAN (R 3.4.0)                      
	 colorspace          1.3-2    2016-12-14 CRAN (R 3.4.0)                      
	 combinat            0.0-8    2012-10-29 CRAN (R 3.4.0)                      
	 compiler            3.4.3    2017-12-07 local                               
	 crayon              1.3.4    2017-09-16 CRAN (R 3.4.1)                      
	 datasets          * 3.4.3    2017-12-07 local                               
	 deSolve             1.20     2017-07-14 CRAN (R 3.4.1)                      
	 devtools          * 1.13.4   2017-11-09 CRAN (R 3.4.2)                      
	 digest              0.6.13   2017-12-14 CRAN (R 3.4.3)                      
	 dispRity          * 0.5      2017-12-20 Github (TGuillerme/dispRity@80f3e97)
	 dplyr             * 0.7.4    2017-09-28 CRAN (R 3.4.2)                      
	 expm                0.999-2  2017-03-29 CRAN (R 3.4.0)                      
	 fastmatch           1.1-0    2017-01-28 CRAN (R 3.4.0)                      
	 forcats           * 0.2.0    2017-01-23 CRAN (R 3.4.0)                      
	 foreign             0.8-69   2017-06-22 CRAN (R 3.4.3)                      
	 gdata               2.18.0   2017-06-06 CRAN (R 3.4.0)                      
	 geiger              2.0.6    2015-09-07 CRAN (R 3.4.0)                      
	 geometry            0.3-6    2015-09-09 cran (@0.3-6)                       
	 geoscale          * 2.0      2015-05-14 CRAN (R 3.4.0)                      
	 ggplot2           * 2.2.1    2016-12-30 CRAN (R 3.4.0)                      
	 glue                1.2.0    2017-10-29 CRAN (R 3.4.2)                      
	 graphics          * 3.4.3    2017-12-07 local                               
	 grDevices         * 3.4.3    2017-12-07 local                               
	 grid              * 3.4.3    2017-12-07 local                               
	 gridExtra         * 2.3      2017-09-09 CRAN (R 3.4.1)                      
	 gtable              0.2.0    2016-02-26 CRAN (R 3.4.0)                      
	 gtools              3.5.0    2015-05-29 CRAN (R 3.4.0)                      
	 haven               1.1.0    2017-07-09 CRAN (R 3.4.1)                      
	 hms                 0.4.0    2017-11-23 CRAN (R 3.4.3)                      
	 htmltools           0.3.6    2017-04-28 cran (@0.3.6)                       
	 htmlwidgets         0.9      2017-07-10 cran (@0.9)                         
	 httpuv              1.3.5    2017-07-04 cran (@1.3.5)                       
	 httr                1.3.1    2017-08-20 CRAN (R 3.4.1)                      
	 igraph              1.1.2    2017-07-21 CRAN (R 3.4.1)                      
	 jsonlite            1.5      2017-06-01 CRAN (R 3.4.0)                      
	 knitr               1.17     2017-08-10 CRAN (R 3.4.1)                      
	 lattice             0.20-35  2017-03-25 CRAN (R 3.4.3)                      
	 lazyeval            0.2.1    2017-10-29 CRAN (R 3.4.2)                      
	 lubridate           1.7.1    2017-11-03 CRAN (R 3.4.2)                      
	 magic               1.5-6    2013-11-20 cran (@1.5-6)                       
	 magrittr            1.5      2014-11-22 CRAN (R 3.4.0)                      
	 maps              * 3.2.0    2017-06-08 CRAN (R 3.4.0)                      
	 MASS                7.3-47   2017-02-26 CRAN (R 3.4.3)                      
	 Matrix              1.2-12   2017-11-20 CRAN (R 3.4.3)                      
	 memoise             1.1.0    2017-04-21 CRAN (R 3.4.0)                      
	 methods           * 3.4.3    2017-12-07 local                               
	 mgcv                1.8-22   2017-09-24 CRAN (R 3.4.3)                      
	 mime                0.5      2016-07-07 CRAN (R 3.4.0)                      
	 mnormt              1.5-5    2016-10-15 CRAN (R 3.4.0)                      
	 modelr              0.1.1    2017-07-24 CRAN (R 3.4.1)                      
	 msm                 1.6.5    2017-12-05 CRAN (R 3.4.3)                      
	 munsell             0.4.3    2016-02-13 CRAN (R 3.4.0)                      
	 mvtnorm             1.0-6    2017-03-02 CRAN (R 3.4.0)                      
	 nlme                3.1-131  2017-02-06 CRAN (R 3.4.3)                      
	 numDeriv            2016.8-1 2016-08-27 CRAN (R 3.4.0)                      
	 paleotree           3.0.0    2017-10-21 cran (@3.0.0)                       
	 parallel            3.4.3    2017-12-07 local                               
	 permute             0.9-4    2016-09-09 CRAN (R 3.4.0)                      
	 phangorn            2.3.1    2017-11-01 cran (@2.3.1)                       
	 phyclust            0.1-22   2017-12-02 cran (@0.1-22)                      
	 phytools          * 0.6-44   2017-11-12 CRAN (R 3.4.2)                      
	 pkgconfig           2.0.1    2017-03-21 CRAN (R 3.4.0)                      
	 plotrix             3.7      2017-12-07 CRAN (R 3.4.3)                      
	 plyr                1.8.4    2016-06-08 CRAN (R 3.4.0)                      
	 psych               1.7.8    2017-09-09 CRAN (R 3.4.3)                      
	 purrr             * 0.2.4    2017-10-18 CRAN (R 3.4.2)                      
	 quadprog            1.5-5    2013-04-17 CRAN (R 3.4.0)                      
	 R6                  2.2.2    2017-06-17 CRAN (R 3.4.0)                      
	 Rcpp                0.12.14  2017-11-23 CRAN (R 3.4.3)                      
	 readr             * 1.1.1    2017-05-16 CRAN (R 3.4.0)                      
	 readxl              1.0.0    2017-04-18 CRAN (R 3.4.0)                      
	 reshape2            1.4.3    2017-12-11 CRAN (R 3.4.3)                      
	 rgl               * 0.98.22  2017-12-13 CRAN (R 3.4.3)                      
	 rlang               0.1.4    2017-11-05 CRAN (R 3.4.2)                      
	 rstudioapi          0.7      2017-09-07 CRAN (R 3.4.1)                      
	 rvest               0.3.2    2016-06-17 CRAN (R 3.4.0)                      
	 scales              0.5.0    2017-08-24 CRAN (R 3.4.1)                      
	 scatterplot3d       0.3-40   2017-04-22 CRAN (R 3.4.0)                      
	 shiny               1.0.5    2017-08-23 CRAN (R 3.4.1)                      
	 snow                0.4-2    2016-10-14 cran (@0.4-2)                       
	 splines             3.4.3    2017-12-07 local                               
	 stats             * 3.4.3    2017-12-07 local                               
	 strap             * 1.4      2014-11-05 cran (@1.4)                         
	 stringi             1.1.6    2017-11-17 CRAN (R 3.4.2)                      
	 stringr           * 1.2.0    2017-02-18 CRAN (R 3.4.0)                      
	 subplex             1.4-1    2017-07-18 CRAN (R 3.4.1)                      
	 survival            2.41-3   2017-04-04 CRAN (R 3.4.3)                      
	 tibble            * 1.3.4    2017-08-22 CRAN (R 3.4.1)                      
	 tidyr             * 0.7.2    2017-10-16 CRAN (R 3.4.2)                      
	 tidyverse         * 1.2.1    2017-11-14 CRAN (R 3.4.2)                      
	 tools               3.4.3    2017-12-07 local                               
	 utils             * 3.4.3    2017-12-07 local                               
	 vegan               2.4-5    2017-12-01 cran (@2.4-5) 
	 viridis           * 0.4.0    2017-03-27 CRAN (R 3.4.0)                      
     viridisLite       * 0.2.0    2017-03-24 CRAN (R 3.4.0)                       
	 withr               2.1.1    2017-12-19 CRAN (R 3.4.3)                      
	 xml2                1.1.1    2017-01-24 CRAN (R 3.4.0)                      
	 xtable            * 1.8-2    2016-02-05 cran (@1.8-2)                       
	 yaml                2.1.16   2017-12-12 CRAN (R 3.4.3) 