# Time for a rethink: time sub-sampling methods in disparity-through-time analyses.
Author(s): [Thomas Guillerme](https://github/TGuillerme) and [Natalie Cooper](mailto:natalie.cooper.@nhm.ac.uk)  

This repository contains all the code and data used in the manuscript [Link to final published pdf will be here]().

To cite the paper: 
> Thomas Guillerme \& Natalie Cooper. 2018. Time for a rethink: time sub-sampling
methods in disparity-through-time analyses. [Palaeontology].

To cite this repo: 
> Natalie Cooper \& Thomas Guillerme. 2018. GitHub: nhcooper123/time-slice: Release for publication. DOI to be added.

[![DOI](https://zenodo.org/badge/98415211.svg)]()

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
* **05-figure-dtt.Rmd** Reproduces Figure 2.
* **06-table-wilcox.Rmd** Reproduces Table 2, and Table A1. 
* **07-figure-peaks.Rmd** Reproduces Figure 3.
* **08-figure-extinction.Rmd** Reproduces Figure 4.
* **09-appendix-trees-figures.Rmd** Builds plots of phylogenies using the R package `strap` to add stratigraphic information.
* **10-appendix-figure-dtt.Rmd** Reproduces Figures AX and AX.
* **11-appendix-figure-peaks.Rmd** Reproduces Figures AX and AX.

## Session Info
For reproducibility purposes, here is the output of `devtools:session_info()` used to perform the analyses in the publication.

TO ADD

## Checkpoint for reproducibility
To rerun all the code with packages as they existed on CRAN at time of our analyses we recommend using the `checkpoint` package, and running this code prior to the analysis:

```{r}
checkpoint("2017-12-17")
```