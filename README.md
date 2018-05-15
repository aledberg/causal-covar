# causal-covar repository



Data and R-code related to the causal covariability project. This project site is under construction.

---

## Data source
The data used in this project originates from the enlistment-process for compulsory military service in Sweden. The data-set covers persons enlisted during the years 1969-1996 and contains a large number of measurments made on young Swedish men. The full data-set was made public under Creative Commons CCZero licence by the [Swedsh National Archives](https://riksarkivet.se/startpage) (Riksarkivet) and is available in the archive [INSARK](https://riksarkivet.se/psidata#INSARK). The data used in the paper can be downloaded directly from this page and constitutes a small subset of the original data-set. 


---

## How-to process the data


The code is organized in a set of R-scripts as follows:

* loadInsarkData.R:  Contains the code to read data into R and do some preliminary case selection

* makeFigure.R: Contains code to reproduce Figure 4 in the paper

* runModelForPaper.R:  Contains code to fit the probability models. This script depends on one auxillary
script (fitModelForPaper.R) and two files with initial values for the numerical optimization (optimal_params12.txt and optimal_params13.txt)

* bootstrapForPaper.R Contains code to do the bootstrap resampling needed to estimate the confidence intervals of parameter values.


Example:

To generate Figure 4 do the following: 

* clone the repository (git clone https://github.com/aledberg/causal-covar)

* start up R in the cloned directory

* at the R prompt execute source("makeFigure.R")

* wait a minute or two, the error bars (confidence intervals) in the plot are estimated by bootstrapping
