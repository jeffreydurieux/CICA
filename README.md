# Clusterwise Independent Component Analysis R package

[![GPLv3 License](https://img.shields.io/badge/License-GPL%20v3-yellow.svg)](https://opensource.org/license) ![GitHub R package version](https://img.shields.io/github/r-package/v/jeffreydurieux/CICA) ![CRAN/METACRAN](https://img.shields.io/cran/v/CICA) [![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/CICA?color=blue)](https://r-pkg.org/pkg/CICA) ![GitHub contributors](https://img.shields.io/github/contributors/jeffreydurieux/CICA)

## Version notes

#### Version of CICA on CRAN notes:

-   CRAN v0.1.0: CICA version with ALS random start procedure

#### Version of CICA on GitHub:

Download the development version of CICA using the devtools package: `devtools::install_github('jeffreydurieux/CICA')`

This version contains:

-   R v0.1.0: CICA version with ALS random start procedure

-   R v0.2.1: CICA with (pseudo-) rational start options

    -   v0.2.0: modified RV matrix computations (computeRVmat()). A (dis) similarity matrix is computed between a list of input matrices. This is based on the two-step clustering procedure from [Durieux & Wilderjans (2019).](https://link.springer.com/article/10.1007/s41237-019-00086-4)
    -   v0.2.0: FindRationalStarts() function. This function applies the two-step procedure using several hierarchical clustering methods in order to find rational starts for the ALS algorithm for CICA. Cluster perturbation options are also included. This function returns an object of class `rstarts`. This object can be passed to the CICA main function.
    -   v0.2.0: These options are also directly included in the CICA main function.
    -   v0.2.1: Update of example data. Added a single example data set from the simulation design of [Durieux & Wilderjans (2019).](https://link.springer.com/article/10.1007/s41237-019-00086-4) It contains 60 subjects and original cluster specific components and the true simulated clustering is added.

-   R v0.3.0 CICA version with multiple CICA models

-   R v1.0.0 CICA version with all working functionalities. This version is also available on CRAN. This package version includes the papayar archived files that were made by John Muschelli.

-   R v1.1.0 CICA version with a fast EVD based estimation procedure. This results in an equal (or similar) clustering. Use the final clustering to seed the CICA (using method = 'fastICA') to extract independent components. 