
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-archived-brightgreen.svg)](https://img.shields.io/badge/lifecycle-archived-brightgreen.svg)
[![Travis-CI Build Status](https://travis-ci.org/koalaverse/anomalyDetection.svg?branch=master)](https://travis-ci.org/koalaverse/anomalyDetection)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/bradleyboehmke/anomalyDetection?branch=master&svg=true)](https://ci.appveyor.com/project/bradleyboehmke/anomalyDetection)
[![codecov](https://codecov.io/gh/koalaverse/anomalyDetection/branch/master/graph/badge.svg)](https://codecov.io/gh/koalaverse/anomalyDetection)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/anomalyDetection)](http://cranlogs.r-pkg.org/badges/grand-total/anomalyDetection)

anomalyDetection <img src="tools/anomalyDetection-logo.png" align="right" width="120" height="139" />
=====================================================================================================

`anomalyDetection` implements procedures to aid in detecting network log anomalies. By combining various multivariate analytic approaches relevant to network anomaly detection, it provides cyber analysts efficient means to detect suspected anomalies requiring further evaluation.

Installation
------------

You can install `anomalyDetection` two ways.

-   Using the latest released version from CRAN:

<!-- -->

    install.packages("anomalyDetection")

-   Using the latest development version from GitHub:

<!-- -->

    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }

    devtools::install_github("koalaverse/anomalyDetection", build_vignettes = TRUE)

Learning
--------

To get started with `anomalyDetection`, read the intro [vignette](https://cran.r-project.org/web/packages/anomalyDetection/vignettes/Introduction.html): `vignette("Introduction", package = "anomalyDetection")`. This will provide a thorough introduction to the functions provided in the package.

References
----------

Gutierrez, R.J., Boehmke, B.C., Bauer, K.W., Saie, C.M. & Bihl, T.J. (2017) "`anomalyDetection`: Implementation of augmented network log anomaly detection procedures." The R Journal, 9(2), 354-365. [link](https://journal.r-project.org/archive/2017/RJ-2017-039/index.html)
