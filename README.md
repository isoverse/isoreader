
<!-- README.md is generated from README.Rmd. Please edit that file -->
isoreader
=========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoreader)](https://cran.r-project.org/package=isoreader) [![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.9.18.9000-orange.svg?style=flat-square)](/commits) [![Last-Update](https://img.shields.io/badge/updated-2018--06--04-yellowgreen.svg)](/commits) [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://kopflab.github.io/isoreader/) [![Build Status](https://travis-ci.org/KopfLab/isoreader.svg?branch=master)](https://travis-ci.org/KopfLab/isoreader) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/KopfLab/isoreader?branch=master&svg=true)](https://ci.appveyor.com/project/KopfLab/isoreader)

About
-----

This package is intended as a unified one-stop command line interface to all common IRMS (isotope ratio mass spectrometry) file formats used in stable isotope geochemistry. It is an extension and highly stream-lined re-implemention of the proof-of-concept [isoread](https://github.com/sebkopf/isoread) package and is designed to fit into a larger framework of IRMS data tools that includes the web-based graphical user interface package [isoviewer](https://github.com/KopfLab/isoviewer) and the data processing pipeline [isoprocessor](https://github.com/KopfLab/isoprocessor) (in development).

[isoreader](https://kopflab.github.io/isoreader/) enables the reading and processing of stable isotope data directly from the data files and thus provides a tool for platform-independent (Windows, Mac, Linux), efficient and reproducible data reduction. Although implemented in R, it can be used in both RMarkdown as well as Jupyter data processing notebooks and also provides functionality for easy export to Python using the shared R/Python feather file format. At present, it can read most Thermo dual inlet (.did, .caf) and continuous flow (.dxf, .cf) data files as well as Elementar continuous flow data archives (.iarc) with additional extensions for other file formats in the works. Due to the dynamic implementation and design based on the popular [tidyverse](https://www.tidyverse.org/) style of R programming, isoreader is easily extendable, takes care of error catching to avoid pipeline breaks due to problems encountered in source data files (modeled after [readr](https://readr.tidyverse.org/)) and works great with [tidyverse](https://www.tidyverse.org/) packages such as [tidyr](https://tidyr.tidyverse.org/), [dplyr](https://dplyr.tidyverse.org/) and [ggplot](https://ggplot2.tidyverse.org/). The release candidate for [isoreader](http://www.github.com/KopfLab/isoreader) version 1.0 is currently going through beta testing and will be submitted to the Comprehensive R Archive Network (CRAN) later this year. No major functionality changes are expected for version 1 and you can install and use the beta version using the `devtools` package.

Installation
------------

You can install isoreader from github with the devtools package (version &gt; 1.13.2 required for bioconductor support).

``` r
# install.packages("devtools") # only needed once
devtools::install_github("KopfLab/isoreader")
```

Functionality
-------------

Currently supported file types:

| extension | format                                     | type            |
|:----------|:-------------------------------------------|:----------------|
| .did      | Isodat Dual Inlet file format (newer)      | Dual Inlet      |
| .caf      | Isodat Dual Inlet file format (older)      | Dual Inlet      |
| .di.rda   | Isoreader R Data Archive                   | Dual Inlet      |
| .cf       | Isodat Continuous Flow file format (older) | Continuous flow |
| .dxf      | Isodat Continuous Flow file format (newer) | Continuous flow |
| .iarc     | IonOS Continous Flow data archieve         | Continuous flow |
| .cf.rda   | Isoreader R Data Archive                   | Continuous flow |

-   for a full reference of all available functions, see the **[Function Reference](https://kopflab.github.io/isoreader/reference/)**
-   for an example of how to work with continuos flow data files, see the vignette on **[Continuous Flow](https://kopflab.github.io/isoreader/articles/continuous_flow.html)**
-   for an example of how to work with dual inlet data files, see the vignette on **[Dual Inlet](https://kopflab.github.io/isoreader/articles/dual_inlet.html)**

Troubleshooting
---------------

If you run into a file format that is not currently supported or any issues with supported formats, please file a request/bug report in the [issue tracker](https://github.com/kopflab/isoreader/issues). Likewise if you run into any unexpected behaviour or uncaught errors. Most isoreader functionality is continuously tested on Unix and Windows systems using [Travis](https://travis-ci.org/) and [AppVeyor](https://ci.appveyor.com/), respectively. This makes it possible to ensure proper functionality and catch issues quickly, however, sometimes something slips through or is not yet automatically tested. We try to make sure to fix such errors as soon as possible but ask for patience due to the small develoment team. If you have the skills and are willing to fix problems yourself, that's great, please take a look at the development section below.

Development
-----------

If you are interested in helping with development, that's fantastic! Please fork the repository and branch off from the [dev branch](https://github.com/KopfLab/isoreader/tree/dev) since it contains the most up-to-date development version of [isoreader](https://kopflab.github.io/isoreader/). Make sure to write [`testthat` tests](http://r-pkgs.had.co.nz/tests.html) for your work (stored in the tests/testthat directory). All tests can be run automatically and continuously during development to make it easier to spot any code problems on the go. The easiest way to run them is by running `make auto_test` in the [isoreader](https://kopflab.github.io/isoreader/) directory from command line (it will test everything automatically in a completely separate R session).

Open Source
-----------

[isoreader](https://kopflab.github.io/isoreader/) is and will always be fully open-source (i.e. free as in 'freedom' and free as in 'free beer') and is provided as is. The source code is released under GPL-2.
