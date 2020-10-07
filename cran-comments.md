This is a new package.

## Test environments

* Local OS X install, R 4.0.2
* Mac OS X 10.15.6 (on GitHub), R 4.0.2 (release), R 4.1.0 (devel)
* Ubuntu 16.04 (on GitHub), R 4.0.2 (release)
* Windows Server 2019 (on GitHub), R 4.0.2 (release)
* Win-builder (release and devel)

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Sebastian Kopf <sebastian.kopf@colorado.edu>'
  New submission
  Possibly mis-spelled words in DESCRIPTION: spectrometry (3:48)
  
This is the correct spelling of spectrometry.

> checking installed package size ... NOTE
    installed size is  5.1Mb
    sub-directories of 1Mb or more:
      extdata   3.4Mb

This package provides an interface to various file formats commonly used in the scientific field of isotope geochemistry. The `extdata` folder holds 12 example files for 8 different file formats adding up to 3.4Mb. These files are used in the vignettes and function examples and are included in the installed package to make it easier for users of this package to explore its functionality. Note: this note did not occur with win-builder.

## Downstream dependencies

There are currently no downstream dependencies for this package.