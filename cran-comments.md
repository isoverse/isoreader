This is a resubmission of a new package. 

The following issue identified by Gregor Seyer was addressed in this resubmission:

> We do not need "| file LICENSE" and the file as these are part of R. This is only needed in case of attribution requirements or other possible restrictions. Hence please omit it.

Removed `LICENSE` file and corresponding note in the `DESCRIPTION`.

## Test environments

* Local OS X install, R 4.0.2
* Mac OS X 10.15.6 (on GitHub), R 4.0.2 (release), R 4.1.0 (devel)
* Ubuntu 16.04 (on GitHub), R 4.0.2 (release)
* Windows Server 2019 (on GitHub), R 4.0.2 (release)
* Win-builder (release and devel)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Sebastian Kopf <sebastian.kopf@colorado.edu>'
  New submission
  
## Downstream dependencies

There are currently no downstream dependencies for this package.
