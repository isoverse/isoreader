Update of an existing CRAN package to address problems during vignette re-building that have arisen on systems without pandoc (e.g Solaris) as pointed out by Prof. Brian Ripley in a message on Feb. 12 2021 to the maintainers of all affected packages. The package has been tested on GitHub without pandoc and all vignette errors have been addressed (mostly stemming from the use of `rmarkdown::paged_table()`).

## Test environments

* Local OS X install, R 4.0.2 (release)
* Mac OS X 10.15.6 (on GitHub), R 4.0.2 (release)
* Ubuntu 16.04 (on GitHub), R 4.0.2 (release)
* Windows Server 2019 (on GitHub), R 4.0.2 (release)
* Win-builder (release and devel)

## R CMD check results

There were no ERRORs, no WARNINGs, and no NOTEs. 

## Downstream dependencies

There are currently no downstream dependencies for this package.
