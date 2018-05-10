# tools for active package development

all: docu check

docu:
	rm -f -r vignettes/cache
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_site()"
	rm -f -r vignettes/cache
	rm -f vignettes/*.feather
	rm -f vignettes/*.xlsx
	rm -f vignettes/*.rda

check:
	Rscript -e "devtools::check()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
