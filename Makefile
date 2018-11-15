# tools for active package development

all: vignettes docu check

docu:
	rm -f -r vignettes/cache
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_site()"
	rm -f -r vignettes/cache
	rm -f vignettes/*.feather
	rm -f vignettes/*.xlsx
	rm -f vignettes/*.rda
	rm -f vignettes/*.rds

vignettes:
	Rscript -e "devtools::build_vignettes()"

check:
	Rscript -e "devtools::check()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
