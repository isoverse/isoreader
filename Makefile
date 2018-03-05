# tools for active package development

all: docu check

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_site()"

check:
	Rscript -e "devtools::check()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
