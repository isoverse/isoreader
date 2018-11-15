# tools for active package development

all: vignettes docu check

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

vignettes:
	Rscript -e "devtools::build_vignettes()"

check:
	Rscript -e "devtools::check()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
