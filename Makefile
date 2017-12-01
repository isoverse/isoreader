# tools for active package development

vignettes:
	Rscript -e "devtools::build_vignettes()"

check:
	Rscript -e "devtools::check()"

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
	Rscript -e "pkgdown::build_site()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
