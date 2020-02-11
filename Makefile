# tools for active package development

all: vignettes docu check

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

vignettes:
	Rscript -e "devtools::build_vignettes()"

check:
	Rscript -e "devtools::check()"

# test package functionality without all example files
auto_test:
	R -q -e "rm(list = ls()); options("isoreader.run_file_tests" = FALSE); testthat::auto_test_package()"

# test all example files
auto_test_all:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
