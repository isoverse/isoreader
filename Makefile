# tools for active package development

.PHONY: check auto_test auto_test_all

all: docu check

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

check:
	R -q -e "devtools::check(env_vars = c())"

# test package functionality without all example files
auto_test:
	R -q -e "rm(list = ls()); options("isoreader.run_file_tests" = FALSE); testthat::auto_test_package()"

# test all example files
auto_test_all:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
