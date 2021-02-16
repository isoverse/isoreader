# tools for active package development

.PHONY: check auto_test auto_test_all

all: docu check

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

# test package functionality without all example files (= as if on CRAN)
check:
	R -q -e "devtools::check(env_vars = c())"

auto_test:
	R -q -e "rm(list = ls()); Sys.setenv(NOT_CRAN = \"false\"); testthat::auto_test_package()"

# test with all example files (= as if not on CRAN)
auto_test_all:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
