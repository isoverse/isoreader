# tools for active package development

.PHONY: check auto_test auto_test_all

all: docu check

docu:
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"

# test package functionality without example files (= as if on CRAN)
check:
	R -q -e "message('\nINFO: running check as if on CRAN\n'); devtools::check(env_vars = c())"

# tests without file tests (= as if on CRAN)
auto_test:
	R -q -e "rm(list = ls()); options("isoreader.skip_file_tests" = TRUE); message('\nINFO: running tests as if on CRAN\n'); testthat::auto_test_package()"

# tests with file test
auto_test_all:
	R -q -e "rm(list = ls()); message('\nINFO: running all tests (as if NOT on CRAN)\n'); testthat::auto_test_package()"

# check code complexity
count:
	scc R tests --include-ext r --sort lines
	
count_all:
	scc . --sort lines