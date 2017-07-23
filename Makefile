# tools for active package development

vignettes:
	Rscript -e "devtools::build_vignettes()"

check:
	Rscript -e "devtools::check()"

document:
	Rscript -e "devtools::check_doc()"

auto_test:
	R -q -e "rm(list = ls()); testthat::auto_test_package()"
