# Bioconductor packages not available on MRAN
install.packages("BiocManager")
BiocManager::install("rhdf5")

# Package itself
install.packages("isoreader")

# Packages for knitting
install.packages(c("rmarkdown", "caTools", "bitops"))
