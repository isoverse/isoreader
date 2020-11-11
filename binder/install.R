# Bioconductor packages not available on MRAN
install.packages("BiocManager")
BiocManager::install("rhdf5")

# Package itself
install.packages("isoreader")

# Additional packages (in suggests for isoreader 1.2.7)
install.packages("readxl")

# Packages for knitting
install.packages(c("rmarkdown", "caTools", "bitops"))
