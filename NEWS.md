# isoreader 1.3.0

## Major changes

 - functions for previously deprecated `.rda` file format removed
 - R version (>= 4.0.0) and dependency version requirements clarified
 - dependencies simplified (`openxlsx`, `feather`, `xml2` and `rhdf5` are now optional extensions)

## Bug fixes

 - dependency on pandoc removed

# isoreader 1.2.3

## Major Features

#### Reading isotope ratio mass spectrometry (IRMS) data files

This package currently supports reading the following raw data files:

 - continuous flow data files from mass spectrometers run by Isodat and IonOS software (`?iso_read_continuous_flow`)
 - dual inlet data files from mass spectrometers run by Isodat and Nu software (`?iso_read_dual_inlet`)
 - scan data files from mass spectrometers run by Isodat software (`?iso_read_scan`)

#### Aggregating data from files

This package provides the following data aggregation and data processing functionality for all supported data files:

 - aggregating file information including sequence line data (`?iso_get_file_info`)
 - aggregating raw mass spectrometric data (`?iso_get_raw_data`)
 - aggregating mass spectrometric background data (`?iso_get_bgrd_data`)
 - aggregating vendor-specific data tables if provided in raw file formats (`?iso_get_vendor_data_table`)
 - aggregating information on detector resistors if included in raw file formats (`?iso_get_resistors`)
 - aggregating information on internal isotope standards if included in raw file formats (`?iso_get_standards`)
 - processing file information with tidyverse-equivalent `select`, `rename`, `mutate` and `filter` functions (`?iso_filter_files`, `?iso_select_file_info`, `?iso_rename_file_info`, `?iso_mutate_file_info`)
 - adding file information with `?iso_add_file_info`to simplify sequential join operations


#### Exporting information

This package provides the following data export functionality for all supported data files:

 - export to open Excel (.xslx) with `?iso_export_to_excel`
 - export to the Python/R cross-over feather file format with `?iso_export_to_feather`
 