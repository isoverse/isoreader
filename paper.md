---
title: 'Isoreader: An R package to read stable isotope data files for reproducible research'
tags:
  - R
  - stable isotopes
  - earth science
  - ecology
  - isotope ratio mass spectrometry
  - reproducible research
authors:
  - name: Sebastian Kopf^[corresponding author]
    orcid: 0000-0002-2044-0201
    affiliation: 1
  - name: Brett Davidheiser-Kroll
    orcid: 0000-0002-6153-7851
    affiliation: 1
  - name: Ilja Kocken
    orcid: 0000-0003-2196-8718
    affiliation: 2
affiliations:
 - name: Department of Geological Sciences, University of Colorado Boulder
   index: 1
 - name: Department of Earth Sciences, Utrecht University, the Netherlands
   index: 2
date: 11 November 2020
bibliography: paper.bib
---

# Summary

The measurement and interpretation of the stable isotope composition of any material or molecule has widespread application in disciplines ranging from the earth sciences to ecology, anthropology, and forensics. The naturally occurring differences in the abundance of the stable isotopes of carbon, nitrogen, oxygen, and many other elements provide valuable insight into environmental conditions and sources, fluxes, and mechanisms of material transfer. Because isotopic variations in nature are very small, the measurement itself requires cutting edge analytical instrumentation using isotope ratio mass spectrometry (IRMS) as well as rigorous data reduction procedures for calibration and quality control. The `isoreader` package implements an easily extendable interface for IRMS data from common instrument vendor file formats and thus enables the reading and processing of stable isotope data directly from the source. This provides a foundational tool for platform-independent, efficient and reproducible data reduction.

# Statement of need

Reproducible data processing is a key prerequisite for efficient data exchange, methodological progress, and productive discourse in scientific research. However, generating a record of every step of a data processing pipeline in a format that is transparent and easy to understand is not an easy task. In the world of stable isotopes, many data processing steps require proprietary software for data access and depend on point-and-click interactions. This makes it challenging to share and discuss one’s approach, review others’ and compare calculations and datasets across laboratories. Moreover, it severely restricts opportunities for iteration, exchange of ideas, and data aggregation.

The `isoreader` package enables efficient and reproducible reading and processing of stable isotope data directly from the data files no matter which operating system (Windows, Mac, Linux). It is already being used for stable isotope data processing in several laboratories and recent publications including @Silverman2019, [@Cheng2019], [@Ingalls2020], and [@Suarez2020]. The `isoreader` package was designed to be easily extendable with readers for new file formats, and provides data export functionality to Python using the shared R/Python feather file format. This will enable the development, sharing and vetting of open-source data processing pipelines for stable isotope data across scientific disciplines.

# Acknowledgements

We thank Max Lloyd for his valuable contributions to parsing dual inlet file formats. We thank Seth Newsome and the executive committee of the IsoBank initiative for organizing a workshop that helped improve this software. We also thank all the laboratories that shared test files including the Bender, Bergmann, Bradley, Eiler, Kim, Ono, Pearson, Sessions, Sigman, Snell, and Ziegler labs, as well as the Stable Isotope Facilities at the University of California Davis, University of Colorado, University of New Brunswick, University of New Mexico, University of Ottawa, University of Utah, University of Washington, University of Wyoming, and the United States Geological Survey. Lastly, we thank the members of the stable isotope community on the ISOGEOCHEM listserv for their contributions and bug reports that improved this software. This project was supported, in part, by grants to SHK from the National Science Foundation (EAR 1928303) and the University of Colorado Boulder.

# References
