# JGR_B_Bigelow22_23
---------------------------------------------------------------------
## Overview
Dataset Title: Drought response in three conifer species detected by sap flow and proximal thermal remote 
sensing 
This repository contains R scripts and associated data processing workflows used to analyze sap flow dynamics (cm h⁻¹) and ecosystem fluxes — ET (mm day⁻¹) and GPP (µmol m⁻² s⁻¹) — under different environmental conditions, including VPD (kPa) and soil moisture (v/v %), across different years and seasons in a mixed conifer forest. The analysis also incorporates canopy temperature (°C) and the difference between canopy and air temperature (ΔT), in addition to merging sap flow data from multiple species, flux tower data, and thermal data, and generating publication-quality figures and tables.

Investigators: Daphna Uni, Russell L. Scott, Mostafa Javadian, Joel Biederman, Matthew P. Dannenberg, William K. Smith

Point of Contact:  daphnau@arizona.edu University of Arizona 

## Data availability
All data files and the large datasets Mt.bigelow flux tower (AMF_US-MtB_BASE_HH_4-5) are deposited in Zenodo: [DOI link here](10.5281/zenodo.15802813)


## Repository contents

project-directory/
│
├── Data/                        # All data files needed for the analysis

├── Script/                     # R script for cleaning and analysis
                                # R script for the multi-year analysis

└── README.md

- `/Data/` — directory containing required CSV files (data tables, flux tower data, sap flow data, thermal data).
- `/Data/source/round.POSIXct.R` — helper function script for time rounding.
- `/script/Data cleaning and analysis.R` — main script performing data preprocessing, merging, seasonal and diurnal analyses, statistical modeling, and generating all figures.
This script:
- Reads the original datasets from the `Data/original data` folder.
- Aligns timestamp formats across all sources for consistent comparison.
- Filters the thermal data by:
  - Removing rainy days, which distort the thermal signal.
  - Excluding data points with wind speeds greater than 1 standard deviation above the monthly average.
- Processes the data starting at **line 181** to:
  - Compute means and variances
  - Merge the cleaned tables

---

## Requirements

- R version ≥ 4.2.0
- R packages:
  - `tidyverse`
  - `lubridate`
  - `ggplot2`
  - `patchwork`
  - `ggpubr`
  - `segmented`
  - `gridExtra`
  - `reshape2`
