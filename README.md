# About quickpcr

quickpcr is an R package that automates preprocessing, outlier detection, statistical analysis, and visualization of qPCR raw data. Originally developed for outputs from Applied Biosystems qPCR instruments, it works directly with simple .xls matrices and can be applied to any qPCR dataset that follows this format.

## Installation

```r
# CRAN version
install.packages("quickpcr")

# Development version (GitHub)
remotes::install_github("wez-97/quickpcr")
```
# Quick Start

## STEP 1: Export rawdata from Quantstudio™ Design & Anaylsis Software 
If you do not use Applied Systems qpcr instrument, skip this step and go to step 1-1

![alt text](image.png)

## STEP 1-1: Prepare matrix for quickpcr anaylsis 
Generate the rawdata(metadata) matrix and name that sheet as "rawdata"
- `Warning`: Column names must match exactly as shown, including capitalization and spaces.
- `Warning`: The sample name must follow the format "{Cell Name}_{Treatment}_{Day}". quickpcr will automatically generate four new columns — Cell Name, Treatment, Day, and experiment — based on the sample name.



