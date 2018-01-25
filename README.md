---
output: html_document
---

## Introduction

Software for video densitometry analysis. The software takes one chromatogram image, dimension parameters from the sample and development steps and extract the video densitograms. 
Then a step of preprocessing is made (baseline correction but more eventually like peak alignment, smoothing etc...), followed by a step of peak detection and integration. 
Finally, a linear or quadratic model is fit which give access to LOD, LOQ, R2, RMSE and allow to predict new samples. 

## Web application

Available on our server hosted in the Justus Liebig University of Giessen, Germany:

http://134.176.7.66/quanTLC/

## Installation

Install R
https://www.r-project.org/

#### From cran

Incomming (may be)

#### From github

In the console, install the devtools package with those commands
```r
install.packages("devtools")
library(devtools)
install_github("dimitrif/quanTLC")
devtools::install_github("jrowen/rhandsontable") ## this package is available on CRAN but there is an issue, install the github version instead
```

Then, run this command to launch the application
```r
quanTLC::run.quanTLC()
```
