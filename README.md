---
output: html_document
---

## Introduction

Software for video densitometry analysis. The software takes one chromatogram image, dimension parameters from the sample and development steps and extract the video densitograms. 
Then a step of preprocessing is made (baseline correction but more eventually like peak alignment, smoothing etc...), followed by a step of peak integration. 
Finally, a linear or quadratic model is fit which give access to LOD, LOQ, R2, RMSE and allow to predict new samples. 

## Web application

Available on our server hosted in the Justus Liebig University of Giessen, Germany:

https://shinyapps.ernaehrung.uni-giessen.de/quanTLC/

## Installation

Go to 
https://github.com/OfficeChromatography/quantTLC_Installation
and follow the instructions.

#### From cran

Incomming (may be)
