## ---------------------------
##
## Script name: Shrub Severity dNBR script 
##
## Purpose of script:
##
## Author: Hannah Fertel
##
## Date Created: 2021-10-26
##
## Copyright (c) Hannah Fertel, 2021
## Email: hmfertel@berkeley.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(data.table)
# source("functions/packages.R")       # loads up all the packages we need

## ---------------------------

## load up our functions into memory

# source("functions/summarise_data.R") 

## ---------------------------

libraries<-libraries <- c("rgdal", "raster", "rgeos", "RColorBrewer", "dplyr", "sf", "magrittr", "RCurl")
lapply(libraries,library,character.only=TRUE)
library(devtools)
#install_github("bleutner/RStoolbox")
library(RStoolbox)


#load rasters pre and post fire event 
bands_pre<-list.files("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/Shrub_Severity",
                      pattern=glob2rx("*band*.tif$"),
                      full.names=TRUE)
band5<-raster("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/Shrub_Severity/LC08_CU_002008_20200505_20210504_02_SR_B5.TIF")
band7<-raster("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/Shrub_Severity/LC08_CU_002008_20200505_20210504_02_SR_B7.TIF")
plot(band5)

stack<-stack(band5,band7)
brick<-brick(stack)

#create a raster that calculates the difference per the equation for dNBR? 
