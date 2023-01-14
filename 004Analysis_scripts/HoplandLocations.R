  ## ---------------------------
  ##
  ## Script name: Hopland GIS Locations
  ##
  ## Purpose of script:
  ##
  ## Author: Hannah Fertel
  ##
  ## Date Created: 2022-03-31
  ##
  ## Copyright (c) Hannah Fertel, 2022
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
  
  
  library(tidyverse)
  
  #read in transect, sampling, and post GPS data to combine into one master datasheet 
  
transects<-read.csv("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/Non-Con_Severity/Data/TransectInfo.csv")
Posts<-read.csv("C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/Non-Con_Severity/Data/HoplandGPS.csv")


transects1<-transects %>% 
  mutate(PostID=TransectID) %>% 
  drop_na(TransectID)

transects2<-left_join(transects1,Posts, by="PostID")

transects3<-transects2 %>% 
  mutate(Sampled2018=ifelse(is.na(min_0_1 == TRUE),0,1)) %>% 
  dplyr::select(PlotID,TransectID.x,PostB,Az,Plot,Sampled2018,NewE,NewN,latitude,longitude,slope,elevation,aspect,comments)

#write.csv(file="C:/Users/hmfertel.CAMPUS/Documents/Stephens Lab/Non-Con_Severity/Data/LocationsHopland_Corrected.csv",x = transects3)  
  
  