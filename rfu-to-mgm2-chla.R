####################################################################
## Audrey Thellman
## Calculate chla (mg_m2) for all samples (just the function)
## Created: 08-May-2020
## Updated: 08-May-2020
####################################################################

## load packages 
## ~~~~~~~~~~~~~

library(readxl)
library(tidyverse)
library(lubridate)

## function to calculate chla (mg m2)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# this depends on the slope of the standard curve, the surface area of the subtrate, and the volume of EtOH used in analysis 

#input: rfu, run, subscode, run_error, vol_true (either NA or #s from a column that contains real volumes ), 

rfu_mgm2_chla <- function(rfu, run, sc, run_error, vol_true){
  
  ## load dependencies 
  savedir <- "C:/Users/Thell/Documents/Duke University/Research/HBEF/FieldExperiments/Summer2019/HB_chla_summer2019/data/tidy/"
  blanks <- readRDS(paste0(savedir,"blanks_v1.rds"))
  masterslope <- readRDS(paste0(savedir, "masterslope.rds"))
  volcalc <- readRDS(paste0(savedir, "volcalc.rds"))
  surfaceareas <- readRDS(paste0(savedir, "surfaceareas.rds"))
  
  
  
      sa <- surfaceareas$surface_area_m2[which(surfaceareas$subs_code == sc)] 
      #calculates the surface area
      
      run_vol <- ifelse(run_error == "none", run, run_error) 
      vol_tile <- volcalc$vol_tile[which(volcalc$run == run_vol)] 
      #calcuates these based on ru, note that fake moss, light gap and 2018 have special code
      vol_other <- volcalc$vol_scrub[which(volcalc$run == run_vol)]
      vol_eth <- as.numeric(ifelse(sc == T, vol_tile, vol_other)) 
      
      rfu_cor1 <- rfu - blanks$blanks[which(blanks$run == run)] 
      rfu_cor <- ifelse(rfu_cor1 < 0, 0, rfu_cor1)
      #this is the blank code that calculates what to subtract by 
      
      vol <- as.numeric(ifelse(is.na(vol_true), vol_eth, vol_true))#error catch for real vols

      mg_m2 <- rfu_cor*masterslope*(1/1000)*(vol/1000)*(1/sa) #1000's for conversion factors 
      
      return(mg_m2)
}


