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

rfu_mgm2_chla <- function(chla_df, sa_df, slp, blank_df){
  
  ## load dependencies
  # chla_df <- chla_data
  # sa_df <- surfaceareas
  # slp <- slope
  # blank_df <- blanks_df
  
    int_function <- function(sc, vol, rfu, run) {
      # row <- 1
      # dat_row <- chla_df[row,]
      # 
      # sc <- dat_row$subs_code
      # vol <- dat_row$vol_Etoh
      # rfu <- dat_row$value_rfu
      # run <- dat_row$run
      
      sa <- sa_df$surface_area_m2[which(sa_df$subs_code == sc)] #calculates surface area
      rfu_cor1 <- rfu - blank_df$blanks[which(blank_df$run == run)] 
      rfu_cor <- ifelse(rfu_cor1 < 0, 0, rfu_cor1)
      
      mg_m2 <- rfu_cor*slp*(1/1000)*(vol/1000)*(1/sa) #1000's for conversion factors 
      return(mg_m2)
      
    }
    
    
  chla_df$value_mgm2 <-  mapply(int_function, chla_df$subs_code, chla_df$vol_Etoh,chla_df$value_rfu, chla_df$run)

return(chla_df)
}
  