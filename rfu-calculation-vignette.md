rfu-calculation-vignette
================
Audrey Thellman
8/31/2020

## Introduction

Converting between chlorophyll-a (chl-a) raw units (rfu) and mg/m*^2*
for the Hubbard Brook Experimental Forest long-term algal record

To do this calculation, you need three five things:

  - The sample list file (sample ID, sampling date, substrate, and
    watershed, or weir)
  - The raw chl-a units (rfu) file with: a) rfu, b) volume of etoh, c)
    sample ID, and d) chla run number
  - The standard curve slope
  - The blanks measured for each run
  - the surface areas of the substrates

In general, the calculation will first, merge the raw data with the
sampling listing. Second, subtract average blank values from each run.
Third, calculate chl-a in mg/m2 using the slope of the standard curve,
the volume of EtOH, and the surface area of the substrates.

The first thing we need to do is load all required packages and set our
project directory.

``` r
projdir <- getwd() #set project directory 

# add packages 

library(tidyverse)
library(lubridate)
library(readxl)
```

## Step 1: clean input data

First, load the data into your workspace, if you are using 2 separate
csv files for the data, use this code chunk:

``` r
rfu_data <- read.csv(paste0(projdir, "/raw data/hbef_chla_rfu.csv"))
samplinglist <- read.csv(paste0(projdir, "/raw data/samplinglist.csv"), skip = 1)
```

If you are using an excel file, un-comment and use this code chunk:

``` r
# rfu_data <- read_excel(paste0(projdir, "/raw data/hbef_2019samples_chla.xlsx"), sheet = 1)
# samplinglist <- read_excel(paste0(projdir, "/raw data/hbef_2019samples_chla.xlsx"), sheet = 2)
```

To check column compatibility (format check), your column names must
match:

``` r
#list of column names 
colnames(rfu_data_example)
```

    ## [1] "Sample.ID"  "short_id"   "Flr_sample" "run"        "vol_Etoh"  
    ## [6] "value_rfu"

``` r
colnames(samplinglist_example)
```

    ## [1] "Sample.ID" "WEIR.REP"  "Date"

*If the columns don’t match, manually change them and re-add your files*

## Step 2: check for errors and create factors

In the next steps, we will:

1)  check that your sample ID’s match between `rfu_data` and
    `samplinglist`
2)  create factors for weir and substrate
3)  assign a substrate code (see `substrate_surfaceareas.xlsx`)

<!-- end list -->

``` r
error <- rfu_data[rfu_data$Sample.ID %in% samplinglist$Sample.ID == F,]
print(error)
```

    ## [1] Sample.ID  short_id   Flr_sample run        vol_Etoh   value_rfu 
    ## <0 rows> (or 0-length row.names)

``` r
#if there are data that are missing 

rfu_data <- rfu_data[!rfu_data$Sample.ID %in% error$Sample.ID,]
# if the data's sample ID's do not match, they will show up here on the rfu_data file, check for typos 

chla_data <- merge(rfu_data, samplinglist, by = "Sample.ID", all.x = T) #merge two dataframes by SampleID, keeping all of those values 

#create factors for weir and substrate 

chla_data$weir <- as.factor(substr(chla_data$WEIR.REP, 1,2))
chla_data$substrate <- as.factor(substr(chla_data$WEIR.REP,4,4))

#assign substrate code (NOTE ONLY FOR 2019+ SAMPLES)
chla_data$subs_code <- as.factor(ifelse(chla_data$substrate == "M", "M_b", "T"))

#only run for 2018 samples 
#chla_data$subs_code <- ifelse(chla_data$substrate == "M", "M_s", chla_data$substrate)

#change to date format 
chla_data$Date <- as.Date(chla_data$Date)
```

Now your data should have the required columns of a) rfu value, b)
substrate, c) date, d) weir, and e) sampling ID which will give you
substrate, date, and weir

## Step 3: convert from rfu to mg/m^2

To covert from rfu to mg/m2 we use the following equation:

![equation 1](raw%20data/equation_image.PNG)

where `RFU` is the raw units (corrected by substracting the average of
the blanks), `Slope` is the standard slope (`rfu/(ug/L)`), `V` is the
volume of ethanol (mL) and `SA` is the surface area (`m^2`).

To do this calculation, we will source the function `rfu-to-mgm2-chla.R`

Please pay attention to formatting\! The formatting of the sheet must be
identical to this example for this function to work. Also, check that
`vol_Etoh` and `value_rfu` are “numeric” (e.g. `int` or `num` or `dbl`)

``` r
surfaceareas <- read_excel(path = paste0(projdir, "/raw data/substrate_surfaceareas.xlsx"), sheet = 1)
blanks_df <- read.csv(paste0(projdir, "/raw data/slope_and_blanks.csv"))[1:2]
slope <- as.numeric(read.csv(paste0(projdir, "/raw data/slope_and_blanks.csv"))[1,3])

#check the data
#str(chla_data)

source("rfu-to-mgm2-chla.R")

chla_data2 <- rfu_mgm2_chla(chla_df = chla_data, sa_df = surfaceareas, slp = slope, blank_df = blanks_df)

head(chla_data2) #view result data 
```

    ##   Sample.ID short_id Flr_sample  run vol_Etoh value_rfu WEIR.REP       Date
    ## 1  CH190001        1 SAMPLE-008 run1       10     81.91     W1-T 2019-04-29
    ## 2  CH190002        2 SAMPLE-009 run1       20    284.07     W1-M 2019-04-29
    ## 3  CH190003        3 SAMPLE-010 run1       10     18.07     W2-T 2019-04-29
    ## 4  CH190004        4  AMPLE-011 run1       20     88.47     W2-M 2019-04-29
    ## 5  CH190005        5 SAMPLE-012 run1       10    105.27     W3-T 2019-04-29
    ## 6  CH190006        6 SAMPLE-013 run1       20    279.11     W3-M 2019-04-29
    ##   weir substrate subs_code value_mgm2
    ## 1   W1         T         T 0.16078204
    ## 2   W1         M       M_b 0.43118900
    ## 3   W2         T         T 0.02358289
    ## 4   W2         M       M_b 0.12668110
    ## 5   W3         T         T 0.21098524
    ## 6   W3         M       M_b 0.42346733

Now, we can save a “tidy” output version of this data, keeping only the
parts that we need:

## Step 4: save the data output

For all output data, we will be using the filename
`hbwater_YEAR_chla_output.csv`

``` r
chla_data3 <- data.frame(chla_data2[c("Sample.ID","run","vol_Etoh","value_rfu","Date","weir","substrate","value_mgm2")])

#write.csv(chla_data3, row.names = F, file = "hbwater_2019_chla_output.csv")
```
