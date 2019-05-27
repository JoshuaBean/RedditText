## Libs ----
pacman::p_load(tidyverse)

## Read in data ----
CricketData  <- read_csv("inst/extdata/ODIDataset.csv")

## Removing the first column
CricketData <- CricketData[-1]

## Save data ----
usethis::use_data(CricketData, overwrite = TRUE)
