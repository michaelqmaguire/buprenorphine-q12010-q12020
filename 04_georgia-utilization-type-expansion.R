#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: BUPRENORPHINE Q1 2010 - Q1 2020                                                                        #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 04_import-subset-aggregate-georgia.R                                                           			    #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(data.table)
library(tidyverse)
library(tidylog)

## Read in base file.

base <-
  fread(
    file_location,
    colClasses = c("proper_ndc" = "character")
  )

## Extract years of interest, drugs of interest, and states of interest.

yrSt <-
  base[
    i = year %in% c(2010:2020) & state == "GA" & str_detect(gennme, pattern = "Buprenorphine"),
    j = .(utilization.type, strngth, mstfmds, year, proper_ndc, quarter, state, suppression, numberrx, prodnme, gennme)
  ]

## Aggregate by year, state, quarter, generic name, brand name, strength, form, and whether values were suppressed.

drugsAggStateProdnmeRange <-
  yrSt[
    i = !c(year == 2020 & quarter %in% c(2, 3, 4)),
    j  = .(totalRX = sum(numberrx)),
    by = c("utilization.type", "year", "state", "quarter", "gennme", "prodnme", "strngth", "mstfmds", "suppression")
  ]

# Generic name plot

ggplot(data = drugsAggStateProdnmeRange) +
  geom_col(
    aes(
      x = as.factor(paste0(year, "-", quarter)),
      y = totalRX,
      fill = gennme
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  facet_wrap(
    ~ utilization.type
  )

# Product name plot

ggplot(data = drugsAggStateProdnmeRange) +
  geom_col(
    aes(
      x = as.factor(paste0(year, "-", quarter)),
      y = totalRX,
      fill = prodnme
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  facet_wrap(
    ~ utilization.type
  )

# Output file

fwrite(drugsAggStateProdnmeRange, "./data/clean/ga-with-utilization-buprenorphine-drugs-q1-2010-q1-2020-year-quarter-state-drug.csv")

