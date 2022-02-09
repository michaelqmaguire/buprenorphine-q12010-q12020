#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: BUPRENORPHINE Q1 2010 - Q1 2020                                                                        #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JOSHUA BROWN, PHARMD, PHD,                                                 #
# SCRIPT: 04_import-subset-aggregate-georgia.R                                                           			    #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(data.table)
library(hrbrthemes)
library(tidyverse)
library(tidylog)

## Read in base file.

skStates <- setNames(
  as.data.table(
    c('Alabama', 'Florida', 'Idaho', 'Indiana', 'Kansas', 'Louisiana', 'Mississippi', 'Montana',
      'Nebraska', 'Nevada', 'North Carolina', 'North Dakota', 'South Dakota', 'Tennessee', 'Wyoming'
      )
    ),
  "state")

statesAbb <- setNames(as.data.table(cbind(state.name, state.abb)), c("state", "abbreviation"))

skFinal <-
  statesAbb[skStates, on = .(state = state)]

statesOfInterest <- skFinal$abbreviation

## Read in SDUD file 

sdud <- fread(
  file_location,
  colClasses = c("proper_ndc" = "character")
)

yrSt <-
  sdud[
    i = year %in% c(2013:2020) & state %in% statesOfInterest & str_detect(gennme, pattern = "Buprenorphine"),
    j = .(utilization.type, strngth, mstfmds, year, proper_ndc, quarter, state, suppression, numberrx, prodnme, gennme)
  ]

unique(yrSt$prodnme, by = 'prodnme')

setorder(yrSt, state, year, quarter, proper_ndc)

yrStRange <- yrSt[
    i = ,
    j = `:=` (inRange = ifelse((year == 2013 & quarter < 4) | (year == 2020 & quarter > 1), 0, 1), yearqtr = paste0(year, '-', quarter))
  ]

yrStRange |>
  janitor::tabyl(inRange)

yrStRange |>
  filter(inRange == 1) |>
  distinct(yearqtr)

yrStRange |>
  filter(inRange == 0) |>
  distinct(yearqtr)

drugsAggStateProdnmeRange <- yrStRange[
    i = inRange == 1,
    j  = .(totalRX = sum(numberrx)),
    by = c("utilization.type", "year", "state", "quarter", "gennme", "prodnme", "strngth", "mstfmds", "suppression")
  ]

## Plot total prescriptions by year, quarter, and generic name.

drugsAggStateProdnmeRange |> filter(suppression == 'F') |>
  ggplot() +
  geom_col(
    aes(
      x = as.factor(paste0(year, '-', quarter)),
      y = totalRX,
      fill = gennme
    )
  ) +
  theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(direction = -1)

## Plot total prescriptions by year, quarter, and brand name.

drugsAggStateProdnmeRange |> filter(suppression == 'F') |>
  ggplot() +
  geom_col(
    aes(
      x = as.factor(paste0(year, '-', quarter)),
      y = totalRX,
      fill = prodnme
    )
  ) +
  theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(angle = 90)
  ) + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(direction = -1)

## Plot total prescriptions by year, quarter, generic name, and state.

drugsAggStateProdnmeRange |> filter(suppression == 'F') |>
  ggplot() +
  geom_col(
    aes(
      x = as.factor(paste0(year, '-', quarter)),
      y = totalRX,
      fill = gennme
    )
  ) + 
  facet_wrap(~state) +
  theme_ipsum_rc() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(direction = -1)

## Plot total prescriptions by year, quarter, brand name, and state.

drugsAggStateProdnmeRange |> filter(suppression == 'F') |>
  ggplot() +
  geom_col(
    aes(
      x = as.factor(paste0(year, '-', quarter)),
      y = totalRX,
      fill = prodnme
    )
  ) + 
  facet_wrap(~state) +
  theme_ipsum_rc() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_viridis_d(direction = -1)

fwrite(drugsAggStateProdnmeRange, file = "./data/clean/buprenorphine-q42013-q12020_rr.csv")
