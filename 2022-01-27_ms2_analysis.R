## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, include = FALSE, warning=FALSE, message=FALSE, error = FALSE)

library(Gmisc)
library(broman)
library(dplyr)
library(MASS)
library(effects)
library(emmeans)
library(DescTools)
library(tidyverse)
library(sjPlot)
library(here)
library(janitor)
library(zoo)
library(lubridate)
library(logbin)
library(lme4)
library(readxl)
library(ggeffects)
library(kableExtra)
library(logistf)
library(corrplot)
library(ghibli)
library(ggpubr)

#written by Arianne Albert (arianne.albert@cw.bc.ca)
#written in
# R version 4.1.1 "Kick Things"
# Platform: x86_64-apple-darwin17.0 (64 bit)
# working in RStudio





## ----load data----------------------------------------------------------------------------------
## BC, MB, QC combined
bmq <- read.csv(here("BC_MB_QC_CLEANED.csv"))

## ON
on.dat <- read.csv(here("Provincial summary data/ON reports/ONTARIO line-level/ON_DATA_CLEANED.csv"))




## ----restrict dates for BC MB QC----------------------------------------------------------------




