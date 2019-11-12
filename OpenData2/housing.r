
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

data <- fread('2018_lloguer_preu_trim.csv')
cols <- colnames(data)

