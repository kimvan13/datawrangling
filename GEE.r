library(tidyverse)
library(readxl)
library(visdat)
GEE <- read_excel('Government_Expenditure_Education.xls', col_names = TRUE, skip = 3)
GEE <- GEE[,-(3:48)]
GEE <- GEE[, -(17)]
GEE %>% vis_miss()
GEE[is.na(GEE)] <- 'no data'
head(GEE)
str(GEE)
names(GEE)[1] <- "Country"
New_GEE <- GEE %>%
  filter(Country == "Mexico")
M_GEE <- New_GEE %>%
  gather(key=Year, value=PublicExpenditure)
M_GEE <- M_GEE[-(1:2),]
names(M_GEE)[2] <- "Government_Expenditure"

