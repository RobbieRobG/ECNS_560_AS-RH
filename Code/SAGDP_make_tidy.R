library(tidyverse)
library(readxl)

SAGDP_tidy = read_csv("1. Raw Data/SAGDP_Filtered/SAGDP1__ALL_AREAS_1997_2023.csv")|>
  select(-GeoFIPS,-TableName,-LineCode,-IndustryClassification,-Unit,-Region)|>
  pivot_longer(
    cols = starts_with(c("20","19")),
    names_to = "Year",
    values_to = "Estimate"
  )|>
  filter(!is.na(GeoName))|>
  pivot_wider(
    names_from = Description,
    values_from = Estimate
  )
saveRDS(SAGDP_tidy, '2. Merged Data/SAGDP_tidy.rds')
write.csv(SAGDP_tidy, "2. Merged Data/SAGDP_tidy.csv", row.names = F)