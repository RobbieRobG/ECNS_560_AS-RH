library(readxl)
library(tidyverse)


# YEARS: 1980-1989----------

# This data may not be usable only 1980, no yearly estimates

popdata_80_89 = read_excel('1980-89/pe-02.xls', skip = 5)

popdata_80_89 = popdata_80_89|>
  filter(if_all(everything(), ~ !is.na(.)))|>
  group_by(`FIPS State and County Codes`,`Year of Estimate`)|>
  summarize(across(matches('years'),\(x) sum(x, na.rm = TRUE)))

popdata_80_90 = read.table("1980-89/comp8090.txt", header = TRUE, sep = "")

readLines("1980-89/comp8090.txt", n = 5)

# YEARS: 1990-1999----------

# Shoot! data does not contain county info
 
# Define the column widths based on the census data structure
col_widths <- c(2, 2, 4, 3, 1, 10, 10, 10, 10, 10, 10, 10, 10, 
                10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)

# Define the column names based on your description
col_names <- c("Series", "Month", "Year", "Age", "blank", 
               "Total_Population", "Total_Male", "Total_Female", 
               "White_Male", "White_Female", "Black_Male", 
               "Black_Female", "AI_Male", "AI_Female", 
               "API_Male", "API_Female", "Hispanic_Male", 
               "Hispanic_Female", "WNH_Male", "WNH_Female", 
               "BNH_Male", "BNH_Female", "AI_NH_Male", 
               "AI_NH_Female", "API_NH_Male", "API_NH_Female")

popdata_90_90 <- read.fwf("1990-99/e9090cmp.txt", 
                          widths = col_widths, 
                          col.names = col_names, 
                          stringsAsFactors = FALSE)

# YEARS: 2000-2010----------

popdata_00_10 = read.csv("2000-10/co-est00int-agesex-5yr.csv", header = TRUE)

popdata_00_10 = popdata_00_10 |> 
  #filtering sex == 0 because that is total for both male and female
  filter(SEX==0)|>
  group_by(STATE, COUNTY, STNAME, CTYNAME, AGEGRP)|>
  summarize(across(matches('ESTIMATE|POP'),\(x) sum(x, na.rm = TRUE)))|>
  # renaming estimates to be year names
  rename_with(~ gsub("POPESTIMATE", "", .x), starts_with("POPESTIMATE"))|>
  #replacing 2000 and 2010 estimates with actual census data:
  select(-c('2000','2010'))|>
  rename('2000' = ESTIMATESBASE2000)|>
  rename('2010' = CENSUS2010POP)

#making this data tidy:
popdata_00_10_tidy = popdata_00_10|>
  pivot_longer(
    cols = starts_with("20"),
    names_to = "Year",
    values_to = "Estimate"
  )|>
  pivot_wider(
    names_from = AGEGRP,
    values_from = Estimate
  )
#Saving data
saveRDS(popdata_00_10_tidy, '2000-10/popdata_00_10_tidy.rds')
write.csv(popdata_00_10_tidy, "2000-10/popdata_00_10_tidy.csv", row.names = F)

# YEARS: 2011-2020----------

popdata_11_20 = read.csv("2011-20/CC-EST2020-AGESEX-ALL.csv", header = TRUE)

# filtering out years we have data for or those that have
# APR and AUG estimates (we only need 1, so we will keep them consistant 
# to be the AUG estimates)
popdata_11_20 = popdata_11_20|>
  filter(!YEAR %in% c(1,2,3,13))|>
  mutate(YEAR = recode(YEAR,
                       `4` = 2011,
                       `5` = 2012,
                       `6` = 2013,
                       `7` = 2014,
                       `8` = 2015,
                       `9` = 2016,
                       `10` = 2017,
                       `11` = 2018,
                       `12` = 2019,
                       `14` = 2020))

# removing extraneous columns
popdata_11_20 = popdata_11_20|>
  select(-contains("MALE"), 
         -contains("FEM"), 
         -contains("MEDIAN"),
         -SUMLEV)

#Saving data
saveRDS(popdata_11_20, '2011-20/popdata_11_20_tidy.rds')
write.csv(popdata_11_20, "2011-20/popdata_11_20_tidy.csv", row.names = F)

# YEARS: 2021-2023----------

popdata_20_23 = read.csv("2020-23/cc-est2023-agesex-all.csv", header = TRUE)

#filter out data we already have
popdata_20_23 = popdata_20_23|>
  filter(!YEAR %in% c(1))|>
  mutate(YEAR = recode(YEAR,
                       `2` = 2020,
                       `3` = 2021,
                       `4` = 2022,
                       `5` = 2023))

# removing extraneous columns
popdata_20_23 = popdata_20_23|>
  select(-contains("MALE"), 
         -contains("FEM"), 
         -contains("MEDIAN"),
         -SUMLEV)

#Saving data
saveRDS(popdata_20_23, '2020-23/popdata_20_23_tidy.rds')
write.csv(popdata_20_23, "2020-23/popdata_20_23_tidy.csv", row.names = F)

# YEARS: 2000-2023----------

  # Combining the most similar data sets from 2011-2023
popdata_11_23 = popdata_11_20|>
  # remove 2020 values to avoid doubling up
  filter(YEAR != 2020) |>
  # make everything integers for joining
  mutate(POPESTIMATE = as.integer(POPESTIMATE))|>
  mutate(across(contains("TOT"), as.integer))|>
  
  full_join(popdata_20_23)|>
  arrange(STATE, COUNTY, STNAME, CTYNAME)


# Combining in the 2000-10 data

# making sure year is integer for both datasets:
# Convert YEAR in both dataframes to integer
popdata_11_23 <- popdata_11_23 |> 
  mutate(YEAR = as.integer(YEAR))
popdata_00_10_tidy <- popdata_00_10_tidy |> 
  mutate(Year = as.integer(Year))

popdata_00_23 = popdata_11_23|>
  full_join(popdata_00_10_tidy,
            by = c(
              "STATE" = "STATE",
              "COUNTY" = "COUNTY",
              "STNAME" = "STNAME",
              "CTYNAME" = "CTYNAME",
              "YEAR"="Year",
              "POPESTIMATE" = "0",
              "AGE04_TOT" = "1",
              "AGE59_TOT" = "2",
              "AGE1014_TOT" = "3",
              "AGE1519_TOT" = "4",
              "AGE2024_TOT" = "5",
              "AGE2529_TOT" = "6",
              "AGE3034_TOT" = "7",
              "AGE3539_TOT" = "8",
              "AGE4044_TOT" = "9",
              "AGE4549_TOT" = "10",
              "AGE5054_TOT" = "11",
              "AGE5559_TOT" = "12",
              "AGE6064_TOT" = "13",
              "AGE6569_TOT" = "14",
              "AGE7074_TOT" = "15",
              "AGE7579_TOT" = "16",
              "AGE8084_TOT" = "17",
              "AGE85PLUS_TOT" = "18"
            )
           )|>
  arrange(STATE, COUNTY, STNAME, CTYNAME)

# Final Pop_data Clean----------


popdata_00_23_clean = popdata_00_23|>
  # create a FIPS code to coordinate with other data, which makes the SATE and COUNTY variables redundant
  mutate(FIPS = sprintf("%02d%03d", STATE, COUNTY))|>
  select(FIPS, everything(), -STATE, -COUNTY)|>
  # removing age groupings that are not consistant across all years
  select(-UNDER5_TOT,-AGE513_TOT, -AGE1417_TOT, -AGE1824_TOT,
         -AGE16PLUS_TOT, -AGE18PLUS_TOT, -AGE1544_TOT, -AGE2544_TOT,
         -AGE4564_TOT, -AGE65PLUS_TOT)#|>
  #adding in proportions of total pop for each remaining age group:
  
popdata_00_23_clean = popdata_00_23_clean|>
  mutate(across(starts_with("AGE"), 
                ~ ./POPESTIMATE,
                # Rename new variables by replacing "TOT" with "PROP"
                .names = "{sub('TOT', 'PROP', .col)}"))  

#Saving data
saveRDS(popdata_00_23_clean, '2. Merged Data/popdata_00_23_clean.rds')
write.csv(popdata_00_23_clean, "2. Merged Data/popdata_00_23_clean.csv", row.names = F)
