# Libraries:
library(tidyverse)
library(readxl)
library(purrr)

# Loading in CSVs----------

#st_fin_2010 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2010.xlsx")
st_fin_2011 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2011.xlsx",
                         col_names = F)
st_fin_2012 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2012.xlsx",
                         col_names = F)
st_fin_2013 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2013.xlsx",
                         col_names = F)
st_fin_2014 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2014.xlsx",
                         col_names = F)
st_fin_2015 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2015.xlsx",
                         col_names = F)
st_fin_2016 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2016.xlsx",
                         col_names = F)
st_fin_2017 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2017.xlsx",
                         col_names = F)
st_fin_2018 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2018.xlsx",
                         col_names = F)
st_fin_2019 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2019.xlsx",
                         col_names = F)
st_fin_2020 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2020.xlsx",
                         col_names = F)
st_fin_2021 = read_excel("1. Raw Data/Excel_cleaned_state_finances/StateFinancesByYear2021.xlsx",
                         col_names = F)
st_fin_2010 = read_excel("1. Raw Data/Excel_cleaned_state_finances/2010.xlsx",
                         col_names = F)
# FUNCTION: renaming columns in a dataframe ----------

rename_columns = function(data) {
  # Extract the first row as new column names
  new_column_names = as.character(data[1, ])
  # Assign the new names to the data frame
  colnames(data) = new_column_names
  # Remove the first row
  data = data[-1, ]
}

# Parsing data, list of names to include------------
# excel generated list of values useful for analysis
list_to_keep = c("State","Year","From State government1",
                 "Taxes",
                 "Property",
                 "Sales and gross receipts",
                 "General sales",
                 "Selective sales",
                 "Motor fuel",
                 "Alcoholic beverage",
                 "Tobacco products",
                 "Public utilities",
                 "Other selective sales",
                 "Individual income",
                 "Corporate income",
                 "Motor vehicle license",
                 "Other taxes",
                 "Charges and miscellaneous general revenue",
                 "Current charges",
                 "Education",
                 "Institutions of higher education",
                 "School lunch sales (gross)",
                 "Hospitals",
                 "Highways",
                 "Air transportation (airports)",
                 "Parking facilities",
                 "Sea and inland port facilities",
                 "Natural resources",
                 "Parks and recreation",
                 "Housing and community development",
                 "Sewerage",
                 "Solid waste management",
                 "Other charges",
                 "Education",
                 "Higher education",
                 "Capital outlay",
                 "Elementary & secondary",
                 "Other education",
                 "Libraries",
                 "Public welfare",
                 "Cash assistance payments",
                 "Vendor payments",
                 "Other public welfare",
                 "Hospitals",
                 "Health",
                 "Employment security administration",
                 "Veterans' services",
                 "Police protection",
                 "Debt outstanding"
)
list_to_keep = unique(list_to_keep)
# Testing for tidy function----------
st_fin_2012_tidy = st_fin_2012|>
  drop_na()|>
  t()|>
  rename_columns()|>
  as.data.frame()
st_fin_2012_tidy = st_fin_2012_tidy|>
  select(intersect(names(st_fin_2012_tidy), list_to_keep))
  
  # pivot_longer(
  #   cols = starts_with(c("20","19")),
  #   names_to = "Year",
  #   values_to = "Estimate"
  # )
  # pivot_wider(
  #   names_from = "1",
  #   values_from =
  # )

# Tidy Function ----------
# Function to tidy a list of data frames----------
make_tidy = function(st_fin_year) {
  tidy_st_fin = st_fin_year|>
    drop_na()|>
    # Transpose the data frame to tidy it
    t() |>
    as.data.frame()|>
    rename_columns()
  tidy_st_fin = tidy_st_fin|>
    select(intersect(names(tidy_st_fin), list_to_keep))
  return(tidy_st_fin)
}

# Make Tidy and Merge ------
  #I know i should be able to use an apply function here but i couldn't figure it out at this time
# st_fin_2011_tidy = make_tidy(st_fin_2011)
# st_fin_2012_tidy = make_tidy(st_fin_2012)
# st_fin_2013_tidy = make_tidy(st_fin_2013)
# st_fin_2014_tidy = make_tidy(st_fin_2014)
# st_fin_2015_tidy = make_tidy(st_fin_2015)
# st_fin_2016_tidy = make_tidy(st_fin_2016)
# st_fin_2017_tidy = make_tidy(st_fin_2017)
# st_fin_2018_tidy = make_tidy(st_fin_2018)
# st_fin_2019_tidy = make_tidy(st_fin_2019)
# st_fin_2020_tidy = make_tidy(st_fin_2020)
# st_fin_2021_tidy = make_tidy(st_fin_2021)

st_fin_2010 <- st_fin_2010|>
  mutate_all(as.character)

all_st_fin_tidy = make_tidy(st_fin_2011)|>
  bind_rows(make_tidy(st_fin_2012))|>
  bind_rows(make_tidy(st_fin_2013))|>
  bind_rows(make_tidy(st_fin_2014))|>
  bind_rows(make_tidy(st_fin_2015))|>
  bind_rows(make_tidy(st_fin_2016))|>
  bind_rows(make_tidy(st_fin_2017))|>
  bind_rows(make_tidy(st_fin_2018))|>
  bind_rows(make_tidy(st_fin_2019))|>
  bind_rows(make_tidy(st_fin_2020))|>
  bind_rows(make_tidy(st_fin_2021))|>
  bind_rows(make_tidy(st_fin_2010))
  
# all_st_fin_tidy <- all_st_fin_tidy|>
#   mutate_all(as.character)
# 
# common_cols <- intersect(names(all_st_fin_tidy),
#                          (names(st_fin_2010)))
# 
# all_st_fin_tidy = bind_rows(
#     select(all_st_fin_tidy,all_of(common_cols)),
#     select(st_fin_2010,all_of(common_cols))
#   )
# 
# st_fin_2010_tidy=st_fin_2010|>
#   select(intersect(names(st_fin_2010), list_to_keep))


