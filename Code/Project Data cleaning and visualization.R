library(tidyverse)
library(dplyr)
#-------------------------------------------------------------------------------------------------------------
# Merging demographic and GDP data
Population_data <- read.csv("C:/Users/Al/OneDrive/Documents/GitHub/ECNS_560_AS-RH/2. Merged Data/popdata_00_23_clean.csv")
view(data)

Sagdp_data <- read.csv("C:/Users/Al/OneDrive/Documents/GitHub/ECNS_560_AS-RH/2. Merged Data/SAGDP_tidy.csv")


# Merge Datasets
Merged_Population_SAGDP_Data <- Population_data %>%
  full_join(Sagdp_data, by = c("STNAME" = "GeoName", "YEAR" = "Year"))

write.csv(result, "Merged_Population_SAGDP_Data.csv", row.names = FALSE)


#--------------------------------------------------------------------------------------------------------------
# Turning quarterly debt to income ratio data into annual data
debt_to_income_ratio_data <- read.csv("C:/Users/Al/OneDrive/Documents/GitHub/ECNS_560_AS-RH/1. Raw Data/Household Debt To Income Ratio Data/DebtToIncomeByCounty.csv")


# Checking data structure and converting variables to numeric as needed
str(debt_to_income_ratio_data)
debt_to_income_ratio_data$Debt_to_income_ratio_low <- as.numeric(debt_to_income_ratio_data$Debt_to_income_ratio_low)
debt_to_income_ratio_data$Debt_to_income_ratio_high <- as.numeric(debt_to_income_ratio_data$Debt_to_income_ratio_high)
debt_to_income_ratio_data$area_fips <- as.numeric(debt_to_income_ratio_data$area_fips)
debt_to_income_ratio_data$year <- as.numeric(debt_to_income_ratio_data$year)
debt_to_income_ratio_data$qtr <- as.numeric(debt_to_income_ratio_data$qtr)

# Aggregating by County and Year to get annual data
debt_to_income_ratio_data1 <- debt_to_income_ratio_data %>%
  group_by(area_fips, year) %>%
  summarize(
    Annual_Debt_to_income_ratio_low = mean(Debt_to_income_ratio_low, na.rm = T),
    Annual_Debt_to_income_ratio_high = mean(Debt_to_income_ratio_high, na.rm = T),
    .groups = "drop"               
  )

# Saving new data

write.csv(debt_to_income_ratio_data1, "Annual_Debt_To_Income_Ratio_data.csv", row.names = F)


#------------------------------------------------------------------------------------------------------------

# More merging 
Merged_Population_SAGDP_Data <- read.csv("C:/Users/Al/OneDrive/Documents/GitHub/ECNS_560_AS-RH/2. Merged Data/Merged_Population_SAGDP_Data.csv")

annual_debt_to_income_ratio_data <- read.csv("C:/Users/Al/OneDrive/Documents/GitHub/ECNS_560_AS-RH/2. Merged Data/Annual_Debt_To_Income_Ratio_data.csv")



Merged_Population_SAGDP_DebtToIncomeRatio_Data <- Merged_Population_SAGDP_Data %>%
  full_join(annual_debt_to_income_ratio_data, by = c("FIPS" = "area_fips", "YEAR" = "year"))

write.csv(Merged_Population_SAGDP_DebtToIncomeRatio_Data, "Merged_Population_SAGDP_DebtToIncomeRatio_Data.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------------------------------

#Visualizing key variables:

#1: Histogram GIF of age catagory distribution over the years

library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

# Specify the age proportion columns and their corresponding new names
age_prop_vars <- c("AGE04_PROP", "AGE59_PROP", "AGE1014_PROP", "AGE1519_PROP", 
                   "AGE2024_PROP", "AGE2529_PROP", "AGE3034_PROP", "AGE3539_PROP", 
                   "AGE4044_PROP", "AGE4549_PROP", "AGE5054_PROP", "AGE5559_PROP", 
                   "AGE6064_PROP", "AGE6569_PROP", "AGE7074_PROP", "AGE7579_PROP", 
                   "AGE8084_PROP", "AGE85PLUS_PROP")

# Create a named vector for the new labels
age_labels <- c("Under_4", "5_to_9", "10_to_14", "15_to_19", 
                "20_to_24", "25_to_29", "30_to_34", "35_to_39", 
                "40_to_44", "45_to_49", "50_to_54", "55_to_59", 
                "60_to_64", "65_to_69", "70_to_74", "75_to_79", 
                "80_to_84", "85_plus")

# Reshape the data into long format for ggplot
long_data <- Merged_Population_SAGDP_DebtToIncomeRatio_Data %>%
  select(YEAR, all_of(age_prop_vars)) %>%
  pivot_longer(cols = all_of(age_prop_vars), names_to = "Age_Group", values_to = "Proportion")

# Map the original age groups to the new labels
long_data$Age_Group <- factor(long_data$Age_Group, 
                              levels = age_prop_vars, 
                              labels = age_labels)

# Create the ggplot object for boxplots with age groups on the x-axis
plot <- ggplot(long_data, aes(x = Age_Group, y = Proportion)) +
  geom_boxplot(aes(fill = Age_Group), outlier.shape = NA, alpha = 0.5) +  # Use boxplot to visualize proportions
  geom_jitter(aes(color = Age_Group), width = 0.2, alpha = 0.5) +  # Add jitter to show distribution of points
  labs(
    title = "Distribution of Population Proportions by Age Group From 2011 - 2023",
    y = "Proportion",
    x = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of x-axis labels
    axis.title.x = element_text(size = 16),  # Increase size of x-axis title
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18)      # Increase size of plot title
  ) +
  theme(legend.position = "none") +  # Remove legend if not needed
  transition_states(YEAR, transition_length = 2, state_length = 1, wrap = FALSE) +  # Animate by year
  ease_aes('linear')  # Define easing of the animation

# Save the animated plot
anim <- animate(plot, nframes = 26, width = 1600, height = 1200)
anim_save("age_proportion_distribution_animation.gif", animation = anim)




#2: Debt to income ratios


# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)

# Specify the debt-to-income ratio variables
debt_to_income_vars <- c("Annual_Debt_to_income_ratio_low", "Annual_Debt_to_income_ratio_high")

# Create a named vector for the new labels
debt_labels <- c("Low_Estimate", "High_Estimate")

# Reshape the data into long format for ggplot
long_debt_data <- Merged_Population_SAGDP_DebtToIncomeRatio_Data %>%
  select(YEAR, all_of(debt_to_income_vars)) %>%
  pivot_longer(cols = all_of(debt_to_income_vars), names_to = "Debt_Group", values_to = "Ratio")

# Map the original debt groups to the new labels
long_debt_data$Debt_Group <- factor(long_debt_data$Debt_Group, 
                                    levels = debt_to_income_vars, 
                                    labels = debt_labels)

# Create the ggplot object for boxplots with debt groups on the x-axis
debt_plot <- ggplot(long_debt_data, aes(x = Debt_Group, y = Ratio)) +
  geom_boxplot(aes(fill = Debt_Group), outlier.shape = NA, alpha = 0.5) +  # Use boxplot to visualize ratios
  geom_jitter(aes(color = Debt_Group), width = 0.2, alpha = 0.5) +  # Add jitter to show distribution of points
  labs(
    title = "Distribution of Debt-to-Income Ratios by Year from 2011-2023",
    y = "Debt-to-Income Ratio",
    x = "Debt Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of x-axis labels
    axis.title.x = element_text(size = 16),  # Increase size of x-axis title
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18)      # Increase size of plot title
  ) +
  theme(legend.position = "none") +  # Remove legend if not needed
  transition_states(YEAR, transition_length = 2, state_length = 1, wrap = FALSE) +  # Animate by year
  ease_aes('linear')  # Define easing of the animation

# Save the animated plot
debt_anim <- animate(debt_plot, nframes = 26, width = 800, height = 600)
anim_save("debt_to_income_distribution_animation.gif", animation = debt_anim)


#histogram

# Create a histogram for the debt-to-income ratio variable
histogram_plot <- ggplot(Merged_Population_SAGDP_DebtToIncomeRatio_Data, aes(x = Annual_Debt_to_income_ratio_low)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +  # Adjust binwidth as necessary
  labs(
    title = "Histogram of Annual Debt-to-Income Ratio (Low Estimate)",
    x = "Annual Debt-to-Income Ratio (Low Estimate)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),  # Increase size of x-axis title
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18)      # Increase size of plot title
  )

# Save the histogram as a JPEG file
ggsave(filename = "debt_to_income_histogram_low.jpeg", plot = histogram_plot, width = 10, height = 6, dpi = 300)


# Create a histogram for the debt-to-income ratio (high)
histogram_high_plot <- ggplot(Merged_Population_SAGDP_DebtToIncomeRatio_Data, aes(x = Annual_Debt_to_income_ratio_high)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black", alpha = 0.7) +  # Adjust binwidth as necessary
  labs(
    title = "Histogram of Annual Debt-to-Income Ratio (High Estimate)",
    x = "Annual Debt-to-Income Ratio (High Estimate)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),  # Increase size of x-axis title
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18)      # Increase size of plot title
  )

# Save the histogram as a JPEG file
ggsave(filename = "debt_to_income_histogram_high.jpeg", plot = histogram_high_plot, width = 10, height = 6, dpi = 300)




#Economic Variables

# Create a histogram for Real GDP
gdp_histogram_plot <- ggplot(Merged_Population_SAGDP_DebtToIncomeRatio_Data, aes(x = Real.GDP..millions.of.chained.2017.dollars..1.)) +
  geom_histogram(binwidth = 10000, fill = "green", color = "black", alpha = 0.7) +  # Adjust binwidth as necessary
  labs(
    title = "Histogram of Real GDP (Millions of Chained 2017 Dollars)",
    x = "Real GDP (Millions of Chained 2017 Dollars)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),  # Increase size of x-axis title
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18)      # Increase size of plot title
  )

# Save the histogram as a JPEG file
ggsave(filename = "real_gdp_histogram.jpeg", plot = gdp_histogram_plot, width = 10, height = 6, dpi = 300)





# HEATMAP OF RETIRMENT AGE POPULATION BY STATE AND YEAR

state_data = read_csv("2. Merged Data/FinalDataset.csv")

# creates count of population older 65 and older
retirement_age = state_data|>
  select(State, CTYNAME, Year, POPESTIMATE,starts_with("AGE"))|>
  mutate(Retirement_Age_Proportion = AGE6569_PROP + AGE7074_PROP + 
           AGE7579_PROP + AGE8084_PROP + AGE85PLUS_PROP)|>
  filter(!is.na(State))

# Summarize the data to get a unique value for each State and Year
retirement_age_summary <- retirement_age %>%
  group_by(State, Year) %>%
  summarize(Retirement_Age_Proportion = mean(Retirement_Age_Proportion, na.rm = TRUE), .groups = 'drop')|>
  filter(!is.na(Retirement_Age_Proportion) & !is.nan(Retirement_Age_Proportion))

# Check the structure of the summarized data
print(head(retirement_age_summary))
print(unique(retirement_age_summary$Year))
print(unique(retirement_age_summary$State))

# Ensure Year is a factor with levels in the correct order
retirement_age_summary$Year <- factor(retirement_age_summary$Year, 
                                      levels = sort(unique(retirement_age_summary$Year)))

# Create the heatmap
ggplot(retirement_age_summary, aes(x = Year, y = State, fill = Retirement_Age_Proportion)) +
  geom_tile(color = "white") +  # Adds borders to tiles
  labs(
    title = "Retirement Age Proportion of State Population",
    x = "Year",
    y = "State",
    fill = "Retirement Age Proportion"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(option = "D") +
  theme(
    axis.text.x = element_text(angle = 80, hjust = 1, size = 8, color = "black"),  
    axis.text.y = element_text(size = 8, color = "black"), 
    plot.title = element_text(size = 14, color = "black", face = "bold"), 
    axis.title.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 12, color = "black") 
  ) +
  coord_fixed() -> retirement_heatmap


# Save the heatmap as a JPEG file
ggsave(filename = "Visualizations/retirement_heatmap.jpeg", plot = retirement_heatmap, width = 10, height = 6, dpi = 300)



