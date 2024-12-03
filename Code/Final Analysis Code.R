
#LOAD DATA AND LIBRARIES------------------------------

library(plm)
library(dplyr)
library(ggplot2)
library(glmnet)
library(tictoc)


file_path <- "C:/Users/d57n293/Documents/GitHub/ECNS_560_AS-RH/2. Merged Data/FinalDataset.csv"
FinalDataset <- read.csv(file_path, header = TRUE, sep = ",")

FinalDataset$UniqueID <- paste(data$State, data$CTYNAME, sep = "_")

# Convert the dataset to a panel data frame
FinalDataset <- pdata.frame(FinalDataset, index = c("UniqueID", "Year"))

# View the pdata.frame structure
summary(FinalDataset)
#---------------------------------------------------------------------------------------------------------------------------

#ELASTICNET MODEL

tic()
# Standardize numeric variables within each UniqueID (panel entity)
FinalDataset <- FinalDataset %>%
  group_by(UniqueID) %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector())) %>%
  ungroup()

# Remove rows with any missing values in the dataset
FinalDataset <- FinalDataset %>%
  drop_na()

# Step 2: Prepare Data for ElasticNet
# Define independent variables (X) and dependent variable (y)
X <- as.matrix(FinalDataset %>%
                 select(POPESTIMATE, AGE04_PROP:Employment.security.administration))
y <- FinalDataset$Annual_Debt_to_income_ratio_low

# Step 3: Run ElasticNet Regression

# Set the seed for reproducibility
set.seed(123)  # For reproducibility

# Fit ElasticNet model using cross-validation to find optimal lambda
elasticnet_model <- cv.glmnet(X, y, alpha = 0.5, family = "gaussian")

# View results and plot cross-validation results
print(elasticnet_model)
plot(elasticnet_model)

# Get the best lambda value (from cross-validation)
best_lambda <- elasticnet_model$lambda.min
print(best_lambda)

# Step 4: Extract Coefficients
# Get the coefficients for the optimal lambda
best_coefficients <- coef(elasticnet_model, s = "lambda.min")
print(best_coefficients)
toc()

# Optional Step 5: Handling Fixed Effects
# De-meaning: Remove fixed effects by subtracting the mean for each UniqueID
FinalDataset <- FinalDataset %>%
  group_by(UniqueID) %>%
  mutate(across(where(is.numeric), ~ . - mean(.))) %>%
  ungroup()

# After handling fixed effects, you can proceed to model fitting again if desired


#PANEL REGRESSION------------------------------------------------------------------------------------------------------------------------------------------

# Step 5: Extract Significant Variables
selected_variables <- rownames(best_coefficients)[which(best_coefficients != 0)]
selected_variables <- selected_variables[selected_variables != "(Intercept)"]  # Remove intercept

# Step 6: Prepare Data for Fixed Effects Panel Regression
FinalDataset_selected <- FinalDataset %>%
  select(UniqueID, Year, Annual_Debt_to_income_ratio_low, all_of(selected_variables)) %>%
  arrange(UniqueID, Year)

# Step 7: Run Fixed Effects Panel Regression
fixed_effects_model <- plm(Annual_Debt_to_income_ratio_low ~ ., 
                           data = FinalDataset_selected, 
                           index = c("UniqueID", "Year"), 
                           model = "within")

# View the summary of the fixed effects model
summary(fixed_effects_model)

# Optional: Cluster standard errors by UniqueID
coeftest(fixed_effects_model, vcov = function(x) vcovHC(x, type = "HC1"))




