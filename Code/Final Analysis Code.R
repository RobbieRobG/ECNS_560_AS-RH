
#LOAD DATA AND LIBRARIES------------------------------

library(plm)
library(dplyr)
library(ggplot2)
library(glmnet)
library(tictoc)
library(readxl)



file_path <- "2. Merged Data/FinalDataset1.xlsx"
FinalDataset <- read_excel(file_path)


#Generating an ID variable for the counties
FinalDataset$UniqueID <- paste(FinalDataset$State, FinalDataset$CTYNAME, sep = "_")


# Convert the dataset to a panel data frame
FinalDataset <- pdata.frame(FinalDataset, index = c("UniqueID", "Year"))

# View the pdata.frame structure
View(FinalDataset)


#ELASTICNET MODEL---------------------------------------------------------------------------------------------------------------------------

#ELASTICNET MODEL

# Standardize numeric variables within each UniqueID (panel entity)
FinalDataset <- FinalDataset %>%
  group_by(UniqueID) %>%
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector())) %>%
  ungroup()

# Step 2: Prepare Data for ElasticNet
# Define independent variables (X) and dependent variable (y)

X <- as.matrix(FinalDataset %>%
                 select(
                   POPESTIMATE, AGE04_PROP, AGE59_PROP, AGE1014_PROP, AGE1519_PROP, AGE2024_PROP, AGE2529_PROP,
                   AGE3034_PROP, AGE3539_PROP, AGE4044_PROP, AGE4549_PROP, AGE5054_PROP, AGE5559_PROP,
                   AGE6064_PROP, AGE6569_PROP, AGE7074_PROP, AGE7579_PROP, AGE8084_PROP, AGE85PLUS_PROP,
                   Real.GDP..millions.of.chained.2017.dollars..1., Compensation..millions.of.dollars.,
                   Gross.operating.surplus..millions.of.dollars.,
                   Taxes.on.production.and.imports..TOPI...millions.of.dollars., Subsidies..millions.of.dollars.,
                   From.State.government1, Taxes, Property, Sales.and.gross.receipts, General.sales, Selective.sales,
                   Motor.fuel, Alcoholic.beverage, Tobacco.products, Public.utilities, Other.selective.sales,
                   Individual.income, Corporate.income, Motor.vehicle.license, Other.taxes,
                   Charges.and.miscellaneous.general.revenue, Current.charges, Education,
                   School.lunch.sales..gross., Hospitals, Highways, Air.transportation..airports.,
                   Parking.facilities, Sea.and.inland.port.facilities, Natural.resources,
                   Parks.and.recreation, Housing.and.community.development, Sewerage, Solid.waste.management,
                   Other.charges, Capital.outlay, Higher.education, Elementary...secondary, Other.education,
                   Libraries, Public.welfare, Cash.assistance.payments, Vendor.payments, Other.public.welfare,
                   Health, Veterans..services, Police.protection, Debt.outstanding, Employment.security.administration
                 )
)

# We exclude as redundant: Chain.type.quantity.indexes.for.real.GDP, Current.dollar.GDP..millions.of.current.dollars. Taxes.on.production.and.imports..TOPI..less.subsides..millions.of.dollars.

y <- FinalDataset$Annual_Debt_to_income_ratio_low


#Remove missing values

complete_cases <- complete.cases(X, y)
X <- X[complete_cases, ]
y <- y[complete_cases]


# Step 3: Run ElasticNet Regression

# Set the seed for reproducibility
set.seed(123)  # For reproducibility


# Define training (80%) and testing (20%) indices
train_indices <- sample(seq_len(nrow(X)), size = 0.8 * nrow(X))

# Split the data
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]



# Fit ElasticNet model using cross-validation to find optimal lambda
elasticnet_model <- cv.glmnet(X_train, y_train, alpha = 0.5, family = "gaussian")

# Predict on the test set
predictions <- predict(elasticnet_model, newx = X_test, s = "lambda.min")

# Calculate performance metrics (e.g., Mean Squared Error)
mse <- mean((y_test - predictions)^2)
cat("Mean Squared Error on Test Set:", mse, "\n")




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



#PANEL REGRESSION BASED OFF OF THE BEST VARIABLES FROM ELASTICNET MODEL ------------------------------------------------------------------------------------------------------------------------------------------

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


