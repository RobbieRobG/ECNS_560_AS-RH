
#LOAD DATA AND LIBRARIES------------------------------

library(plm)
library(dplyr)
library(ggplot2)
library(glmnet)
library(tictoc)
library(readxl)



file_path <- "C:/Users/d57n293/Documents/GitHub/ECNS_560_AS-RH/2. Merged Data/FinalDataset1.xlsx"
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


# CREATING A BUNCH OF PLOTS FOR THE REPORT-----------------------------------------------------------------

# Libraries for plots and tables
library(knitr)
library(kableExtra)
library(gridExtra)
library(ggplot2)


# Plot 1: Cross-validation plot from ElasticNet------------------------------


cv_error <- elasticnet_model$cvm
lambda_values <- elasticnet_model$lambda

# Create a data frame for plotting
cv_data <- data.frame(Lambda = lambda_values, CV_Error = cv_error)

# Plot the cross-validation error using ggplot2
cv_plot <- ggplot(cv_data, aes(x = Lambda, y = CV_Error)) +
  geom_line() +
  scale_x_log10() +  # Log scale for lambda
  labs(title = "Cross-Validation Error vs. Lambda", x = "Lambda", y = "Cross-Validation Error") +
  theme_minimal()

# Save the plot as JPEG
ggsave(filename = paste(output_dir, "/ElasticNet_CV_Plot.jpg", sep = ""), plot = cv_plot, width = 8, height = 6, dpi = 300)



# 2. Coefficient Plot from ElasticNet------------------------------

# Convert the coefficients to a data frame
coefficients_matrix <- as.matrix(best_coefficients)  # Convert coefficients to a matrix
coefficients_df <- data.frame(Coefficient = coefficients_matrix[, 1])  # Extract the first column (the actual coefficients)

# Add the variable names as a column
coefficients_df$Variable <- rownames(coefficients_matrix)

# Filter out intercept and zero coefficients
coefficients_df <- coefficients_df[coefficients_df$Coefficient != 0, ]
coefficients_df <- coefficients_df[coefficients_df$Variable != "(Intercept)", ]  # Remove intercept

# Reorder the variables by coefficient values for better visualization
coefficients_df$Variable <- factor(coefficients_df$Variable, levels = coefficients_df$Variable[order(coefficients_df$Coefficient)])

# Create the coefficient plot
coefficients_plot <- ggplot(coefficients_df, aes(x = Variable, y = Coefficient)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x labels for better readability
  labs(title = "ElasticNet Coefficients", x = "Variables", y = "Coefficients") +
  theme_minimal()

# Save the plot as JPEG
ggsave(filename = paste(output_dir, "/ElasticNet_Coefficients_Plot_Manual.jpg", sep = ""), 
       plot = coefficients_plot, width = 8, height = 6, dpi = 300)





# Plot 3: Residuals Plot-----------------------------------------

# Calculate residuals
# Calculate residuals
residuals <- y_test - predictions

# Create a data frame for residuals with the correct column name
residuals_df <- data.frame(Residuals = residuals)

# Ensure the column name is correctly set to "Residuals"
colnames(residuals_df) <- c("Residuals")

# Check the structure of the residuals_df data frame
str(residuals_df)

# Verify the column names
print(colnames(residuals_df))  # Should print "Residuals"

# Plot residuals distribution
residuals_plot <- ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Save the residuals plot as JPEG
ggsave(filename = paste(output_dir, "/Residuals_Plot_Manual.jpg", sep = ""), 
       plot = residuals_plot, width = 8, height = 6, dpi = 300)




# Plot 4: Prediction vs Actual Plot

# Ensure predictions is a vector (if it's a matrix, convert it to a vector)
predictions <- as.vector(predictions)

# Create a data frame for Predicted vs Actual values
pred_vs_actual_df <- data.frame(Predicted = predictions, Actual = y_test)

# Verify the column names of the data frame
print(colnames(pred_vs_actual_df))  # Should print "Predicted" and "Actual"

# Plot Predicted vs Actual values
pred_vs_actual_plot <- ggplot(pred_vs_actual_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs Actual", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Save the Predicted vs Actual plot as JPEG
ggsave(filename = paste(output_dir, "/Predicted_vs_Actual_Plot_Manual.jpg", sep = ""), 
       plot = pred_vs_actual_plot, width = 8, height = 6, dpi = 300)




# Plot 5: Significant Variables in Fixed Effects Model
fixed_effects_coeffs <- summary(fixed_effects_model)$coefficients
fixed_effects_df <- data.frame(Variable = rownames(fixed_effects_coeffs), 
                               Coefficient = fixed_effects_coeffs[, 1], 
                               P_value = fixed_effects_coeffs[, 4])
significant_vars <- fixed_effects_df[fixed_effects_df$P_value < 0.05, ]
significant_vars_plot <- ggplot(significant_vars, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  coord_flip() +
  labs(title = "Significant Variables in Fixed Effects Model", x = "Variables", y = "Coefficient") +
  theme_minimal()
ggsave(filename = paste(output_dir, "/Significant_Variables_Plot_Manual.jpg", sep = ""), 
       plot = significant_vars_plot, width = 8, height = 6, dpi = 300)