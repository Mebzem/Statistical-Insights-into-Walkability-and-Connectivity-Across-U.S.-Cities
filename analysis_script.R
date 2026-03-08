

library(readr) 
library(dplyr) 


file_path <- "data/urban_connectivity.csv" 
urban_data <- read_csv(file_path)


print("First 6 rows of the dataset:")
print(head(urban_data))


print("Structure of the dataset:")
str(urban_data)


print("Summary statistics:")
summary(urban_data)


# To Check how many missing values are in 'Transit Score'
missing_transit <- sum(is.na(urban_data$`Transit Score`))
print(paste("Number of missing Transit Scores:", missing_transit))


# We need 'Transit Score' for our models, so removing missing rows
# NOTE: This reduces the number of cities in the analysis to 97
urban_data_clean <- urban_data %>%
  filter(!is.na(`Transit Score`))

# Verify the number of rows before and after cleaning
print(paste("Rows before cleaning:", nrow(urban_data)))
print(paste("Rows after cleaning (removing NA Transit Score):", nrow(urban_data_clean)))

# Convert Categorical Variable to Factor
# The 'City_Population_Stats_density_classification' column will be used for ANOVA.
# We need to ensure R treats it as a categorical variable (factor).
urban_data_clean$`City_Population_Stats_density_classification` <- 
  factor(urban_data_clean$`City_Population_Stats_density_classification`)

# Check the levels (categories) of the factor
print("Levels of Density Classification factor:")
print(levels(urban_data_clean$`City_Population_Stats_density_classification`))

# Check the structure again to confirm the changes
print("Structure of the cleaned dataset:")
str(urban_data_clean)


print("Data loading and initial preparation complete. Ready for EDA")







# -----------------------------------------------------------------------------
# Step 2: Exploratory Data Analysis (EDA)
# -----------------------------------------------------------------------------

# --- Load Libraries for Plotting ---
# Install if needed: install.packages("ggplot2")
# Install if needed: install.packages("GGally") 
library(ggplot2) # For creating visualizations
library(dplyr)   # For data manipulation used later
library(GGally)  # For correlation plots (ggpairs)

# --- Create plots directory if it doesn't exist ---
# This ensures the code doesn't error if the folder is missing
if (!dir.exists("plots")) {
  dir.create("plots")
  print("Created 'plots' directory.")
}

# --- 1. Distributions of Key Continuous Variables ---

# Histogram for Walk Score
print("Generating Walk Score histogram...")
p_walk <- ggplot(urban_data_clean, aes(x = `Walk Score`)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Distribution of Walk Scores") +
  xlab("Walk Score") +
  ylab("Frequency (Number of Cities)") +
  theme_minimal()
print(p_walk)
ggsave("plots/histogram_walk_score.png", plot = p_walk, width = 6, height = 4, dpi = 300)

# Histogram for Transit Score
print("Generating Transit Score histogram...")
p_transit <- ggplot(urban_data_clean, aes(x = `Transit Score`)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  ggtitle("Distribution of Transit Scores") +
  xlab("Transit Score") +
  ylab("Frequency (Number of Cities)") +
  theme_minimal()
print(p_transit)
ggsave("plots/histogram_transit_score.png", plot = p_transit, width = 6, height = 4, dpi = 300)

# Histogram for Bike Score
print("Generating Bike Score histogram...")
p_bike <- ggplot(urban_data_clean, aes(x = `Bike Score`)) +
  geom_histogram(binwidth = 5, fill = "salmon", color = "black") +
  ggtitle("Distribution of Bike Scores") +
  xlab("Bike Score") +
  ylab("Frequency (Number of Cities)") +
  theme_minimal()
print(p_bike)
ggsave("plots/histogram_bike_score.png", plot = p_bike, width = 6, height = 4, dpi = 300)

# Histogram for Population Density (people/acre)
print("Generating Density histogram...")
p_density <- ggplot(urban_data_clean, aes(x = `City_Population_Stats_density__people_acre_`)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  ggtitle("Distribution of Population Density") +
  xlab("Density (People per Acre)") +
  ylab("Frequency (Number of Cities)") +
  theme_minimal()
print(p_density)
ggsave("plots/histogram_density.png", plot = p_density, width = 6, height = 4, dpi = 300)

# Histogram for Number of Playgrounds
print("Generating Playgrounds histogram...")
p_playgrounds <- ggplot(urban_data_clean, aes(x = `Number_of_Playgrounds`)) +
  geom_histogram(binwidth = 25, fill = "gold", color = "black") + # Adjust binwidth as needed
  ggtitle("Distribution of Number of Playgrounds") +
  xlab("Number of Playgrounds") +
  ylab("Frequency (Number of Cities)") +
  theme_minimal()
print(p_playgrounds)
ggsave("plots/histogram_playgrounds.png", plot = p_playgrounds, width = 6, height = 4, dpi = 300)


# --- 2. Relationships Between Variables ---

# Correlation plot for key numeric variables
print("Selecting variables for correlation plot...")

numeric_vars_for_corr <- urban_data_clean %>%
  select(`Walk Score`, `Transit Score`, `Bike Score`, 
         `City_Population_Stats_density__people_acre_`, `Number_of_Playgrounds`) %>%
  filter(complete.cases(.)) # Remove rows with NAs in these selected columns for ggpairs

print(paste("Number of rows for correlation plot after handling NAs:", nrow(numeric_vars_for_corr)))

# pairs plot (correlation coefficients, scatter plots, density plots)

print("Generating correlation plot (ggpairs)...")
corr_plot <- ggpairs(numeric_vars_for_corr)
print(corr_plot)
# Saving ggpairs object requires a specific approach if done via code, 
# manually exporting from the Plots pane is often easier for ggpairs.
# Or, save individual components if needed. Let's skip ggsave for this one for simplicity tonight.

# Boxplot of Walk Score by Density Classification (for ANOVA preview)
print("Generating Walk Score by Density boxplot...")
p_boxplot_density <- ggplot(urban_data_clean, aes(x = `City_Population_Stats_density_classification`, y = `Walk Score`, fill = `City_Population_Stats_density_classification`)) +
  geom_boxplot() +
  ggtitle("Walk Score by City Density Classification") +
  xlab("Density Classification") +
  ylab("Walk Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels if needed
print(p_boxplot_density)
ggsave("plots/boxplot_walk_score_by_density.png", plot = p_boxplot_density, width = 7, height = 5, dpi = 300)


# --- End of EDA ---
print("Exploratory Data Analysis complete. Plots saved to 'plots' folder (except ggpairs).")








# -----------------------------------------------------------------------------
# Step 3: Multiple Linear Regression
# -----------------------------------------------------------------------------
# Goal: Model Walk Score as a function of Transit Score, Bike Score, 
#       population density, and the number of playgrounds to understand 
#       which factors significantly predict walkability.

# --- Load necessary libraries (dplyr should already be loaded) ---
library(dplyr)

# --- 1. Rename Columns for Cleaner Code (Optional but Recommended) ---
# Renaming columns with spaces or special characters makes formulas easier to write and read.
# We will create a new dataframe 'model_data' with renamed columns for modeling purposes.

model_data <- urban_data_clean %>%
  rename(
    Walk_Score = `Walk Score`,
    Transit_Score = `Transit Score`,
    Bike_Score = `Bike Score`,
    Density_People_Acre = `City_Population_Stats_density__people_acre_`,
    Num_Playgrounds = `Number_of_Playgrounds`,
    Density_Classification = `City_Population_Stats_density_classification` 
  )

# Verify the renaming by checking the new column names
print("Column names after renaming for modeling:")
print(names(model_data))

# --- 2. Define and Run the Linear Model ---
# Using the dataframe with renamed columns ('model_data')
# Formula: Walk_Score ~ Transit_Score + Bike_Score + Density_People_Acre + Num_Playgrounds

print("Running Multiple Linear Regression model...")
lm_model <- lm(Walk_Score ~ Transit_Score + 
                 Bike_Score + 
                 Density_People_Acre + 
                 Num_Playgrounds, 
               data = model_data)

# --- 3. View Model Summary ---
# The summary() function provides detailed results of the fitted model.
print("Linear Regression Model Summary:")
summary(lm_model)

# --- 4. Interpretation Notes (For your presentation) ---
# Key components of the summary(lm_model) output:
#   a. Coefficients Table:
#      - 'Estimate': The estimated change in Walk_Score for a one-unit increase 
#                    in the predictor, holding all other predictors constant.
#      - 'Std. Error': The standard error of the coefficient estimate.
#      - 't value': The test statistic for the hypothesis that the coefficient is zero.
#      - 'Pr(>|t|)': The p-value associated with the t value. A small p-value 
#                    (typically < 0.05) indicates statistical significance, meaning 
#                    the predictor has a significant linear relationship with Walk_Score 
#                    after accounting for other predictors. Note the significance codes (stars).
#
#   b. Model Fit Statistics:
#      - 'Residual standard error': An estimate of the standard deviation of the errors (residuals).
#      - 'Multiple R-squared': The proportion of the variance in Walk_Score that is 
#                              explained by the predictors in the model. Ranges from 0 to 1.
#      - 'Adjusted R-squared': R-squared adjusted for the number of predictors in the model. 
#                              Useful for comparing models with different numbers of predictors.
#      - 'F-statistic' and 'p-value': Tests the overall significance of the model. A small 
#                                      p-value indicates that the model as a whole is 
#                                      statistically significant (i.e., the predictors together 
#                                      explain more variance than would be expected by chance).

# --- End of Linear Regression ---
print("Multiple Linear Regression analysis complete.")











# -----------------------------------------------------------------------------
# Step 4: Analysis of Variance (ANOVA)
# -----------------------------------------------------------------------------
# Goal: Determine if the mean Walk Score differs significantly across 
#       different city population density classifications.

# Using the dataframe with renamed columns ('model_data')
# Formula: Walk_Score ~ Density_Classification 
# This tests if the mean Walk_Score is different for at least one group defined by Density_Classification.

print("Running ANOVA model...")
aov_model <- aov(Walk_Score ~ Density_Classification, data = model_data)

# --- 2. View ANOVA Summary Table ---
# The summary() function provides the main ANOVA results (F-statistic, p-value).
print("ANOVA Model Summary:")
summary(aov_model)

# --- 3. Interpretation of ANOVA Summary ---
# Key components of the summary(aov_model) output:
#   a. Df (Degrees of Freedom): Information about the number of groups and observations.
#   b. Sum Sq (Sum of Squares): Measures of variability within and between groups.
#   c. Mean Sq (Mean Squares): Sum of Squares divided by Degrees of Freedom.
#   d. F value: The test statistic for ANOVA. It's the ratio of variance between groups 
#               to variance within groups. Larger values suggest bigger differences between groups.
#   e. Pr(>F) (p-value): The probability of observing an F-statistic as large as (or larger than) 
#                       the one calculated, assuming the null hypothesis (that all group means 
#                       are equal) is true. A small p-value (typically < 0.05) suggests we 
#                       reject the null hypothesis and conclude that at least one group mean is different.

# --- 4. Post-Hoc Test (if ANOVA is significant) ---
# If the ANOVA p-value (Pr(>F)) is less than 0.05, it tells us *at least one* group mean is
# different, but not *which* specific groups differ from each other.
# We use a post-hoc test like Tukey's Honest Significant Differences (HSD) for pairwise comparisons.

# First, check the p-value from the summary(aov_model) output.
# Let's assume for demonstration that it was significant (p < 0.05).

print("Running Tukey's HSD post-hoc test (if ANOVA was significant)...")
tukey_results <- TukeyHSD(aov_model)

# View the results of the pairwise comparisons
print("Tukey HSD Results:")
print(tukey_results)

# --- 5. Interpretation of Tukey HSD Results ---
# The output table shows pairwise comparisons between all group levels (e.g., "Low-High", "Medium-Low-High", etc.).
# Key columns:
#   a. diff: The difference between the means of the two groups being compared.
#   b. lwr / upr: The lower and upper bounds of the confidence interval for the difference in means.
#   c. p adj (adjusted p-value): The p-value for the specific pairwise comparison, adjusted 
#                                for making multiple comparisons. If p adj < 0.05, we conclude 
#                                there is a statistically significant difference between those two specific groups.

# --- End of ANOVA ---
print("ANOVA analysis complete.")




# -----------------------------------------------------------------------------
# Step 5: Multiple Logistic Regression
# -----------------------------------------------------------------------------
# Goal: Identify factors that predict the likelihood of a city having 
#       "High Walkability" (defined as Walk_Score > median).

# --- 1. Create the Binary Outcome Variable ---
# Calculate the median Walk_Score from our modeling dataset
median_walk_score <- median(model_data$Walk_Score, na.rm = TRUE) # na.rm=TRUE just in case
print(paste("Median Walk Score:", median_walk_score))

# 'High_Walk_Score' variable: 1 if Walk_Score > median, 0 otherwise
model_data <- model_data %>%
  mutate(High_Walk_Score = ifelse(Walk_Score > median_walk_score, 1, 0))

# Converting the new binary variable to a factor (good practice for classification)
model_data$High_Walk_Score <- factor(model_data$High_Walk_Score, levels = c(0, 1))

# Check the distribution of the new variable (how many cities are 0 vs 1)
print("Table of High_Walk_Score (0 = Below/Equal Median, 1 = Above Median):")
print(table(model_data$High_Walk_Score))

# --- 2. Define and Run the Logistic Regression Model ---
# Using the glm() function with family = binomial (for logistic regression)
# Formula: High_Walk_Score ~ Transit_Score + Bike_Score + Density_People_Acre + Num_Playgrounds

print("Running Multiple Logistic Regression model...")
log_model <- glm(High_Walk_Score ~ Transit_Score + 
                   Bike_Score + 
                   Density_People_Acre + 
                   Num_Playgrounds, 
                 data = model_data, 
                 family = binomial(link = "logit")) # Specify logistic regression

# --- 3. View Model Summary ---
# Provides coefficients (on the log-odds scale), standard errors, z-values, and p-values.
print("Logistic Regression Model Summary:")
summary(log_model)

# --- 4. Calculate and Interpret Odds Ratios ---
# Coefficients from logistic regression are log-odds. Exponentiating them gives Odds Ratios (ORs).
# OR > 1: Predictor increases the odds of the outcome (High_Walk_Score = 1).
# OR < 1: Predictor decreases the odds of the outcome.
# OR = 1: Predictor has no effect on the odds.

print("Odds Ratios:")
odds_ratios <- exp(coef(log_model))
print(odds_ratios)

# Confidence Intervals for Odds Ratios
print("Confidence Intervals for Odds Ratios:")
conf_int_odds_ratios <- exp(confint(log_model))
print(conf_int_odds_ratios)

# --- 5. Interpretation Notes (For your presentation) ---
# Key components of the summary(log_model) output:
#   a. Coefficients Table:
#      - 'Estimate': Log-odds change for a one-unit increase in the predictor. Harder to interpret directly.
#      - 'Pr(>|z|)': P-value for the significance of each predictor. Look for p < 0.05.
#
# Key components for interpretation (using Odds Ratios):
#   b. Odds Ratios (`odds_ratios` output):
#      - For Transit_Score: For a one-unit increase in Transit Score, the odds of having 
#                           High Walk Score are multiplied by [value from output].
#      - For Bike_Score: For a one-unit increase in Bike Score, the odds of having 
#                        High Walk Score are multiplied by [value from output].
#      - For Density: For a one-unit increase in Density, the odds of having 
#                     High Walk Score are multiplied by [value from output].
#      - For Num_Playgrounds: For a one-unit increase in Playgrounds, the odds of having 
#                             High Walk Score are multiplied by [value from output].
#   c. Confidence Intervals (`conf_int_odds_ratios` output):
#      - If the 95% CI for an odds ratio does *not* include 1.0, the predictor is 
#        statistically significant at the alpha=0.05 level. This usually aligns with the p-values.

# --- End of Logistic Regression ---
print("Multiple Logistic Regression analysis complete.")




# -----------------------------------------------------------------------------
# Step 7: Model Validation (Additional Steps)
# -----------------------------------------------------------------------------

# --- A. ANOVA Diagnostics ---
# Assumptions for ANOVA:
# 1. Independence of observations (assumed from data collection)
# 2. Normality of residuals (for each group, or overall residuals)
# 3. Homogeneity of variances (variances are equal across groups)

print("--- ANOVA Diagnostics ---")
if (exists("aov_model")) {
  # 2. Normality of Residuals for ANOVA
  print("ANOVA Residuals - Shapiro-Wilk Normality Test:")
  shapiro_test_aov_residuals <- shapiro.test(residuals(aov_model))
  print(shapiro_test_aov_residuals) # If p > 0.05, residuals are normally distributed
  
  print("ANOVA Residuals - Q-Q Plot:")
  par(mfrow=c(1,1)) # Ensure single plot layout
  qqnorm(residuals(aov_model))
  qqline(residuals(aov_model), col = "steelblue", lwd = 2)
  # Consider saving this plot:
  # png("plots/anova_qqplot_residuals.png"); qqnorm(residuals(aov_model)); qqline(residuals(aov_model), col = "steelblue", lwd = 2); dev.off()
  
  
  # 3. Homogeneity of Variances for ANOVA (Levene's Test)
  # Levene's test is available in the 'car' package (already loaded for VIF)
  print("ANOVA - Levene's Test for Homogeneity of Variances:")
  levene_test_aov <- leveneTest(Walk_Score ~ Density_Classification, data = model_data)
  print(levene_test_aov) # If p > 0.05, variances are homogeneous
  
} else {
  print("ANOVA model ('aov_model') not found. Skipping ANOVA diagnostics.")
}

# --- B. Logistic Regression Diagnostics & Evaluation ---
# Key aspects: Overall model fit, predictive accuracy

print("--- Logistic Regression Diagnostics & Evaluation ---")
if (exists("log_model")) {
  # 1. Pseudo R-squared (McFadden's)
  # Requires the pscl package (install if needed: install.packages("pscl"))
  # library(pscl)
  # print("Logistic Regression - McFadden's Pseudo R-squared:")
  # pseudo_r2_mcfadden <- pR2(log_model)["McFadden"]
  # print(pseudo_r2_mcfadden)
  # For simplicity tonight, we can rely on AIC and significance of predictors.
  # A more direct measure is a classification table.
  
  # 2. Classification Table (Confusion Matrix) & Accuracy
  print("Logistic Regression - Classification Table & Accuracy:")
  # Get predicted probabilities
  predicted_probs_log <- predict(log_model, type = "response")
  # Classify based on a 0.5 cutoff
  predicted_classes_log <- ifelse(predicted_probs_log > 0.5, 1, 0)
  predicted_classes_log <- factor(predicted_classes_log, levels = c(0, 1))
  
  # Create confusion matrix
  # Ensure High_Walk_Score is a factor with levels 0 and 1
  if (!is.factor(model_data$High_Walk_Score) || !all(levels(model_data$High_Walk_Score) == c("0", "1"))) {
    model_data$High_Walk_Score <- factor(model_data$High_Walk_Score, levels = c(0,1))
    print("Converted High_Walk_Score to factor for confusion matrix.")
  }
  
  # Check lengths
  if(length(predicted_classes_log) == length(model_data$High_Walk_Score)) {
    confusion_matrix_log <- table(Actual = model_data$High_Walk_Score, Predicted = predicted_classes_log)
    print("Confusion Matrix:")
    print(confusion_matrix_log)
    
    # Calculate accuracy
    accuracy_log <- sum(diag(confusion_matrix_log)) / sum(confusion_matrix_log)
    print(paste("Overall Accuracy:", round(accuracy_log, 4)))
    
    # Sensitivity (True Positive Rate)
    sensitivity_log <- confusion_matrix_log[2,2] / sum(confusion_matrix_log[2,])
    print(paste("Sensitivity (Recall for Class 1):", round(sensitivity_log, 4)))
    
    # Specificity (True Negative Rate)
    specificity_log <- confusion_matrix_log[1,1] / sum(confusion_matrix_log[1,])
    print(paste("Specificity (Recall for Class 0):", round(specificity_log, 4)))
  } else {
    print("Length mismatch between predicted classes and actual values. Skipping confusion matrix.")
    print(paste("Length of predicted_classes_log:", length(predicted_classes_log)))
    print(paste("Length of model_data$High_Walk_Score:", length(model_data$High_Walk_Score)))
  }
  
  
  # 3. ROC Curve and AUC (Area Under Curve)
  # Requires the pROC package (install if needed: install.packages("pROC"))
  library(pROC)
  print("Logistic Regression - ROC Curve and AUC:")
  roc_curve_log <- roc(response = model_data$High_Walk_Score, predictor = predicted_probs_log, quiet = TRUE)
  print(paste("AUC (Area Under ROC Curve):", round(auc(roc_curve_log), 4)))
  
  # Plot ROC Curve
  # png("plots/logistic_roc_curve.png"); plot(roc_curve_log, main="ROC Curve for Logistic Regression", col="blue", lwd=2); abline(a=0, b=1, lty=2, col="gray"); dev.off()
  plot(roc_curve_log, main="ROC Curve for Logistic Regression", col="blue", lwd=2, print.auc=TRUE)
  abline(a=0, b=1, lty=2, col="gray") # Adds the diagonal line for reference (random chance)
  
  
} else {
  print("Logistic Regression model ('log_model') not found. Skipping diagnostics.")
}

print("Model validation checks complete.")

# -----------------------------------------------------------------------------
# Linear Regression Model Diagnostics
# -----------------------------------------------------------------------------
# This code assumes your linear regression model object is named 'lm_model'
# and that 'lm_model' has already been created by running:
# lm_model <- lm(Walk_Score ~ Transit_Score + Bike_Score + Density_People_Acre + Num_Playgrounds, data = model_data)

if (exists("lm_model")) {
  print("Generating Linear Regression Diagnostic Plots...")
  
  # Set up a 2x2 plotting area to display all four plots together
  par(mfrow = c(2, 2)) 
  
  # Generate the four standard diagnostic plots
  plot(lm_model)
  
  # Reset the plotting layout to default (1 plot per page)
  par(mfrow = c(1, 1)) 
  
  print("Linear Regression diagnostic plots generated.")
  print("Consider saving these plots for your report (e.g., using the 'Export' button in the Plots pane or ggsave alternatives for base R plots).")
  
  # Optional: To save these plots to a file directly from code:
  # png("plots/linear_regression_diagnostics.png", width = 800, height = 800) # Open PNG device
  # par(mfrow = c(2, 2)) # Set 2x2 layout
  # plot(lm_model)       # Create plots
  # par(mfrow = c(1, 1)) # Reset layout
  # dev.off()            # Close PNG device
  # print("Diagnostic plots also saved to plots/linear_regression_diagnostics.png")
  
} else {
  print("Linear regression model ('lm_model') not found. Please run the lm() command first.")
}
