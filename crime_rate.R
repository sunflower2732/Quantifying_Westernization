# Load necessary packages
library(dplyr)
library(readr)
library(lmtest)
library(sandwich)
library(ggplot2)
library(ggforce)
library(stargazer)

# Load the datasets
complete_dataset <- read_csv("C:/Users/USER/Desktop/MDA/complete_dataset.csv")
mds_coords <- read_csv("C:/Users/USER/Desktop/MDA/mds_coords.csv")

# Merge the datasets on the 'Country_Wave' column
merged_data <- complete_dataset %>% 
  inner_join(mds_coords, by = "Country_Wave")

# First Regression Model without grouping by country

# Create log of GDP_pc and square of Distance_to_Cluster_1
merged_data <- merged_data %>% 
  mutate(log_GDP_pc = log(GDP_pc),
         Distance_to_Cluster_1_sq = Distance_to_Cluster_1^2)

# Plot countries according to Dim1 and Dim2, and add Distance_to_Cluster_1 as color with enhanced contrast
# Build convex hull for points where Distance_to_Cluster_1 == 0
hull_data <- merged_data %>% 
  filter(Distance_to_Cluster_1 == 0) %>% 
  select(Dim1, Dim2) %>% 
  slice(chull(Dim1, Dim2))

# Plot with convex hull for points with Distance_to_Cluster_1 == 0 and color gradient for others
ggplot(merged_data, aes(x = Dim1, y = Dim2)) +
  geom_point(aes(color = Distance_to_Cluster_1), size = 2) +
  geom_text(aes(label = Country), vjust = -0.5, hjust = 0.5) +
  geom_polygon(data = hull_data, aes(x = Dim1, y = Dim2), fill = NA, color = "black", linetype = "dashed", size = 1) +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = median(merged_data$Distance_to_Cluster_1, na.rm = TRUE),
                        na.value = "grey", guide = "colourbar") +
  labs(title = "Countries Plot by Dim1 and Dim2 with Distance to West",
       x = "Dimension 1",
       y = "Dimension 2",
       color = "Distance to Cluster 1") +
  theme_minimal()

# Run regression - First model
# Crime_Rate on GDP_growth + log_GDP_pc + Unemp_Rate + Education + Distance_to_Cluster_1 + Distance_to_Cluster_1_sq
model_1 <- lm(Crime_Rate ~ GDP_growth + log_GDP_pc + Unemp_Rate + Education + Distance_to_Cluster_1 + Distance_to_Cluster_1_sq, 
              data = merged_data)

# Summary of the regression model
summary(model_1)

# Correct standard errors with heteroskedasticity-robust SE
robust_se_1 <- vcovHC(model_1, type = "HC1")
robust_se_diag_1 <- sqrt(diag(robust_se_1))
coeftest(model_1, robust_se_1)

# Perform additional tests if needed, for instance, heteroskedasticity check
bptest(model_1)

# Create a LaTeX table for the regression output using stargazer

# Uncomment and run the additional grouping by country to create another model
merged_data_grouped <- merged_data %>% 
  group_by(Country) %>% 
  filter(row_number() == n()) %>% 
  ungroup()

# Run regression - Second model
model_2 <- lm(Crime_Rate ~ GDP_growth + log_GDP_pc + Unemp_Rate + Education + Distance_to_Cluster_1 + Distance_to_Cluster_1_sq, 
              data = merged_data_grouped)

# Summary of the second regression model
summary(model_2)

# Correct standard errors with heteroskedasticity-robust SE - Second model
robust_se_2 <- vcovHC(model_2, type = "HC1")
robust_se_diag_2 <- sqrt(diag(robust_se_2))
coeftest(model_2, robust_se_2)

# Create a LaTeX table comparing both regression models
stargazer(model_1, model_2, type = "latex", se = list(robust_se_diag_1, robust_se_diag_2), out = "regression_output_comparison.tex")
