# Load necessary libraries
library(dplyr)
library(readr)
library(fixest)
library(ggplot2)

# Load the datasets
mds_coords <- read_csv("C:/Users/USER/Desktop/MDA/mds_coords.csv")
# mds_coords <- mds_coords %>% filter(Wave != 1)  # Exclude observations from the first wave

democracy_index <- read_csv("C:/Users/USER/Desktop/MDA/democracy-index-by-source.csv")

# Function to generate year ranges for the lag value
generate_year_ranges <- function(lag) {
  return(list(
    `1` = c(1981 - lag, 1983 - lag),
    `2` = c(1990 - lag, 1992 - lag),
    `3` = c(1995 - lag, 1998 - lag),
    `4` = c(2000 - lag, 2004 - lag),
    `5` = c(2005 - lag, 2008 - lag),
    `6` = c(2010 - lag, 2014 - lag),
    `7` = c(2017 - lag, 2022 - lag)
  ))
}

# Function to assign lagged democracy index values
assign_shifted_democracy_index <- function(row, democracy_index, year_ranges) {
  country_code <- row$Country
  wave <- row$Wave
  year_range <- year_ranges[[as.character(wave)]]
  
  if (!is.null(year_range)) {
    filtered_df <- democracy_index %>%
      filter(Code == country_code, Year >= year_range[1], Year <= year_range[2])
    
    if (nrow(filtered_df) > 0) {
      return(filtered_df$`Electoral democracy index (best estimate, aggregate: average)`[1])
    }
  }
  return(NA)
}

# Loop to assign lagged democracy index values and run regressions
results <- data.frame(Lag = integer(), Coefficient = numeric(), `P-Value` = numeric(), SE = numeric(), stringsAsFactors = FALSE)

for (lag in -15:15) {
  # Generate year ranges for the lag
  year_ranges <- generate_year_ranges(lag)
  
  # Assign shifted electoral democracy index for the given lag
  mds_coords <- mds_coords %>%
    rowwise() %>%
    mutate(!!paste0("Shifted_Electoral_Democracy_Index_", ifelse(lag < 0, paste0('a', abs(lag)), lag)) := assign_shifted_democracy_index(cur_data(), democracy_index, year_ranges))
  
  # Filter out rows with missing values for regression
  regression_data <- mds_coords %>%
    filter(!is.na(!!sym(paste0("Shifted_Electoral_Democracy_Index_", ifelse(lag < 0, paste0('a', abs(lag)), lag)))), !is.na(Distance_to_Cluster_1))
  
  # Run TWFE regression using fixest (Fixed Effects)
  formula <- as.formula(paste("Cluster ~", paste0("Shifted_Electoral_Democracy_Index_", ifelse(lag < 0, paste0('a', abs(lag)), lag)), " + factor(Country) + factor(Wave)"))
  model <- lm(formula, data = regression_data)
  
  if (lag == 10) {
    model_lag_1 <- model
  }
  
  # Extract the coefficient, p-value, and standard error
  coef <- coef(model)[[paste0("Shifted_Electoral_Democracy_Index_", ifelse(lag < 0, paste0('a', abs(lag)), lag))]]
  p_value <- pvalue(model)[[paste0("Shifted_Electoral_Democracy_Index_", ifelse(lag < 0, paste0('a', abs(lag)), lag))]]
  se <- se(model)[[paste0("Shifted_Electoral_Democracy_Index_", ifelse(lag < 0, paste0('a', abs(lag)), lag))]]
  se
  
  # Store the result in the results dataframe
  results <- rbind(results, data.frame(Lag = lag, Coefficient = coef, `P-Value` = p_value, SE = se))
}

# Save the results to a CSV file
write_csv(results, "C:/Users/USER/Desktop/MDA/robustness_check_results_r.csv")

# Print the results
print(results)

# Visualization of results
# Create a smooth line plot for the coefficients over different lags, including confidence intervals
ggplot(results, aes(x = Lag, y = Coefficient)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_smooth(method = "loess", se = FALSE, color = "darkgreen") +
  geom_errorbar(aes(ymin = Coefficient - 1.645 * SE, ymax = Coefficient + 1.645 * SE), width = 0.2, color = "orange") +
  labs(title = "Coefficient Estimates Across Different Lags with 90% Confidence Intervals", 
       x = "Lag (Years)", 
       y = "Coefficient for Shifted Electoral Democracy Index") +
  theme_minimal()


if (!is.null(model_lag_1)) {
  stargazer(model_lag_1, type = "latex", se=a[1], omit = c("Country", "Wave"), out = "C:/Users/USER/Desktop/MDA/lag_1_regression_results.txt")
}

a = vcovCR(model_lag_1, cluster = regression_data$Country, type="CR0")
sqrt(a[109])
