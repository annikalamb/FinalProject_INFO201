setwd("C:/Users/nancy/Downloads/info 201")
library(tidyverse)

data <- read_delim("combine_data.csv")

filtered_data <- data %>% 
  filter(Student.group == "All Students")

filtered_data <- filtered_data %>%
  mutate(total_SAT = Math.2022.Average.Score + X2022.ELA.Average.Score)

filtered_data$X2021.2022.attendance.rate...year.to.date <- filtered_data$X2021.2022.attendance.rate...year.to.date * 100

ggplot(filtered_data, aes(x = X2021.2022.attendance.rate...year.to.date, y = total_SAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear trendline
  labs(title = "Scatterplot of Attendance Rate vs Total SAT Score",
       x = "Attendance Rate (Year-to-Date)",
       y = "Total SAT Score")

linear_model <- lm(total_SAT ~ X2021.2022.attendance.rate...year.to.date, data = filtered_data)

# Print the summary of the linear model to see the coefficients
summary(linear_model)

# Extract the coefficients
coefficients <- coef(linear_model)
intercept <- coefficients[1]
slope <- coefficients[2]

# Print the equation of the trendline
cat("Equation of the trendline: y =", round(intercept, 2), "+", round(slope, 2), "* x\n")

summary_linear_model <- summary(linear_model)

# Extract the R-squared value
r_squared <- summary_linear_model$r.squared

# Print the R-squared value
cat("R-squared value:", round(r_squared, 4), "\n")