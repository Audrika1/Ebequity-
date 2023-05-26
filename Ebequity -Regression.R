# Load the required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(lmtest)

install.packages("lmtest")


install.packages("readxl")

library(readxl)

# Read the entire Excel sheet into a data frame
dt <- read_excel("/Users/audrika/Desktop/ebequity/regression.xlsx", sheet = "Sheet1")

dt <- read_excel("/Users/audrika/Desktop/ebequity/regression.xlsx", sheet = "Sheet1", col_names = TRUE)

names(dt)

# Create the regression model for LowFat dairy products 
model <- lm(`LowFat_Volume` ~ LowFat_Display + LowFat_Feature_and_Display + LowFat_Feature + LowFat_Multibuy + Big_Firm, data = dt)

# Print the model summary
summary(model)

# Create the regression model for regular dairy products 
model1 <- lm(Regular_Volume ~ Regular_Display + Regular_Feature + Regular_Feature_and_Display + Regular_Feature_and_Display + Regular_Multibuy, data = dt)

# Print the model summary
summary(model1)

Model3 with percentages: 
  dt$Regular_Volume_Percentage <- (dt$Regular_Volume / max(dt$Regular_Volume)) * 100
model3 <- lm(Regular_Volume_Percentage ~ Regular_Display + Regular_Feature + Regular_Feature_and_Display + Regular_Multibuy, data = dt)
summary(model3)

model4 <- lm(Regular_Volume_Percentage ~ Regular_Multibuy * Big_Firm, data = dt)
summary(model4)


# Fit the linear regression model
model3 <- lm(Regular_Volume_Percentage ~ Regular_Display + Regular_Feature + Regular_Feature_and_Display + Regular_Multibuy + Big_Firm, data = dt)

# Create the fitted line plot
ggplot(dt, aes(x = Regular_Display, y = Regular_Volume_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(x = "Regular_Display", y = "Regular_Volume_Percentage") 

# Create the fitted line plot
ggplot(dt, aes(x = Regular_Feature, y = Regular_Volume_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(x = "Regular_Feature", y = "Regular_Volume_Percentage") 

# Create the fitted line plot
ggplot(dt, aes(x = Regular_Multibuy, y = Regular_Volume_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(x = "Regular_Multibuy", y = "Regular_Volume_Percentage") 


# Create the fitted line plot
ggplot(dt, aes(x = Regular_Feature_and_Display, y = Regular_Volume_Percentage)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(x = "Regular_Feature_and_Display", y = "Regular_Volume_Percentage") 

