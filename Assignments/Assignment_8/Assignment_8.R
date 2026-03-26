library(tidyverse)
library(modelr)
library(easystats)
library(broom)

# Load the mushroom growth data
data <- read_csv("mushroom_growth.csv")
# load all the libraries I'm using and read the data from the file

# Make 4 graphs with different variables compared to GrowthRate
ggplot(data, aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(data, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(data, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

ggplot(data, aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Make linear models describing how these variables explain GrowthRate
mod1 <- lm(GrowthRate ~ Temperature, data = data)
mod2 <- lm(GrowthRate ~ Temperature + Light, data = data)
mod3 <- lm(GrowthRate ~ Temperature * Light, data = data)
mod4 <- lm(GrowthRate ~ Temperature + Light + Humidity + Nitrogen, data = data)

# Calculate mean squared error for each model
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)
# Looks like the model with the lowest error was model 4. This makes sense because it uses all the variables

# Check model performance with easystats
performance(mod1)
performance(mod2)
performance(mod3)
performance(mod4)
# Confirms that mod4 is the best model

# Fit the model
mod4 <- lm(GrowthRate ~ Temperature + Light + Humidity + Nitrogen, data = data)

# Real data, add Type column
data_real <- data %>%
  select(Nitrogen, GrowthRate) %>%
  mutate(Type = "Real")

# Predicted data along Nitrogen gradient
new_data <- data.frame(
  Temperature = mean(data$Temperature),
  Light = mean(data$Light),
  Nitrogen = seq(min(data$Nitrogen), max(data$Nitrogen), length.out = 50),
  Humidity = factor("High", levels = levels(data$Humidity))
)

# Add predicted GrowthRate and Type column
new_data <- new_data %>%
  mutate(GrowthRate = predict(mod4, newdata = new_data),
         Type = "Predicted") %>%
  select(Nitrogen, GrowthRate, Type)  # keep same columns as data_real

# Combine
full_data <- bind_rows(data_real, new_data)

# Plot
ggplot(full_data, aes(x = Nitrogen, y = GrowthRate, color = Type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Real" = "red", "Predicted" = "blue")) +
  labs(x = "Nitrogen", y = "Growth Rate") +
  theme_minimal()
#ive spent 3 hours and i cant get this stupid graph to plot blue points. i have no idea i give up dock me points

