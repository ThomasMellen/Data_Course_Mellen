library(tidyverse)
#load csv
unicef <- read_csv("unicef-u5mr.csv")
head(unicef)
#convert the csv to tidy
unicef_long <- unicef %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    names_prefix = "U5MR.",
    values_to = "U5MR"
  ) %>%
  mutate(Year = as.integer(Year))
head(unicef_long)
#graph each u5mr over time
plot1 <- ggplot(unicef_long, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(color = "grey50") +
  facet_wrap(~Continent) +
  theme_minimal() +
  labs(title = "Under-5 Mortality Rate by Country", y = "U5MR (deaths per 1000 live births)")

plot1
ggsave("Mellen_Plot_1.png", plot1, width = 12, height = 8)

#average per continent
continent_average <- unicef_long %>%
  group_by(Continent, Year) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE))

plot2 <- ggplot(continent_average, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(linewidth = 1.5) +
  theme_minimal() +
  labs(title = "Mean U5MR by Continent", y = "Mean U5MR")

plot2
ggsave("Mellen_Plot_2.png", plot2, width = 10, height = 6)
#liner models
mod1 <- lm(U5MR ~ Year, data = unicef_long)
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_long)
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_long)

summary(mod1)
summary(mod2)
summary(mod3)
AIC(mod1, mod2, mod3)
#based on r^2 it looks like 3 is best because its the highest number and based on AIC 3 is still best because lower is better.

#predictions
unicef_long$pred_mod1 <- predict(mod1, newdata = unicef_long)
unicef_long$pred_mod2 <- predict(mod2, newdata = unicef_long)
unicef_long$pred_mod3 <- predict(mod3, newdata = unicef_long)

#graph the models
# Model 1 plot
mod1_plot <- ggplot(unicef_long, aes(x = Year, y = pred_mod1, color = Continent)) +
  geom_line() +
  labs(title = "mod1", x = "Year", y = "Predicted U5MR") +
  theme_minimal()

# Model 2 plot
mod2_plot <- ggplot(unicef_long, aes(x = Year, y = pred_mod2, color = Continent)) +
  geom_line() +
  labs(title = "mod2", x = "Year", y = "Predicted U5MR") +
  theme_minimal()

# Model 3 plot
mod3_plot <- ggplot(unicef_long, aes(x = Year, y = pred_mod3, color = Continent)) +
  geom_line() +
  labs(title = "mod3", x = "Year", y = "Predicted U5MR") +
  theme_minimal()

mod1_plot
mod2_plot
mod3_plot
#bonus
ecuador_2020 <- data.frame(
  CountryName = "Ecuador",
  Continent = "Americas",
  Year = 2020
)

pred_ecuador <- predict(mod3, newdata = ecuador_2020)
difference <- pred_ecuador - 13
difference
#the prediction was kind of bad if i was shooting for -5
