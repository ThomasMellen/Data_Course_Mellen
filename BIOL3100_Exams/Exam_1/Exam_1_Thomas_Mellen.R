library(tidyverse)
#downloads and loads tidyverse 
#1
covid_df <- read_csv("cleaned_covid_data.csv")
#2
A_states <- covid_df %>%
  filter(grepl("^A", Province_State))
#grepl returns a t/f and filter separates the data out
#3
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  #loess =  Locally Estimated Scatterplot Smoothing
  facet_wrap(~ Province_State, scales = "free") +
  theme_bw()
#4
state_max_fatality_rate <- covid_df %>%
  group_by(Province_State) %>%
  summarize(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))
#arrage for rows relocate for columns and na.rm ignores missing values
#5
ggplot(state_max_fatality_rate, aes(x = factor(Province_State, levels = Province_State), y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
#6
us_deaths <- covid_df %>%
  group_by(Last_Update) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE))
#creates total_deaths object
ggplot(us_deaths, aes(x = Last_Update, y = Total_Deaths)) +
  geom_line() +
  theme_bw()
  