library(readxl)
library(tidyverse)

beetles <- read_excel("Horn_length_allometry_phylogeography_02_2023.xlsx")


#the paper i took this data from used a log10 distribution to linerize the relationshipts between the variables. i was able to figre out how to do it, it was easy enough so i added it in here just to see. 
beetles <- beetles %>%
  mutate(
    log_thorax = log10(Thorax_Width),
    log_horn = log10(Horn_Length)
  )
#because the slopes of the lines are whats really imporant on the log10 graph i need to calculate it. select 2 to get slope according to the internet
slopes <- beetles %>%
  group_by(Population) %>%
  summarise(
    model = list(lm(log_horn ~ log_thorax, data = cur_data()))
  ) %>%
  mutate(
    slope = map_dbl(model, ~ coef(.x)[2])
  )

slopes <- slopes %>%
  left_join(
    beetles %>%
      group_by(Population) %>%
      summarise(
        x_pos = min(log_thorax, na.rm = TRUE),
        y_pos = max(log_horn, na.rm = TRUE)
      ),
    by = "Population"
  )

#i faceted by location and added some color, the slopes of the lines are whats important here so i added lables for that info. 
ggplot(beetles, aes(x = log_thorax, y = log_horn, color = Population)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ Population) +
  #add slope lables
  geom_text(
    data = slopes,
    aes(x = x_pos, y = y_pos, label = paste0("slope = ", round(slope, 2))),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1
  ) +
  labs(
    title = "Allometry of Horn Length vs Thorax Width",
    x = "Log10 Thorax Width",
    y = "Log10 Horn Length"
  ) +
  theme_minimal()

