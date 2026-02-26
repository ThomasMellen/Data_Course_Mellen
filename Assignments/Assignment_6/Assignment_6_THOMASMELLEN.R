library(tidyverse)
#relative path to read that csv
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")
names(dat)
head(dat)
dat_long <- dat %>%
  pivot_longer(
    cols = c(Hr_24, Hr_48, Hr_144),
    names_to = "Time",
    values_to = "Absorbance")
# this takes the 3 time intervals for the samples and stacks them into one collumn
dat_long <- dat_long %>%
  mutate(
    Time = as.numeric(str_remove(Time, "Hr_")))
# this takes all the new time interval values and removes the non numeric symbols so i can use them to make graphs
dat_long <- dat_long %>%
  rename(Sample = `Sample ID`)
#renaming the sample id column so it has no spaces
dat_long <- dat_long %>%
  mutate(
    Sample = factor(Sample),
    Substrate = factor(Substrate),
    Rep = factor(Rep))

dat_summary <- dat_long %>%
  group_by(Sample, Substrate, Time) %>%
  summarize(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"  )
#this finds the average value of the absorbance columns for every sample and time and substrate
ggplot(dat_summary, aes(x = Time, y = Mean_Absorbance, color = Sample)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Substrate) +
  theme_bw()
#graphs a different graph for every substrate time on the x absorbance (enzyme activity) on the y. different colors per line
ggsave(
  filename = "Absorbance_over_Time.png",
  width = 10,
  height = 6
)
install.packages("gganimate")
install.packages("gifski")

library(gganimate)
library(gifski)
#install packages that do animation things
