library(tidyverse)
library(broom)

faculty <- read.csv("FacultySalaries_1995.csv")
#clean data and arrange it to recreate that graph
faculty_long <- faculty %>%
  pivot_longer(
    cols = contains("Salary"),
    names_to = "Rank",
    values_to = "Salary"
  ) %>%
  mutate(
    Rank = str_remove(Rank, "Salary_")
  )
#make the graph 3 split graph by tier with salary on y and rank of proffesor on x
ggplot(faculty_long, aes(x = Rank, y = Salary, fill = Rank)) +
  geom_boxplot() +
  facet_wrap(~ Tier)
#make an anova model predicting salary from state + tier + rank
anova_model <- aov(Salary ~ State + Tier + Rank, data = faculty_long)

summary(anova_model)
#looks like state is the best predictor
#read in new csv
juniper <- read.csv("Juniper_Oils.csv", check.names = FALSE)
#makes all the chemical names into a table called chemicals
chemicals <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene",
               "alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
               "beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol",
               "cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol",
               "alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")

#clean data. turns the names of all the columns into a column called chemical id. then it takes the values from the chemicals and puts it into a column called concentration
juniper_long <- juniper %>%
  pivot_longer(
    cols = all_of(chemicals),
    names_to = "ChemicalID",
    values_to = "Concentration"
  )
#make plot faceted by chemical
ggplot(juniper_long, aes(x = YearsSinceBurn, y = Concentration)) +
  geom_smooth() +
  facet_wrap(~ ChemicalID, scales = "free_y")
#setup glm model to fit concentration by chemicalid*yearssinceburn and filters results by a significant p value
glm_model <- glm(Concentration ~ ChemicalID * YearsSinceBurn, data = juniper_long)

glm_results <- tidy(glm_model)

significant_results <- glm_results %>%
  filter(p.value < 0.05)

significant_results
