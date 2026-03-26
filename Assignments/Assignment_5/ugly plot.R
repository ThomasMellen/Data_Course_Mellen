library(tidyverse)
library(ggplot2)
install.packages("ggimage")
library(ggimage)

# load my data set

TempC = read.csv("./Ballinger_et_al_SAT_1900_2024_version1_DASHBOARD-FINAL.csv")
#takes the data columns and combines them and adds all the numbers to the frame Temperature and the names to the frame Location
TempC_long <- TempC %>%
  pivot_longer(
    cols = c(Arctic_Annual_data, Global_Annual_data),
    names_to = "Location",
    values_to = "Temperature"
  )
# makes a new column in the dataset called image and assigns a photo in the working directory to each row depending on location
TempC_long$image <- ifelse(
  TempC_long$Location == "Arctic_Annual_data",
  "C:/Users/Thomas/Desktop/Data_Course_Mellen/Assignments/Assignment_5/arctic.png",
  "C:/Users/Thomas/Desktop/Data_Course_Mellen/Assignments/Assignment_5/earth.png"
)

#makes a new column in the dataset called Spiral_radius that controlls the y axis to spiral out.
TempC_long$spiral_radius <- TempC_long$Temperature + (TempC_long$Year - min(TempC_long$Year)) * 0.1

TempC_long$index <- as.numeric(factor(TempC_long$Year))

#graphs the graph as a standard line plot to make sure the data works
ggplot(TempC_long, aes(x = Year, y = Temperature, color = Location)) +
  geom_line() +
  theme_bw()
#i wanted to turn it into a circle
ggplot(TempC_long, aes(x = Year, y = Temperature, color = Location)) +
  geom_line() +
  coord_polar() +
  theme_bw()
#the rest is making it super ugly

ugly_plot <- ggplot(TempC_long, aes(x = index, y = spiral_radius)) +
  
  geom_image(aes(image = image), size = 0.1) +
  geom_line(aes(color = Location), linewidth = 2) +
  scale_x_continuous(
    breaks = TempC_long$index,
    labels = TempC_long$Year
  ) +

  coord_polar() +
  
  scale_color_manual(values = c("#30e68b", "cyan")) +
  
  ggtitle("WhAT TemPerATuRE Is It REAlly?") +
  
  labs(
    x = "Year",
    y = "Surface Air Temperature"
  ) +
  
  theme(
    plot.title = element_text(size = 28, color = "red", face = "bold"),
    panel.background = element_rect(fill = "#000000"),
    legend.background = element_rect(fill = "#292929"),
    axis.text = element_text(size = 15, color = "#e8b0c5", angle = 90)
  )
#reverses y axis scale
ugly_plot <- ugly_plot +
  scale_y_reverse(
    limits = c(max(TempC_long$spiral_radius), min(TempC_long$spiral_radius))
  )


ugly_plot

ggsave("ugly_plot.jpg", plot = ugly_plot, width = 8, height = 8)
