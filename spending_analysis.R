# Packages
install.packages("tidyverse")

# Libraries
library("tidyverse")

# Import data
climate_spend <- read.csv("data/climate_spending.csv")
energy_spend <- read.csv("data/energy_spending.csv")
fed_spend <- read.csv("data/fed_r_d_spending.csv")

ggplot(data = climate_spend, mapping = aes(x = year, y = gcc_spending, color = department)) +
  geom_line() +
  scale_y_log10() 
  