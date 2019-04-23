# Packages
install.packages("tidyverse")

# Libraries
library("tidyverse")

# Import data
climate_spend <- read.csv("data/climate_spending.csv")
energy_spend <- read.csv("data/energy_spending.csv")
fed_spend <- read.csv("data/fed_r_d_spending.csv")

# Climate budget yearly growth by department
ggplot(data = climate_spend, mapping = aes(x = year, y = gcc_spending, color = department)) +
  geom_line() +
  scale_y_log10()


# Energy budget yearly growth by department
ggplot(data = energy_spend, mapping = aes(x = year, y = energy_spending, color = department)) +
  geom_line() +
  scale_y_log10()


# Federal budget yearly growth by department
ggplot(data = fed_spend, mapping = aes(x = year, y = rd_budget, color = department)) +
  geom_line() +
  scale_y_log10()


# GDP Growth yearly growth rate
# Get  yearly GDP by selecting only one department
yearly_gdp <- fed_spend %>%
  filter(department == "DOD") %>%
  select(year, gdp)

ggplot(data = yearly_gdp, mapping = aes(x = year, y = gdp)) +
  geom_line() +
  scale_y_log10()

# total_fed_spend
yearly_outlays <- fed_spend %>%
  filter(department == "DOD") %>%
  select(year, total_outlays)

# discretionary_fed_spend
yearly_discretionary_spending <- fed_spend %>%
  filter(department == "DOD") %>%
  select(year, discretionary_outlays)

# rate of change for yearly gdp
temp_rate <- 100*diff(yearly_gdp$gdp)/yearly_gdp[-nrow(yearly_gdp),]$gdp
length(temp_rate)




  