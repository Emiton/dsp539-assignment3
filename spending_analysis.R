# Packages
install.packages("tidyverse")
install.packages("reshape2")

# Libraries
library("tidyverse")
library("reshape2")

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
yearly_disc_spend <- fed_spend %>%
  filter(department == "DOD") %>%
  select(year, discretionary_outlays)



# A1 - Method 1: gdp roc with total_spend and disc_spend [R2]

## rate of change for yearly gdp
gdp_change <- 100*diff(yearly_gdp$gdp)/yearly_gdp[-nrow(yearly_gdp),]$gdp
total_outlay_change <- 100*diff(yearly_outlays$total_outlays)/yearly_outlays[-nrow(yearly_outlays),]$total_outlays
disc_outlay_change <- 100*diff(yearly_disc_spend$discretionary_outlays)/yearly_disc_spend[-nrow(yearly_discretionary_spending),]$discretionary_outlays

gdp_budget_roc <- data.frame("Year" = yearly_gdp$year[2:42], 
                             "gdp_change" = gdp_change,
                             "outlay_change" = total_outlay_change,
                             "disc_change" = disc_outlay_change)

p <- ggplot() +
  geom_line(data = gdp_budget_roc, aes(x = Year, y = gdp_change), color = "blue") +
  geom_line(data = gdp_budget_roc, aes(x = Year, y = outlay_change), color = "red") + 
  geom_line(data = gdp_budget_roc, aes(x = Year, y = disc_change), color = "green") +
  xlab("Year") + 
  ylab("Rate of change (%)")

print(p)


# A1 - Method 2: Melt data and then plot
roc_melt <- melt(gdp_budget_roc, id=c("Year"))

ggplot(roc_melt) +
  geom_line(aes(x = Year, y = value, color=variable)) +
  scale_color_manual(values = c("red","green", "blue"),
                     labels = c("GDP", "Total Spending", "Discretionary Spending"))


# A2. gdp roc vs per group roc (fed, energy, climate) [R2]

# A3. gdp roc versus every department [R2]

# 1,2 3 repeated with total spending in place of gdp
# 1,2,3 repeated with disc spending in place of gdp

# budget vs time for all groups (fed, climate, energy) separate department by color

# average rate of change for all groups vs each other [R2]

# gdp versus [R2]
#   total spend
#   disc spend

# # EXTRA # #

# Put budgets into buckets
#   perform analyses based on bucket
#   e.g. mean for bucket 1 is doing blah blah over these years...








  