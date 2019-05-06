# TODO
# R squared
# P-values
# Make PDF
# Write descriptons for graphs

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



# Climate budget yearly growth by department + linear model
ggplot(data = climate_spend, mapping = aes(x = year, y = gcc_spending, color = department)) +
  geom_line() +
  scale_y_log10() +
  ggtitle("Figure X: US Climate Spending per department") +
  xlab("Year") + 
  ylab("Dollars Spent (USD)")

# Energy budget yearly growth by department
ggplot(data = energy_spend, mapping = aes(x = year, y = energy_spending, color = department)) +
  geom_line() +
  scale_y_log10() +
  ggtitle("Figure X: US Energy Spending per department") +
  xlab("Year") + 
  ylab("Dollars Spent (USD)")


# Federal budget yearly growth by department
ggplot(data = fed_spend, mapping = aes(x = year, y = rd_budget, color = department)) +
  geom_line() +
  scale_y_log10() +
  ggtitle("Figure X: US Federal Spending per department") +
  xlab("Year") + 
  ylab("Dollars Spent (USD)")


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

# GDP, spending discretionary spending
all_spending <- data.frame("year" = yearly_gdp$year,
                           "gdp" = yearly_gdp$gdp,
                           "total_spending" = yearly_outlays$total_outlays,
                           "disc_spending" = yearly_disc_spend$discretionary_outlays)

spending_melt <- melt(all_spending, id=c("year"))

ggplot(spending_melt) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red","green", "blue"),
                     labels = c("GDP", "Total Spending", "Discretionary Spending")) + 
  ggtitle("GDP versus Spending") +
  xlab("Year") +
  ylab("Dollars (USD)")

# A1 - Method 1: gdp roc with total_spend and disc_spend [R2]

## rate of change for yearly gdp
gdp_change <- 100*diff(yearly_gdp$gdp)/yearly_gdp[-nrow(yearly_gdp),]$gdp
total_outlay_change <- 100*diff(yearly_outlays$total_outlays)/yearly_outlays[-nrow(yearly_outlays),]$total_outlays
disc_outlay_change <- 100*diff(yearly_disc_spend$discretionary_outlays)/yearly_disc_spend[-nrow(yearly_disc_spend),]$discretionary_outlays

gdp_budget_roc <- data.frame("year" = yearly_gdp$year[2:42], 
                             "gdp_change" = gdp_change,
                             "outlay_change" = total_outlay_change,
                             "disc_change" = disc_outlay_change)

p <- ggplot() +
  geom_line(data = gdp_budget_roc, aes(x = year, y = gdp_change), color = "blue") +
  geom_line(data = gdp_budget_roc, aes(x = year, y = outlay_change), color = "red") + 
  geom_line(data = gdp_budget_roc, aes(x = year, y = disc_change), color = "green") +
  xlab("Year") + 
  ylab("Rate of change (%)")

print(p)

# Fit a linear model
roc_model <- lm(cbind(gdp_change, disc_outlay_change, outlay_change) ~ year, data = gdp_budget_roc)

# Stats: R squared values and p-values
gdp_change_summary <- summary(roc_model)["Response gdp_change"]
gdp_change_r2 <- gdp_change_summary$`Response gdp_change`$r.squared
gdp_change_pv <- gdp_change_summary$`Response gdp_change`$coefficients[,4]

outlay_change_summary <- summary(roc_model)["Response outlay_change"]
outlay_change_r2 <- outlay_change_summary$`Response outlay_change`$r.squared
outlay_change_pv <- outlay_change_summary$`Response outlay_change`$coefficients[,4]

disc_change_summary <- summary(roc_model)["Response disc_outlay_change"]
disc_change_r2 <- disc_change_summary$`Response disc_outlay_change`$r.squared
disc_change_pv <- disc_change_summary$`Response disc_outlay_change`$coefficients[,4]

# A1 - Method 2: Melt data and then plot
roc_melt <- melt(gdp_budget_roc, id=c("year"))

ggplot(roc_melt) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red","green", "blue"),
                     labels = c("GDP", "Total Spending", "Discretionary Spending"))


# A2. gdp roc vs per group roc (fed, energy, climate) [R2]

# get mean roc per group
fed_mean_spend <- fed_spend %>%
  group_by(year) %>%
  summarize(average_spend = mean(rd_budget))

energy_mean_spend <- energy_spend %>%
  group_by(year) %>%
  summarize(average_spend = mean(energy_spending))

climate_mean_spend <- climate_spend %>%
  group_by(year) %>%
  summarize(average_spend = mean(gcc_spending))


fed_mean_change <- 100*diff(fed_mean_spend$average_spend)/fed_mean_spend[-nrow(fed_mean_spend),]$average_spend
energy_mean_change <- 100*diff(energy_mean_spend$average_spend)/energy_mean_spend[-nrow(energy_mean_spend),]$average_spend
climate_mean_change <- 100*diff(climate_mean_spend$average_spend)/climate_mean_spend[-nrow(climate_mean_spend),]$average_spend


fed_roc <- data.frame("year" = fed_mean_spend$year[2:42],
                      "fed_change" = fed_mean_change,
                      "gdp_change" = gdp_change)

energy_roc <- data.frame("year" = energy_mean_spend$year[2:22],
                         "energy_change" = energy_mean_change,
                         "gdp_change" = gdp_change[22:42])

energy_roc <- energy_roc[-nrow(energy_roc),] # Remove last row b/c there is no GDP data for 2018

climate_roc <- data.frame("year" = climate_mean_spend$year[2:18],
                          "climate_change" = climate_mean_change,
                          "gdp_change" = gdp_change[25:41])


# make plot for each roc versus GDP
fed_melt <- melt(fed_roc, id=c("year"))
ggplot(fed_melt) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

energy_melt <- melt(energy_roc, id=c("year"))
ggplot(energy_melt) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

climate_melt <- melt(climate_roc, id=c("year"))
ggplot(climate_melt) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))


# A3. gdp roc versus every department [R2]


" 
function: department_matrix_roc
description: Given a data frame and a column, gets the 
             rate of change per department in said column
returns: matrix where each col is department, and each row
         is percent change from previous year
"
department_matrix_roc <- function(df, monies_col) {
  departments <- unique(df$department)
  dep_one_yearly <- filter(df, department == as.character(departments[1])) # all unique years
  df_years <- nrow(dep_one_yearly) # range of years in DF
  matrix_roc <- data.frame("year" = dep_one_yearly$year[2:df_years])
  
  for (dep in departments) {
    # Get yearly spending for department
    curr_dep <- df %>%
      filter(department == as.character(dep)) %>%
      select(year, monies_col)
    
    curr_dep_roc <- 100*diff(curr_dep[[monies_col]])/curr_dep[-nrow(curr_dep),][[monies_col]]
    matrix_roc[dep] <- curr_dep_roc
  }
  matrix_roc
}

fed_department_roc <- department_matrix_roc(fed_spend, "rd_budget")
fed_department_roc$gdp_change <- gdp_budget_roc$gdp_change

energy_department_roc <- department_matrix_roc(energy_spend, "energy_spending")
energy_department_roc <- energy_department_roc[-nrow(energy_department_roc),] # remove last row, no GDP for 2018
energy_department_roc$gdp_change <- gdp_budget_roc$gdp_change[22:41]

climate_department_roc <- department_matrix_roc(climate_spend, "gcc_spending")
climate_department_roc$gdp_change <- gdp_budget_roc$gdp_change[25:41]

# For each department make plot with all rates of change versus GDP
fed_roc_melt <- melt(fed_department_roc, id=c("year"))
ggplot(fed_roc_melt) +
  geom_line(aes(x = year, y = value, color=variable))


energy_roc_melt <- melt(energy_department_roc, id=c("year"))
ggplot(energy_roc_melt) +
  geom_line(aes(x = year, y = value, color=variable))


climate_roc_melt <- melt(climate_department_roc, id=c("year"))
ggplot(climate_roc_melt) +
  geom_line(aes(x = year, y = value, color=variable))

# B2. Total spending rate of change versus rate of change per group average spending
fed_roc_total_spend <- data.frame("year" = fed_mean_spend$year[2:42],
                      "fed_change" = fed_mean_change,
                      "total_spending_change" = total_outlay_change)

energy_roc_total_spend <- data.frame("year" = energy_mean_spend$year[2:22],
                         "energy_change" = energy_mean_change,
                         "total_spending_change" = total_outlay_change[22:42])
energy_roc_total_spend <- energy_roc_total_spend[-nrow(energy_roc_total_spend),] # Remove last row b/c there is no GDP data for 2018

climate_roc_total_spend <- data.frame("year" = climate_mean_spend$year[2:18],
                          "climate_change" = climate_mean_change,
                          "total_spending_change" = total_outlay_change[25:41])

fed_melt_total_spend <- melt(fed_roc_total_spend, id=c("year"))
ggplot(fed_melt_total_spend) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

energy_melt_total_spend <- melt(energy_roc_total_spend, id=c("year"))
ggplot(energy_melt_total_spend) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

climate_melt_total_spend <- melt(climate_roc_total_spend, id=c("year"))
ggplot(climate_melt_total_spend) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

# B3. Rate of change per department per group vs total spending
fed_department_roc_total_spend <- fed_department_roc[,!(names(fed_department_roc) %in% c("gdp_change"))]
fed_department_roc_total_spend$spending_change <- gdp_budget_roc$outlay_change

energy_department_roc_total_spend <- energy_department_roc[,!(names(energy_department_roc) %in% c("gdp_change"))]
energy_department_roc_total_spend$spending_change <- gdp_budget_roc$outlay_change[22:41]

climate_department_roc_total_spend <- climate_department_roc[,!(names(climate_department_roc) %in% c("gdp_change"))]
climate_department_roc_total_spend$spending_change <- gdp_budget_roc$outlay_change[25:41]

fed_roc_total_spend_melt <- melt(fed_department_roc_total_spend, id=c("year"))
ggplot(fed_roc_total_spend_melt) +
  geom_line(aes(x = year, y = value, color=variable))


energy_roc_total_spend_melt <- melt(energy_department_roc_total_spend, id=c("year"))
ggplot(energy_roc_total_spend_melt) +
  geom_line(aes(x = year, y = value, color=variable))


climate_roc_total_spend_melt <- melt(climate_department_roc_total_spend, id=c("year"))
ggplot(climate_roc_total_spend_melt) +
  geom_line(aes(x = year, y = value, color=variable))

# C2. Discretionary spending rate of change versus rate of change per group average spending
fed_roc_disc_spend <- data.frame("year" = fed_mean_spend$year[2:42],
                                  "fed_change" = fed_mean_change,
                                  "disc_spending_change" = disc_outlay_change)

energy_roc_disc_spend <- data.frame("year" = energy_mean_spend$year[2:22],
                                     "energy_change" = energy_mean_change,
                                     "disc_spending_change" = disc_outlay_change[22:42])
energy_roc_disc_spend <- energy_roc_disc_spend[-nrow(energy_roc_disc_spend),] # Remove last row b/c there is no GDP data for 2018

climate_roc_disc_spend <- data.frame("year" = climate_mean_spend$year[2:18],
                                      "climate_change" = climate_mean_change,
                                      "disc_spending_change" = disc_outlay_change[25:41])

fed_melt_disc_spend <- melt(fed_roc_disc_spend, id=c("year"))
ggplot(fed_melt_disc_spend) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

energy_melt_disc_spend <- melt(energy_roc_disc_spend, id=c("year"))
ggplot(energy_melt_disc_spend) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))

climate_melt_disc_spend <- melt(climate_roc_disc_spend, id=c("year"))
ggplot(climate_melt_disc_spend) +
  geom_line(aes(x = year, y = value, color=variable)) +
  scale_color_manual(values = c("red", "blue"))
# C3. Rate of change per department per group vs discretionary spending
fed_department_roc_disc_spend <- fed_department_roc[,!(names(fed_department_roc) %in% c("gdp_change"))]
fed_department_roc_disc_spend$spending_change <- gdp_budget_roc$disc_change

energy_department_roc_disc_spend <- energy_department_roc[,!(names(energy_department_roc) %in% c("gdp_change"))]
energy_department_roc_disc_spend$spending_change <- gdp_budget_roc$disc_change[22:41]

climate_department_roc_disc_spend <- climate_department_roc[,!(names(climate_department_roc) %in% c("gdp_change"))]
climate_department_roc_disc_spend$spending_change <- gdp_budget_roc$disc_change[25:41]

fed_roc_disc_spend_melt <- melt(fed_department_roc_disc_spend, id=c("year"))
ggplot(fed_roc_disc_spend_melt) +
  geom_line(aes(x = year, y = value, color=variable))


energy_roc_disc_spend_melt <- melt(energy_department_roc_disc_spend, id=c("year"))
ggplot(energy_roc_disc_spend_melt) +
  geom_line(aes(x = year, y = value, color=variable))


climate_roc_disc_spend_melt <- melt(climate_department_roc_disc_spend, id=c("year"))
ggplot(climate_roc_disc_spend_melt) +
  geom_line(aes(x = year, y = value, color=variable))



# Positive/negative chart
# if ROC positive = 1
# if ROC negative = -1
# make new chart with these values for ROC and plot against all departments







  