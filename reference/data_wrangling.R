# R Data Viz

install.packages("tidyverse")

download.file(url="https://ndownloader.figshare.com/files/2292169", destfile = "data/portal_data_joined.csv")

surveys <- read.csv("data/portal_data_joined.csv")


library("tidyverse")

# head(surveys)
# View(surveys)

# mean weight for each sex for each species
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# Heaviest animal each year
surveys %>%
  filter(!is.na(weight)) %>%
  group_by(year) %>%
  filter(weight == max(weight)) %>%
  select(year, genus, species, weight) %>%
  arrange(year)

# summarize mean weight for each genus for each plot_id
surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))

# create new table using spread
surveys_spread <- surveys_gw %>%
  spread(key = genus, value = mean_weight, fill = 0)

str(surveys_spread)

surveys_gather <- surveys_spread %>%
  gather(key = genus, value = mean_weight, -plot_id)

str(surveys_gather)

# gather using multiple columns
surveys_spread %>%
  gather(key = genus, value = mean_weight, Baiomys:Spermophilus) %>%
  head()

# spread
rich_time <- surveys %>%
  group_by(plot_id, year) %>%
  summarize(n_genera = n_distinct(genus)) %>%
  spread(year, n_genera)

head(rich_time)

surveys_long <- surveys %>%
  gather(measurement, value, hindfoot_length, weight)
