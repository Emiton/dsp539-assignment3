# surveys <- read.csv("data/portal_data_joined.csv")

library("tidyverse")

install.packages("hexbin")
library(hexbin)

surveys_complete <- surveys %>%
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))

surveys_plot <- ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length))

# bind ggplot to DF
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) + 
    geom_point(alpha = 0.1, color = "blue") # alpha as 0.1 adds transparency

# color code factors
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, aes(color = species_id))

# color code factors v2
ggplot(data = surveys_complete, mapping = aes(x = weight, y = hindfoot_length, color = species_id)) +
  geom_point(alpha = 0.1)

# boxplot with points 
ggplot(data = surveys_complete, mapping = aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

# hex scatterplot with density
surveys_plot +
  geom_hex()

# get yearly counts
yearly_counts <- surveys_complete %>%
  count(year, species_id)

# print one var by using 'n' for y val
# group in order to separate species 
ggplot(data = yearly_counts, mapping = aes(x = year, y = n, group = species_id)) +
  geom_line()

# use color for better visual
ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = species_id)) +
  geom_line()

# faceting - split one plot into multiple plots based on a factor
ggplot(data = yearly_counts, mapping = aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~ species_id)


yearly_sex_counts <- surveys_complete %>%
  count(year, species_id, sex)

# split based on sex
ggplot(data = yearly_sex_counts, mapping = aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() + # used to set the background
  theme(panel.grid = element_blank()) # make background blank


# used for labeling
labs(title = "Observed species in time",
     x = "Year of observation",
     y = "Number of individuals") +
    theme_bw() +
    theme(text=element_text(size = 16))

# might have to use extra fonts package for font manipulation
# can save as a theme and just add as a layer
grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
      axis.text.y = element_text(colour = "grey20", size = 12),
      text = element_text(size = 16))


# for placing multiple plots on one page
install.packages("gridExtra")
library(gridExtra)

spp_weight_boxplot <- ggplot(data = surveys_complete, 
                             mapping = aes(x = species_id, y = weight)) +
  geom_boxplot() +
  xlab("Species") + ylab("Weight (g)") +
  scale_y_log10()

spp_count_plot <- ggplot(data = yearly_counts, 
                         mapping = aes(x = year, y = n, color = species_id)) +
  geom_line() + 
  xlab("Year") + ylab("Abundance")

grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))

# use to export plots as a high quality
ggsave("fig_output/yearly_sex_counts.png", my_plot, width = 15, height = 10)








