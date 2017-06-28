#### Header Information ####
# This script file is intended to develop some working code based on the CCOD
# database to visualize country contributions to research. It will rely on the
# latitude and longitude coordinates available through Google's DSPL.

# Author: Adam Raikes
# Initial Date: 06/13/2017

#### Necessary libraries ####
library(maps)
library(mapdata)

#### Summarise article density by country ####
ccod.countries <- ccod.included %>%
  group_by(pop_country) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#### Load world map ####
world.map <- map_data("world")

#### Add article counts to world.map ####
ccod.countries <- ccod.countries %>%
  rename(region = pop_country)

world.map <- world.map %>%
  left_join(ccod.countries)

#### Plot world map ####
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ggplot() + 
  geom_polygon(data = world.map, aes(x=long, 
                                     y = lat, 
                                     group = group,
                                     fill = count)) +
  coord_fixed(1.3) +
  geom_point() + 
  scale_fill_gradient(name = "Number of\npublications",
                      trans = "log10") +
  ditch_the_axes
