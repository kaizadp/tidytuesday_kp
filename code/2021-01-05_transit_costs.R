## TIDYTUESDAY 2021-01-08: TRANSIT COSTS PROJECT

## Kaizad F. Patel
## Jan-08-2021

########### #
########### #

# new skills in this assignment:
# - circular bar/segment plots (coord_polar)
# - circular plots with empty space (race track plots)
# - {countrycode} package

########### #
########### #

library(tidyverse)
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# top 10 longest ----------------------------------------------------------
# segment plot showing top 10 longest transit lines

# process the data to select top 10 length
transit_cost_longest = 
  transit_cost %>% 
  drop_na(country) %>% 
  top_n(10, length) %>% 
  dplyr::select(country, city, line, length)

# load fonts
sysfonts::font_add_google("Share", "Share")
showtext::showtext_auto()

transit_cost_longest %>% 
  mutate(length = as.integer(length),
         line2 = paste0(line, " (", city, ")")) %>% 
  ggplot(aes(x = reorder(line2, -length), y = length, color = country))+
  geom_segment(aes(xend = reorder(line2, -length), yend = 0), size = 10, alpha = 0.3, lineend = "round")+
  geom_text(aes(label = line2), 
            hjust = 1, angle = 90, size = 3, position = position_nudge(y = -5), color = "grey20")+
  geom_text(aes(label = length), position = position_nudge(y = 15), fontface = "bold")+
  annotate("text", label = "km", x = 1, y = 210, size = 3, color = "grey50", fontface = "italic")+
  scale_color_viridis_d()+
  labs(x = "",
       y = "",
       caption = "github.com/kaizadp | source = Transit Costs Project
       8 January 2021")+
  annotate("text", label = "The 10 longest transit lines 
           in the world",
           x = 10, y = 200, hjust = 1, fontface = "bold", size = 7, family = "Share")+
  theme_void()+
  theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 1),
    plot.caption = element_text(hjust = 0.5, color = "grey40", size = 8),
    legend.position = "none")

ggsave("images/2021-01-05_transit_costs/transit_longest.png", width = 5.5, height = 5.5)  

#
# top 10 longest -- circular plot ------------------------------------------------------------------
# inspired by https://twitter.com/LauraNavarroSol/status/1347575288847233024?s=20

# install.packages("countrycode")
library(countrycode)
library(patchwork)

transit_radial_top10 = 
  transit_cost %>% 
  drop_na(country) %>% 
  top_n(10, length) %>% 
  mutate(length = as.integer(length),
         line2 = paste0(line, " (", city, ")")) %>% 
  # convert country codes to country names
  mutate(countryname = countrycode(country, "ecb", "country.name"))

# if doing a race track plot, add some rows with 0 value
 # add_row(line2 = "a", length = 0, cost_km_millions = 0) %>% 
 # add_row(line2 = "b", length = 0, cost_km_millions = 0) %>% 
 # add_row(line2 = "c", length = 0, cost_km_millions = 0) %>% 
  
  
sysfonts::font_add_google("Bebas Neue", "Bebas")
sysfonts::font_add_google("Roboto", "Roboto")
showtext::showtext_auto()

# create a base plot first, to set up the circular plot
# then create the plots 

radial_base = 
  transit_radial_top10 %>% 
  ggplot(aes(x = reorder(line2, length), y = length, 
             xend = reorder(line2, -length), yend = 0, color = country))+
  scale_color_brewer(palette = "Dark2", type = "seq")+
  ylim(0, 250)+
  theme_void()+
  theme(legend.position = "none")+
  coord_polar(theta = "y", direction = -1)


(radial_all = 
    radial_base +
    geom_segment(size = 3, lineend = "round")+
    annotate("text", label = "Longest transit lines", 
             x = 9, y = 245, hjust = 0, size = 9, fontface = "bold", color = "grey20", family = "Bebas")+
    annotate("text", label = "Source: Transit Costs Project", 
             x = 8, y = 240, hjust = 0, size = 4, color = "grey40", family = "Roboto")+
    annotate("text", label = "github.com/kaizadp \n 8 January 2021", 
             x = 11.5, y = 125, hjust = 0.5, size = 3, color = "grey50", family = "Roboto")+
    NULL
)

# now facet by country
(radial_facet = 
    radial_base +
    geom_segment(size = 1, lineend = "round")+
    facet_wrap(~countryname, ncol = 3)+
    theme(plot.caption = element_text(family = "Roboto", color = "grey50"))+
    NULL
)

# now combine both plots
radial_all + radial_facet +
  plot_layout(widths = c(3, 1))

ggsave("images/2021-01-05_transit_costs/transit_longest_radial.png", width = 9, height = 6)  
