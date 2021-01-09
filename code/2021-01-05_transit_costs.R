library(tidyverse)
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')


# top 10 longest ----------------------------------------------------------

transit_cost_longest = 
  transit_cost %>% 
  drop_na(country) %>% 
  top_n(10, length) %>% 
  dplyr::select(country, city, line, length)

sysfonts::font_add_google("Share", "Share")
showtext::showtext_auto()

transit_cost_longest %>% 
  mutate(length = as.integer(length),
         line2 = paste0(line, " (", city, ")")) %>% 
  ggplot(aes(x = reorder(line2, -length), y = length, color = country))+
  
  #geom_point(size = 10, alpha = 0.3)+
  geom_segment(aes(xend = reorder(line2, -length), yend = 0), size = 10, alpha = 0.3, lineend = "round")+
  geom_text(aes(label = line2), 
            hjust = 1, angle = 90, size = 3, position = position_nudge(y = -5), color = "grey20")+
  geom_text(aes(label = length), position = position_nudge(y = 15), fontface = "bold")+
  annotate("text", label = "km", x = 1, y = 210, size = 3, color = "grey50", fontface = "italic")+
  #scale_color_manual(values = c("blue", "green", "yellow", "orange", "red", "pink", "purple"))+
  scale_color_viridis_d()+
  labs(x = "",
       y = "",
       #title = "The 10 longest transit lines",
       #subtitle = "length in km",
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


