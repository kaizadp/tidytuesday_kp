# tidytuesday, 2020-12-22
# Big Mac Index
# Kaizad F. Patel
# 12-31-2020

############ # 

# Plots inspired by:
# https://github.com/botan/tidytuesday/blob/main/R/2020-w52-bigmac.Rmd
# https://github.com/AndyABaker/TidyTuesday


## new items:
## - use geom_flame to mimic geom_area across alternating + and - data
## - use x-axis margins to adjust the position of the x axis 
## - struggle with fonts

############ # 


library(tidyverse)
library(heatwaveR)
library(ggtext)
library(showtext)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')


  ## library(extrafont)
  ## extrafont::font_import("Merriweather")
  ## font_add_google("Merriweather")
  ## font_add_google("Old Standard TT")

# data exploration
names(big_mac)
str(big_mac)
skimr::skim(big_mac)


# data processing 

## process the data file, using usd_adjusted as reference
## create a new variable for over/under
## create a new variable for the text labels, which includes only the max/min values

big_mac_processed = 
  big_mac %>% 
  filter(!is.na(usd_adjusted)) %>% 
  filter(name == "Turkey") %>% 
  mutate(valued = if_else(usd_adjusted > 0, "over", "under")) %>% 
  mutate(label = case_when(usd_adjusted == max(usd_adjusted) ~ usd_adjusted,
                           usd_adjusted == min(usd_adjusted) ~ usd_adjusted,
                           TRUE ~ NA_real_)) %>% 
  mutate(label = case_when(usd_adjusted == max(usd_adjusted) ~ usd_adjusted,
                           usd_adjusted == min(usd_adjusted) ~ usd_adjusted)) 


# data visualization

big_mac_processed %>% 
  ggplot(aes(x = date, y = usd_adjusted, label = scales::percent(label))) +
  
  geom_flame(aes(y2 = usd_adjusted, y = 0), fill = "darkred") +
  geom_flame(aes(y = usd_adjusted, y2 = 0), fill = "#ffffff")+
  
  geom_point()+ geom_path()+
  
  geom_text(data = big_mac_processed %>% filter(label > 0),
            fontface = "bold", position = position_nudge(y = 0.06))+
  geom_text(data = big_mac_processed %>% filter(label < 0),
            fontface = "bold", position = position_nudge(y = -0.06))+
  
  labs(title = "The Big Mac Index: Turkey",
       subtitle = "The big mac index was invented by The Economist in 1986
       as a lighthearted guide to whether currencies are at their “correct” level.
       
       This graph shows how the Turkish Lira has compared to the US Dollar.",
       
       caption = "github.com/kaizadp | Source: The Economist
       31 December, 2020",
       y = "GDP adjusted index, relative to the US dollar") + 
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "gray60"),
    plot.title = element_text(family = "Georgia", face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(family = "Georgia", face = "italic", size = 10, hjust = 0.5),
    plot.caption = element_text(family = "Georgia", size = 10, hjust = 0.5),
    #axis.text.x.bottom = element_text(family = "sans"),
    plot.margin = margin(t = 2, r = 1, b = 2, l = 1, unit = "cm"),
    axis.text.x = element_text(face = "bold", margin = margin(t = -5.25, b = 5.25, unit = "cm"))
  ) 

ggsave("images/2020-12-22_bigmac/turkey.tiff", height = 7.7, width = 7.5)




# misc --------------------------------------------------------------------
# plot from # https://github.com/AndyABaker/TidyTuesday

big_mac %>% filter(name != "United States", !is.na(usd_adjusted)) %>% 
  ggplot(aes(date, usd_adjusted)) + 
  geom_flame(aes(x = date, y2 = usd_adjusted, y = 0), fill = "#e63946") +
  geom_flame(aes(x = date, y = usd_adjusted, y2 = 0), fill = "#52b788") + 
  scale_y_continuous(labels = scales::percent) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(vars(name), ncol = 6) + 
  theme_minimal() +
  labs(title = "The Big Mac Index",
       
       caption = "Source: The Economist | Visualisation: @Andy_A_Baker",
       y = "GDP adjusted index, relative to the US dollar") + 
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(family = "ITC Officina Sans"),
        plot.title = element_text(face = "bold"),
        axis.text = element_text(face = "plain"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        strip.text.x = element_text(face = "bold"), 
        #plot.subtitle = element_textbox_simple(lineheight = 1.1)
  )
