# tidytuesday, 2020-12-22
# Big Mac Index
# Kaizad F. Patel
# 12-31-2020

############ # 

# Plots inspired by:
# https://github.com/botan/tidytuesday/blob/main/R/2020-w52-bigmac.Rmd
# https://github.com/AndyABaker/TidyTuesday
# https://github.com/kaustavSen/tidytuesday/blob/master/2020/week_52_big_mac.Rmd

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


# part 1: turkey  ---------------------------------------------------------


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





# part 2: 2011 vs 2020 ----------------------------------------------------------------
library(lubridate)

bigmac_gdp_dollar = 
  big_mac_processed = 
  big_mac %>% 
  filter(!is.na(adj_price)) %>% 
  dplyr::select(name, date, adj_price, dollar_price, usd_raw, usd_adjusted)  %>% 
  mutate(year = year(date)) %>% 
  filter(year %in% c(2011, 2020)) %>% 
  filter(date != "2020-01-14")


bigmac_gdp_dollar %>% 
  ggplot(aes(x = year, y = adj_price, group = name))+
  geom_point(color = "grey") + geom_path(color = "grey")+
  theme_bw()+
  geom_point(data = bigmac_gdp_dollar %>% filter(name == "United States"))+
  labs(title = "how much did/does a Big Mac cost?")
  

bigmac_gdp_dollar_drop = 
  bigmac_gdp_dollar %>% 
  dplyr::select(name, year, usd_raw) %>% 
  pivot_wider(names_from = year, values_from = usd_raw) %>% 
  mutate(drop_color = (`2011` > 0 & `2020` < 0),
         diff = `2020`-`2011`) %>% 
  #filter(drop_color) %>% 
  top_n(-6, diff)


bigmac_gdp_dollar2 = 
  bigmac_gdp_dollar %>% 
  left_join(bigmac_gdp_dollar_drop) 

bigmac_gdp_dollar2 %>% 
  ggplot(aes(x = as.character(year), y = usd_raw, group = name, color = name))+
  # geom_hline(yintercept = 0, color = "lightblue")+
  annotate("segment", y = 0, yend = 0, x = 0.8, xend = 2.2, linetype = "longdash")+
  
  geom_point(color = "grey", size = 2) + geom_path(color = "grey", alpha = 0.5, size = 1)+
  geom_point(data = bigmac_gdp_dollar2 %>% filter(!is.na(diff)), size = 2)+
  geom_path(data = bigmac_gdp_dollar2 %>% filter(!is.na(diff)), size = 1, show.legend = F)+
  geom_text(data = bigmac_gdp_dollar2 %>% filter(!is.na(diff) & year == 2011), 
            aes(label = name),
            hjust = 1, size = 2.8, position = position_nudge(x = -0.05))+
  annotate("segment", x = 0.65, xend = 0.65, y = 0, yend = 1, 
           size = 1.5, color = "dodgerblue4", arrow = arrow(length = unit(2.5, "mm")))+
  annotate("segment", x = 0.55, xend = 0.55, y = 0, yend = -0.65, 
           size = 1.5, color = "coral3", arrow = arrow(length = unit(2.5, "mm")))+
  geom_point(aes(x = 0.65, y = 0), color = "dodgerblue4", size = 2)+
  geom_point(aes(x = 0.55, y = 0), color = "coral3", size = 2)+
  

  annotate("text", label = "positive (overvalued)",
           x = 0.60, y = 0.1, angle = 90, size = 3, hjust = 0, color = "dodgerblue4")+
  annotate("text", label = "negative (underrvalued)",
           x = 0.60, y = -0.05, angle = -90, size = 3, hjust = 0, color = "coral3")+
  
  annotate("text", label = "The Big Mac Index", 
           x = 1.2, y = 1.2, size = 5, hjust = 0, family = "Georgia", fontface = "bold", color = "grey20")+
  annotate("text", label = "How have currencies compared to the US Dollar?", 
           x = 1.2, y = 1.1, size = 3, hjust = 0, family = "sans", color = "grey40")+
  
  
  scale_x_discrete(expand = expand_scale(mult = c(0.5, 0.2)))+

  scale_color_brewer(palette = "Dark2")+
  labs(x = "", y = "",
       caption = "github.com/kaizadp | Source: The Economist
       1 January, 2021")+
  ylim(-0.7, 1.2)+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #plot.title = element_text(family = "sans", size = 18, margin=margin(30,0,30,30)),
        plot.caption = element_text(hjust = 0.6, size = 6, color = "grey40"),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 8, margin = margin(t = -0.4, b = 0, unit = "cm"))
  )+
  NULL

  ggsave("images/2020-12-22_bigmac/index.tiff")
  
    #









#  
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
