## 2020 SEARCH TRENDS
## KAIZAD F. PATEL
## 10 JAN 2021

# top search trends for 2020
# inspired by: https://observablehq.com/@ben-tanen/the-relentless-2020-news-cycle-in-one-chart
# data obtained from: https://trends.google.com/trends/yis/2020/US/


# -------------------------------------------------------------------------

# install.packages("gtrendsR")
# install.packages("ggridges")
library(gtrendsR)
library(tidyverse)
library(ggridges)
library(showtext)

# -------------------------------------------------------------------------

filePaths <- list.files(path = "code/trends2020/",pattern = "*.csv", full.names = TRUE)

# rbind.fill binds all rows and fills in missing columns
trends <- do.call(plyr::rbind.fill, lapply(filePaths, function(path) {
  df <- read.csv(path, header=FALSE, skip = 2)
  df}))

trends = sapply(filePaths,
                  read.csv, skip = 2, simplify = FALSE) %>% bind_rows()

trends_long = 
  trends %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Week) %>% 
  drop_na() %>% 
  mutate(Week = case_when(grepl("/20", Week) ~ lubridate::mdy(Week),
                           grepl("2020-", Week) ~ lubridate::ymd(Week)),
         value = if_else(value == "<1", "0", value),
         value = as.integer(value),
         name = str_replace_all(name, "...United.States.", ""),
         name = str_replace_all(name, "[.]", " ")) %>% 
  distinct() 

trends_order = 
  trends_long %>%
  group_by(name) %>% 
  filter(value >= 50) %>% 
  dplyr::summarise(order = min(Week))

trends_long2 = 
  trends_long %>% 
  left_join(trends_order) %>% 
  group_by(name) %>% 
  arrange(Week) %>% 
  #mutate(value_avg = zoo::rollmean(x=value, 4))
  mutate(value_avg = slider::slide_dbl(value, mean, .before = 2, .after = 0))

trends_long2$name2 = reorder(trends_long2$name, trends_long2$order)


###

sysfonts::font_add_google("Playfair Display", "Playfair")
sysfonts::font_add_google("Roboto", "Roboto")
sysfonts::font_add_google("Andika", "Andika")
showtext_auto()

trends_long2 %>% 
  #  filter(name == "WAP") %>% 
  ggplot(aes(x = Week, y = value))+
  #  geom_line()+
  geom_area(aes(group = name2, fill = name2), color = "grey40", alpha = 0.5)+
  #  geom_ribbon(aes(ymin = 0, ymax = value_avg, fill = name2), color = "black", alpha = 0.4)+
  #  geom_density_ridges()+
  scale_fill_manual(values = rev(PNWColors::pnw_palette("Sunset2", 33)))+
  labs(title = "2020 Search Trends",
       caption = "
       github.com/kaizadp | source: Google Trends
       10 January 2021")+
  facet_grid(name2~.)+
  theme_void()+
  scale_x_date(labels = scales::date_format("%b"), breaks = "3 months")+
  theme(legend.position = "none",
        panel.spacing.y = unit(-0.8, "lines"),
        axis.text.x = element_text(color = "grey30", size = 8),
        plot.title = element_text(hjust = 0.5, color = "grey20", size = 14, 
                                  family = "Playfair", face = "bold"),
        plot.caption = element_text(hjust = 0.5, color = "grey50", size = 8, 
                                    family = "Andika"),
        panel.grid.major.x = element_line(color = "grey90", size = 0.15),
        strip.text.y = element_text(hjust = 0, vjust = 1, family = "Andika",
                                    margin = margin(t = 25, b = 15)))


ggsave("images/trends2020/trends2020.png", height = 9, width = 4)
