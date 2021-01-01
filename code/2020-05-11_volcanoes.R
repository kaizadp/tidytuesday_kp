# map of eruptions
# size = number of eruptions
# color = average magnitude?


# 1. load datasets ----
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


# load packages
library(tidyverse)
library(ggplot2)
library(soilpalettes)
library(ggmap)
library(ggthemes)

# set ggplot theme
bgcol <- '#f0f0f0'
textcol <- "grey95"
titlefont <- "Montserrat Medium"
subfont <- "Montserrat"
subfont2 <- "Montserrat Light"

theme_set(theme_void())
theme_update(plot.background = element_rect(color=bgcol,fill=bgcol),
             panel.background = element_blank(),
             #axis.text = element_text(size=12, color = "black"),
             axis.title = element_text(size = 12, color = "black", face = "bold")
)
             
             


# 2. eruptions over the years ----
names(eruptions)

(eruptions_yearly = 
  eruptions %>% 
  group_by(year = start_year) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup %>% 
    na.omit() %>% 
  ggplot(aes(x = year, y = n))+
  geom_point()+
  xlim(0,2020)+
    labs(title = "eruptions per year")+
    NULL
)

## eruptions over the decades

(eruptions_decade = 
    eruptions %>% 
    group_by(decade = round(start_year/10)*10) %>% 
    dplyr::summarise(n = n(),
                     avg_vei = round(mean(vei, na.rm = T),1)) %>% 
    ungroup %>% 
    na.omit() %>% 
    ggplot(aes(x = decade, y = n, size=avg_vei, color = avg_vei))+
    geom_point()+
    xlim(1500,2020)+
    scale_color_gradientn(colors = soil_palette("redox"))+
    labs(title = "eruptions per decade")+
    NULL
)



# 3. most erupted volcanoes ----
(eruptions_volcanoes = 
  eruptions %>% 
  group_by(volcano_name) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup %>% 
  na.omit() %>%
  top_n(20, n) %>% 
  ggplot(aes(x = reorder(volcano_name, n), y = n))+
  geom_point()+
  theme(axis.text.x = element_text(angle=90, hjust=1))   +
   labs(title = "20 most erupted volcanoes")+
   NULL
)  

# 4. countries and volcanoes ----
# which countries have the most volcanoes

(countries_num = 
   volcano %>% 
   group_by(country, volcano_name) %>% 
   dplyr::summarise(n = n()) %>% 
   group_by(country) %>% 
   dplyr::summarise(n = n()) %>% 
   arrange(desc(n)) %>% 
   top_n(20, n) %>% 
   ggplot(aes(y = reorder(country,n), x = n))+
   geom_point()+
   labs(title = "most volcanoes")+
   NULL
)


# which countries have most eruptions
(countries_eruptions = 
  eruptions %>% 
  dplyr::select(volcano_name, start_year) %>% 
  left_join(dplyr::select(volcano, volcano_name, country), by = "volcano_name") %>% 
  group_by(country) %>% 
  dplyr::summarise(n = n()) %>% 
  na.omit() %>% 
  arrange(desc(n)) %>% 
  top_n(20, n) %>% 
    ggplot(aes(x = n, y = reorder(country, n)))+
    geom_point()+
    labs(title = "most eruptions of all time")+
    NULL
)

(countries_eruptions_1500 = 
    eruptions %>% 
    dplyr::select(volcano_name, start_year) %>% 
    filter(start_year>1500) %>% 
    left_join(dplyr::select(volcano, volcano_name, country), by = "volcano_name") %>% 
    group_by(country) %>% 
    dplyr::summarise(n = n()) %>% 
    na.omit() %>% 
    arrange(desc(n)) %>% 
    top_n(20, n) %>% 
    ggplot(aes(x = n, y = reorder(country, n)))+
    geom_point()+
    labs(title = "most eruptions after 1500")+
    NULL
)








  
# 5. events over the years ----

(events_yearly = 
   events %>% 
   filter(event_date_year>0) %>% 
   group_by(event_date_year, event_type) %>% 
   dplyr::summarise(n = n()) %>% 
   na.omit() %>%
   ggplot(aes(x = event_date_year, y = n, color = event_type))+
   geom_point()+
   NULL
)

# 6. maps ----

# ---- -----
eruptions %>% 
  group_by(volcano_name, longitude, latitude) %>% 
  dplyr::summarise(n = n(),
                   vei = mean(vei)) %>% 
  na.omit() %>% 
  ggplot(aes(longitude, latitude,  color=n, size=vei))+
  borders()+
  geom_point()+
  scale_color_viridis_c(option = "magma", direction = 1, trans = "log10") +
  scale_size_area(max_size = 8)+
  theme_void()+
  theme(legend.position = "bottom")
  
  
eruptions2 = 
  eruptions %>% 
  filter(eruption_category=="Confirmed Eruption") %>% 
  count(volcano_number, latitude, longitude) %>% 
  left_join(volcano %>% dplyr::select(volcano_number, volcano_name)) %>% 
  dplyr::mutate(volcano_number = factor(volcano_number),
                volcano_number = fct_reorder(volcano_number, n))

label = 
  eruptions2 %>% 
  arrange(volcano_number) %>% 
  arrange(desc(n)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank %in% 1:5) 

# rnaturalearth ----
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

world = ne_countries(scale = "medium", returnclass = "sf", type = "countries")

(world_map = 
  world %>% 
  ggplot()+
 geom_sf(fill="black", color=NA)+
  geom_point(aes(longitude, latitude,  size=n),
             data = eruptions2, color="orange2", alpha=0.8, stroke=1)+
  scale_size_area(max_size = 8,
                  guide = guide_legend(direction = "horizontal"))+
#  theme_dark()+
  labs(title = expression(bold("VOLCANIC ERUPTIONS AROUND THE WORLD")),
       #subtitle = "number of eruptions",
       x="",
       y="",
       size="",
       caption = "github.com/kaizadp | data: The Smithsonian Institution")+
  theme(#panel.background = element_rect(fill = "dodgerblue4"),
        #plot.background = element_rect(fill = "dodgerblue4"),
        legend.position = c(0.91, 0.99),
        legend.key=element_blank(),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5,4,0.5,0.5), "cm"))+
    NULL
)
  

(countries_eruptions_1500 = 
    eruptions %>% 
    dplyr::select(volcano_name, start_year) %>% 
    filter(start_year>1500) %>% 
    left_join(dplyr::select(volcano, volcano_name, country), by = "volcano_name") %>% 
    group_by(country) %>% 
    dplyr::summarise(n = n()) %>% 
    na.omit() %>% 
    arrange(desc(n)) %>% 
    top_n(10, n) %>% 
    ggplot()+
    geom_text(aes(x = 0, y = reorder(country,n), label = paste(country, n)),
              hjust=1, size=3, vjust=0, lineheight=0.5)+
    labs(title = "most eruptions",
         x = "",
         y = "")+
    scale_x_continuous(expand = c(0, 0), limits = c(-1,0))+
    theme_void()+
    theme(plot.title = element_text(colour = "black", hjust = 1, size=14, face = "bold"),
          plot.subtitle = element_text(colour = "black", hjust = 1, size=13),
          plot.margin = unit(c(0,0,0,0), "cm"))+
    NULL
)


library(cowplot)

layout <- c(
  area(t = 0, l = 0, b = 3, r = 8),
  area(t = 2, l = 8, b = 3, r = 8)
)
world_map+countries_eruptions_1500  +
  plot_layout(design = layout)
ggsave("map.png", width = 9, height = 5)


ggdraw(world_map) +
  draw_plot(countries_eruptions_1500, x = 0.77, y = 0.25, height = 0.39, width = 0.2)
ggsave("images/2020-05-11_map_eruptions.png", width = 9, height = 4.5)

   


########






japan = ne_countries(scale = "medium", returnclass = "sf", type = "countries", country = "japan")

japan %>% 
  ggplot()+
  geom_sf(fill="#061a21ff", color=NA)+
  geom_point(aes(longitude, latitude),
             data = volcano %>% filter(country=="Japan"), color = "orange2")+
#  scale_color_viridis_c(option = "magma", direction = 1, trans = "log10") +
  scale_size_area(max_size = 8)+
  theme_dark()+
  theme(panel.background = element_rect(fill = "#132e41ff"))

  
japan = ne_countries(scale = "medium", returnclass = "sf", type = "countries", country = "america")

  
japan = ne_states(returnclass = "sf")

japan %>% 
  ggplot()+
  geom_sf(fill="#061a21ff", color=NA)+
  



