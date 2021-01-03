## TIDYTUESDAY 2020-01-21: SPOTIFY

## Kaizad F. Patel
## Jan-02-2021

########### #
########### #
# plots inspired by https://github.com/jakelawlor/TidyTuesday_JL/blob/master/CodeFiles/Jan21.20.R

# new skills in this assignment:
# - radar/spider plots (coord_polar, ggiraphExtra::coord_radar)
# - scaling variables (scale)

########### #
########### #

library(tidyverse)
library(ggiraphExtra)
library(showtext)

# PART 1: TIDYTUESDAY DATA ------------------------------------------------

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# set the order needed for the spider plots
order <- c(#"acousticness", 
  #"instrumentalness", 
  "tempo", 
  "loudness",
  "energy",
  "valence",
  "danceability"
  #"liveness",
  #"speechiness"
) 

# clean the data
# select only the metrics we want
# scale all the metrics on a 0-1 scale
# pivot_longer and reorder

spotify_songs_subset_long = 
  spotify_songs_subset %>% 
  ungroup() %>% 
  dplyr::select(track_name, playlist_genre, danceability:tempo, -key) %>% 
  mutate(tempo = scale(tempo,center=F,scale=max(tempo,na.rm = T)),
         loudness = scale((loudness+40),center=F,scale=max((loudness+40),na.rm = T))) %>% 
  pivot_longer(-c(track_name, playlist_genre)) %>% 
  arrange(factor(name, levels = order))

# calculate medians
spotify_songs_subset_long_median = 
  spotify_songs_subset_long %>% 
  group_by(playlist_genre, name) %>% 
  dplyr::summarise(value = median(value)) %>% 
  arrange(factor(name, levels = order))

# make plot

# load fonts
sysfonts::font_add_google("Oswald", "Oswald")
showtext_auto()

spotify_songs_subset_long %>% 
  ggplot(aes(x = name, y = value, group = track_name))+
  geom_polygon(fill = NA, color = "grey80", size = 0.1, show.legend = F)+
  geom_polygon(data = spotify_songs_subset_long_median,
               aes(color = playlist_genre, fill = playlist_genre, group = playlist_genre),
               alpha = 0.2, size = 1, show.legend = F)+
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  coord_radar()+
  
  labs(title = "Spotify Music Profiles
       ",
       caption = "github.com/kaizadp | Source: spotifyr
       02 January, 2021")+
  scale_x_discrete(limits = order,
                   labels=c("Fast", "Loud","Energetic","Happy","Danceable"))+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, family = "Oswald"),
        plot.caption = element_text(hjust = 0.5, size = 8, color = "grey30", family = "Oswald"),
        panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
        panel.spacing.y = unit(1.5, "lines")) + #facet spacing for y axis)+
  facet_wrap(~playlist_genre)+
  NULL

ggsave("images/2020-01-21_spotify/spotify_tt.tiff", height = 8, width = 8)

#
# PART 2: SPOTIFY TOP 2020 (SELF) -------------------------------------------------------------------------
## this portion uses the {spotifyr} package to download my top 2020 songs for analysis
## the SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET need to be entered
## these values can be obtained by going into https://developer.spotify.com and signing in to the dashboard

## playlist URI can be obtained by going to the playlist in question > ... > share > URI

# library(spotifyr)
# 
# Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxx')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxx')
# 
# access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), 
#                                          client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))
# 

access_token <- get_spotify_access_token()
top2020 <- get_playlist_audio_features(username= "XXXXXX", 
                                       playlist_uris="37i9dQZF1ELVhsoteknKb1",
                                       authorization = get_spotify_access_token())

top2020_subset = 
  top2020 %>% 
  dplyr::select(danceability:tempo,
                `track.name`, `track.album.name`, `track.popularity`, 
                key_name, mode_name, key_mode) %>% 
  rename(track_name = `track.name`) %>% 
  mutate(tempo = scale(tempo,center=F,scale=max(tempo,na.rm = T)),
         loudness = scale((loudness+40),center=F,scale=max((loudness+40),na.rm = T)))


order <- c(
  #"acousticness", 
  #"instrumentalness", 
  "tempo", 
  "loudness",
  "energy",
  "valence",
  "danceability"
  #"liveness",
  #"speechiness"
) 

top2020_subset_long = 
  top2020_subset %>% 
  top_n(50, `track.popularity`) %>% 
  dplyr::select(track_name, danceability:tempo) %>% 
  pivot_longer(-track_name) %>% 
  arrange(factor(name, levels = order))

top2020_subset_long %>% 
  ggplot(aes(x = name, y = value))+
  geom_violin()

top2020_subset_long %>% 
  ggplot()+
  geom_density(aes(y = value, color = name), alpha = 0.2)+
  coord_flip()+
  theme_void()

top2020_subset_long_median = 
  top2020_subset_long %>% 
  group_by(name) %>% 
  dplyr::summarise(value = median(value)) %>% 
  arrange(factor(name, levels = order))

top2020_subset_long %>% 
  filter(value <= 1) %>% 
  ggplot()+
  geom_polygon(aes(x = name, y = value, group = track_name), 
               fill = NA, color = "plum4", size = 0.1)+
  geom_polygon(data = top2020_subset_long_median,
               aes(x = name, y = value, group = 1), 
               fill = "goldenrod2", alpha = 0.3, color = "goldenrod2", size = 0.7)+
  
  ylim(0,1)+
  #coord_polar(clip="off")+
  coord_radar()+
  
  labs(title = "2020 Music Profile",
       subtitle = "2020 was loud and energetic",
       caption = "github.com/kaizadp | Source: spotifyr
       02 January, 2021")+
  
  scale_x_discrete(limits = order,
                   labels=c("Fast", 
                            "Loud","Energetic","Happy","Danceable"))+
  #theme_void()+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        strip.text.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, family = "Oswald"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Oswald", color = "grey30"),
        plot.caption = element_text(hjust = 0.5, size = 6, color = "grey30", family = "Oswald"),
        panel.spacing.x = unit(1.5, "lines"), 
        panel.spacing.y = unit(1.5, "lines")) + 
  NULL

ggsave("images/2020-01-21_spotify/spotify_2020.tiff", height = 3.5, width = 3.5)
