library(dplyr)
library(ggplot2)
library(stringr)


tuesdata <- tidytuesdayR::tt_load('2020-05-19')
tuesdata <- tidytuesdayR::tt_load(2020, week = 21)


vb_matches <- tuesdata$vb_matches

library(skimr)

skim(vb_matches)

vb_subset = 
  vb_matches %>% 
  dplyr::select(circuit, tournament, country, date,
                w_player1, w_player2, w_p1_country, w_p2_country
                )


## which countries hosted the most?
(host = vb_subset %>% 
    group_by(country) %>% 
    dplyr::summarise(n = n()) %>%
    ungroup %>% 
    arrange(desc(n)) %>% 
#    top_n(n, 10) %>% 
# WHY IS TOP_N NOT WORKING?
    head(20) %>% 
    ggplot(aes(x = n, y = reorder(country, n)))+
    geom_point()+
    labs(title = "which countries hosted the most")+
    NULL
)
a = vb_subset %>% arrange(desc(country))%>% head(10)

## which countries had the most wins?
(wins_countries = 
    vb_subset %>% 
    dplyr::select(w_p1_country, w_p2_country) %>% 
    dplyr::mutate(same = (w_p1_country==w_p2_country),
                  w2 = if_else(same==TRUE, as.character(NA),w_p2_country)) %>% 
    dplyr::select(w_p1_country, w2) %>% 
    tidyr::gather(value = "country") %>% 
    na.omit() %>% 
    group_by(country) %>% 
    dplyr::summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
# WHY IS TOP_N NOT WORKING?
    head(20) %>% 
#    top_n(n, 2) %>% 
    ggplot(aes(x = n, y = reorder(country,n)))+
    geom_point()+
    labs(title = "which countries had the most wins?")+
    NULL
)

## for usa only

(wins_usa = 
    vb_subset %>% 
    dplyr::select(w_player1, w_p1_country) %>%
    rename(player = w_player1, country = w_p1_country) %>% 
    rbind(vb_subset %>% dplyr::select(w_player2, w_p2_country) %>% rename(player = w_player2, country = w_p2_country)) %>% 
    filter(country %in% "United States") %>% 
    group_by(player) %>% 
    dplyr::summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    head(20) %>% 
    ggplot(aes(x = n, y = reorder(player, n)))+
    geom_point()+
    labs(title = "which US players had the most wins?")+
    NULL
)
  
  
  
## us players chord diagram
usa_chord = 
  vb_subset %>% 
  filter(w_p1_country=="United States" & w_p2_country=="United States") %>% 
  dplyr::select(w_player1, w_player2) %>% 
  group_by(w_player1,w_player2) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::mutate(from = str_replace_all(w_player1,"[^A-Z]",""),
                to = str_replace_all(w_player2,"[^A-Z]","")) %>% 
  rename(value=n) %>% 


        # rename(from = w_player1,
        #        to = w_player2,
        #        value = n) %>% 
  arrange(desc(value)) %>% 
  head(15) %>% 
  ungroup %>% 
  dplyr::select(from, to, value)

library(circlize)

chordDiagram(usa_chord)
title(main = list("Top Players in the US",
                  cex=2,
                  col="grey75"))
  

## countries chord diagram
countries_chord = 
  vb_subset %>% 
#  filter(w_p1_country=="United States" & w_p2_country=="United States") %>% 
  dplyr::select(w_p1_country, w_p2_country) %>% 
  group_by(w_p1_country,w_p2_country) %>% 
  dplyr::summarise(n = n()) %>% 
# dplyr::mutate(from = str_replace_all(w_player1,"[^A-Z]",""),
#               to = str_replace_all(w_player2,"[^A-Z]","")) %>% 
#  rename(value=n) %>% 
  
  
   rename(from = w_p1_country,
          to = w_p2_country,
          value = n) %>% 
  arrange(desc(value)) %>% 
  head(15) %>% 
  ungroup %>% 
  dplyr::select(from, to, value)

chordDiagram(countries_chord)
title(main = list("Winning Countries",
                  cex=2,
                  col="grey75"))  
