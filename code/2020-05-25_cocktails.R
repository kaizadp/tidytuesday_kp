# 2020-05-25 cocktails
# Kaizad F. Patel

# load packages ----

library(dplyr)
library(stringr)

library(ggalluvial) # sankey plot


# load files ----
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# set ggtheme ----

bgcol <- '#73b881'
textcol <- "grey95"
titlefont <- "Montserrat Medium"
subfont <- "Montserrat"
subfont2 <- "Montserrat Light"

theme_set(theme_void())
theme_update(plot.background = element_rect(color=bgcol,fill=bgcol),
             panel.background = element_blank()
             ,
#             axis.text = element_text(size=12, color = "black"),
#             axis.title = element_text(size = 12, color = "black", face = "bold")
             #            axis.line = element_line(color=textcol),
             #            text = element_text(color=textcol),
             #            axis.ticks = element_line(color=textcol),
             #            axis.text = element_text(color=textcol,family=subfont2),
             #            axis.title = element_text(family=subfont),
             #            plot.title = element_text(family=titlefont),
             #            plot.subtitle = element_text(family=subfont),
             #            plot.caption = element_text(family=subfont2)
)
#
# absinthe cocktails ----
## select cocktails containing absinthe
## make a list
abs_list =
  boston_cocktails %>% 
# there are entries with different strings containing absinthe, so search for the string
  dplyr::mutate(absinthe = str_detect(ingredient, "Absinthe")) %>% 
  filter(absinthe %in% TRUE) %>% 
# remove absinthe splashes
  dplyr::mutate(absinthe_splash = (str_detect(ingredient, "Absinthe") & str_detect(measure, "splash"))) %>% 
  filter(absinthe_splash %in% FALSE) %>% 
  pull(name)

## use the list to filter the cocktails needed
abs_cocktails = 
  boston_cocktails %>% 
  filter(name %in% abs_list) %>% 
  filter(!str_detect(name, "T.N.T.")) 
  

# skimr::skim(abs_cocktails)
# levels(as.factor(abs_cocktails$name))

#
# sankey plot ----

(abs_sankey = 
    abs_cocktails %>% 
    #  filter(name %in% c("Dirty Harry", "Paddington", "Gotham")) %>% 
    filter(!str_detect(ingredient, "Absinthe")) %>% 
   dplyr::select(name, category) %>% 
   distinct() %>% 
    ggplot(
      aes(axis2 = name,
          axis1 = category,
          y = 1)) +
    geom_alluvium(aes(fill = category), alpha = 1) +
    geom_stratum() +
    geom_text(stat = "stratum", 
              label.strata = TRUE) +
    #  scale_x_discrete(limits = c("Class", "Sex", "Survived"),
    #                   expand = c(.1, .1)) +
    scale_fill_viridis_d(option = "plasma") +
      labs(title = "What kind of absinthe cocktail?",
           subtitle = "based on cocktail type"
    ) +
    #  theme_minimal() +
#    theme_void()+
    theme(legend.position = "none") +
    #  coord_flip()+
    NULL
)


# labels/recipes ----
ingredients = 
  abs_cocktails %>% 
  dplyr::mutate(ing = paste(measure, ingredient)) %>% 
  dplyr::select(name, ing, ingredient_number) %>% 
  tidyr::spread(ingredient_number, ing) %>% 
  dplyr::mutate(all_ing = paste0(`1`," \n",`2`," \n",`3`," \n",`4`," \n",`5`," \n",`6`),
                all_ing = str_replace_all(all_ing, "NA",""),
                all_ing = str_replace_all(all_ing, "\n \n","")) %>% 
  dplyr::select(name, all_ing) %>% 
  dplyr::mutate(label1 = paste0(row_number(),": ",name),
                id = row_number())



library(ggforce)

(gg_ingred = 
    ingredients %>% 
    arrange(id) %>% 
    ggplot(aes(x = 1, y = 1))+
    geom_text(aes(label = all_ing), size=3)+
    facet_wrap(~id+name, scales = "free", ncol = 3)+
    labs(caption = "github.com/kaizadp | data: Kaggle & Kaggle")+
    theme(strip.text = element_text(face = "bold", size=10),
          plot.margin = margin(1,1,1,1, "cm"))+
    NULL
)


#
# mix and match ----
categories = 
  abs_cocktails %>% 
  dplyr::select(name, category) %>% 
  distinct()

profile = 
  abs_cocktails %>% 
  dplyr::select(name) %>% 
  distinct() %>% 
  arrange((name)) %>% 
  dplyr::mutate(id = row_number(),
                profile = c("anise", "citrus", "anise", "maraschino", 
                            "cream", "peach", "anise", "citrus", "maraschino bitters",
                            "maraschino bitters", "gin", "citrus", "bitters",
                            "maraschino citrus pineapple", "chocolate"),
                recom = c(7,7,8,6,2,7,9,8,7,7,7,5,8,6,4)) %>% 
  tidyr::separate(profile, c("profile1", "profile2", "profile3"), sep = " ") %>% 
  tidyr::pivot_longer(-c(name, id, recom), values_to = "profile", names_to = "x") %>% 
  dplyr::select(-x) %>% 
  na.omit() %>% 
  left_join(categories, by = "name")

#library(ggrepel)
(gg_profile = 
    profile %>% 
    filter(!id %in% c(1, 10)) %>% 
    ggplot(aes(x = profile, y = reorder(category, desc(category)), 
               size = recom, fill = as.character(recom)))+
    geom_point(shape = 21, alpha = 0.5)+
    #  geom_label(size=4)+
    geom_text(aes(label = id), check_overlap = T, size=4, )+
    geom_curve(x = 1.2, xend = 1, y = 4.5,  yend = 5.5, curvature = -0.2, size=0.5)+
    annotate("text", label = "Green Opal \ntop recommended!", x = 1, y = 4.2, hjust = "left")+
    scale_size_binned(range = c(0.5,15))+
    scale_x_discrete(position = "top")+
    scale_fill_viridis_d(option = "plasma", direction = 1) +
    theme_minimal()+
    theme(legend.position = "none",
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size=10),
          axis.text.x = element_text(angle = 45, size=12, hjust = 0),
          panel.background = element_rect(fill = "#adff2f"),
          plot.background = element_rect(fill = "#138808"))+
    
    NULL
)

# chord diagram ----
library(circlize)

abs_chord = 
  profile %>% 
  dplyr::select(name, id, category) %>% 
  distinct() %>% 
  dplyr::mutate(category = recode(category,
                                  "Whiskies" = "Whisky",
                                  "Rum - Daiquiris" = "Rum",
                                  "Cordials and Liqueurs" = "Liqueur",
                                  "Cocktail Classics" = "Classic"),
                from = category,
                to = id,
                y = 1) %>% 
  dplyr::select(from, to, y)


circos.par(start.degree = 90, clock.wise = FALSE)
gg_chord = chordDiagram(abs_chord)

pal = PNWColors::pnw_palette("Sunset2",7)


grid.col = c(Brandy = pal[1], 
             Tequila = pal[2], 
             Vodka = pal[3], 
             Classic = pal[4], 
             Liqueur = pal[5],
             Whisky = pal[6], 
             Rum = pal[7],
             
             
             `1` = "grey", `2` = "grey", `3` = "grey", `4` = "grey", `5` = "grey", `6` = "grey",
             `7` = "grey", `8` = "grey", `9` = "grey", `10` = "grey", `11` = "grey", `12` = "grey",
             `13` = "grey", `14` = "grey", `15` = "grey")

par(
  mar = c(1, 0, 3, 0),    # Margin around chart
  bg = c("#d0e7d5"),     # background color
  family="American Typewriter"
)
gg_chord = chordDiagram(abs_chord, grid.col = grid.col,directional = 1,
                        annotationTrack = c("name", "grid"),
                        annotationTrackHeight = c(0.01, 0.05))
title(main = list(expression(bold("Absinthe: Choose your cocktail")),
                  cex=2.5,
                  col="#204027"))
# text(0.7, -1, "github.com/kaizadp | data: Kaggle & Kaggle", cex = 1)

circle <- recordPlot()



col_mat = rand_color(length(abs_chord), transparency = 0.5)

#
# absinthe heatmap ----
(gg_heatmap = 
   profile %>% 
   ggplot(aes(x = profile, y = category, fill = category))+
   geom_tile()+
   geom_text(aes(label = id), check_overlap = T)+
   theme_minimal()+
   NULL
)



circos.clear()
chord2 = 
  profile %>% 
  tidyr::pivot_longer(cols = profile:category, names_to = "names", values_to = "label") %>% 
  mutate(from = label,
         to = id, 
         y = 1) %>% 
  dplyr::select(from, to, y)
chordDiagram(chord2)



chordDiagram(mat)
data(mat)
circos.info()

#  

mat1 = matrix(rnorm(25), nrow = 5)
rownames(mat1) = paste0("A", 1:5)
colnames(mat1) = paste0("B", 1:5)

mat2 = matrix(rnorm(25), nrow = 5)
rownames(mat2) = paste0("A", 1:5)
colnames(mat2) = paste0("C", 1:5)

mat3 = matrix(rnorm(25), nrow = 5)
rownames(mat3) = paste0("B", 1:5)
colnames(mat3) = paste0("C", 1:5)

mat = matrix(0, nrow = 10, ncol = 10)

rownames(mat) = c(rownames(mat2), rownames(mat3))
colnames(mat) = c(colnames(mat1), colnames(mat2))
mat[rownames(mat1), colnames(mat1)] = mat1
mat[rownames(mat2), colnames(mat2)] = mat2
mat[rownames(mat3), colnames(mat3)] = mat3
mat

chordDiagram(mat, annotationTrack = c("grid", "axis"),
             preAllocateTracks = list(
               track.height = uh(4, "mm"),
               track.margin = c(uh(4, "mm"), 0)
             ))
circos.track(track.index = 2, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), mean(ylim), sector.index, cex = 0.6, niceFacing = TRUE)
}, bg.border = NA)



highlight.sector(rownames(mat1), track.index = 1, col = "red", 
                 text = "A", cex = 0.8, text.col = "white", niceFacing = TRUE)
highlight.sector(colnames(mat1), track.index = 1, col = "green", 
                 text = "B", cex = 0.8, text.col = "white", niceFacing = TRUE)
highlight.sector(colnames(mat2), track.index = 1, col = "blue", 
                 text = "C", cex = 0.8, text.col = "white", niceFacing = TRUE)


####

names = profile %>% pull(name)
profile2 = profile %>% pull(profile)
category = profile %>% pull(category)

all = c(names, profile2, category)
totalsegs <- unique(abind::abind(unique(chord3$from),unique(chord3$to)))
chord2 %>% dplyr::select(-y) -> chord3


chordDiagram(x=chord3, order = all,
             directional = 1,
             #  direction.type = "arrows",
             diffHeight = F,
             preAllocateTracks = list(list(track.height=  uh(3,"mm")   ), # outside track for names
                                      list(track.height=  uh(10,"mm")    )), # middle track for regions
             self.link = 1,
             annotationTrack = "grid",
             grid.col = grid.col
)

highlight.sector(category, track.index = 1, col = "red",
                 text = "Category", cex = 0.7, text.col = "white", niceFacing = TRUE)








# combine ----
library(patchwork)
abs_sankey+gg_ingred+
  plot_layout(widths = c(2,1))

gg_profile+gg_ingred+
  plot_layout(widths = c(1,2))

circle+gg_ingred+
  plot_layout(widths = c(1,2))




gg_chord+gg_ingred

library(cowplot)
plot_grid(circle, gg_ingred)
ggsave("images/2020-05-25/absinthe.png", height = 8, width = 20, units = "in")






