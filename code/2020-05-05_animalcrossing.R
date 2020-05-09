
library(dplyr)
library(ggplot2)
library(tidytext)

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')


str(villagers)
plot(villagers)

# set ggplot theme ----
bgcol <- '#8fd3fe'
textcol <- "grey95"
titlefont <- "Montserrat Medium"
subfont <- "Montserrat"
subfont2 <- "Montserrat Light"

theme_set(theme_classic())
theme_update(plot.background = element_rect(color=bgcol,fill=bgcol),
             panel.background = element_rect(color=bgcol,fill=bgcol),
             axis.text = element_text(size=12, color = "black"),
             axis.title = element_text(size = 12, color = "black", face = "bold"),
             plot.title = element_text(face = "bold"),
             legend.background = element_rect(color = bgcol, fill = bgcol)
             
             )



#
# . word cloud of reviews ----
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


critic_text = critic %>% pull(text) %>% write.table("critic_text.txt", row.names = F, col.names = F)
user_text = user_reviews%>% pull(text) %>% write.table("user_text.txt", row.names = F, col.names = F)

text = readLines("user_text.txt")
docs <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

docs2 <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

docs2 <- tm_map(docs2, content_transformer(tolower))
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
docs2 <- tm_map(docs2, removeWords, c("animal crossing", "new horizons", "game")) 

dtm <- TermDocumentMatrix(docs2) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 100,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"),
          scale=c(3.5,0.25))

wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df, size=1, color='random-dark')

#

# 1. what personalities do the species have? ----

# calculate summary of counts per species/personality
villagers_person = 
  villagers %>% 
  group_by(species, personality) %>% 
  summarise(n = n()) %>% 
  ungroup %>% 
  group_by(species) %>% 
  mutate(total = n())


ggplot(villagers_person, aes(x = personality, y = reorder(species, desc(species)), size = n))+
  geom_point()+
  theme(legend.background = element_rect(fill = bgcol))+
  labs(title = "what personalities do the characters have?\n",
       y = "",
       caption = "github.com/kaizadp | data: Animal Crossing",
       size = "number")

ggsave("images/2020-05-05_animalcrossing/personalities.png", height = 8, width = 10)


#
# tidytext ----
## 1. what was the distribution of grades? ----

(gg_grades_distrib = 
  user_reviews %>% 
  ggplot(aes(x = grade))+
  geom_histogram(fill = "grey20", binwidth = 0.5)+
  labs(title = "how were grades distributed?",
       subtitle = "extreme grades were most frequent")+
  scale_x_continuous(breaks = c(0,2,4,6,8,10))
)

#

## 2. how did grades change over time? ----
(gg_reviews = 
   user_reviews %>%
  group_by(date = lubridate::floor_date(date, "week")) %>% 
  dplyr::summarise(avg_grade = mean(grade),
                   n = n()) %>% 
    filter(n>20) %>% 
  ggplot(aes(x = date, y = avg_grade))+
  geom_point(aes(size = n))+
  geom_path()+
   scale_size_area(max_size = 10)+
  labs(size = "number of reviews",
       title = "user reviews: how did grades change over time?",
       caption = "github: kaizadp | data: Animal Crossing",
       x="",
       y = "average grade")+
   theme(legend.position = "top")+
   NULL
)


## 3. analyze the text ----

data("stop_words")

review_words = 
  user_reviews  %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(user_name, word, grade) %>% 
  arrange(desc(n))

(review_words_bar = 
  review_words %>% 
  count(word, grade) %>% 
  filter(!word %in% c("nintendo", "switch", "animal", "crossing", "new", "horizon")) %>%   
  group_by(grade) %>% 
  top_n(10, n) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>%   
  filter(grade %in% c(0,10)) %>% 
  dplyr::mutate(grade = paste("grade =",grade)) %>%   
  ggplot(aes(x = n, y = reorder(word, n)))+
  geom_bar(stat = "identity", aes(fill = as.factor(grade)), show.legend = F)+
  facet_grid(grade~., scales = "free")+
  labs(title = "user reviews: top words",
       subtitle = "for 0 and 10 grades",
       fill = "grade",
       caption = "github: kaizadp | data: Animal Crossing")+
    theme(strip.background = element_rect(fill = bgcol, color = bgcol),
          strip.text = element_text(size=12, face="bold"))+
  NULL
)


(review_words_scatter_top =     
    review_words %>% 
  group_by(word) %>% 
  summarize(n = n(),
            avg_grade = mean(grade)) %>% 
  filter(!word %in% c("nintendo", "switch", "animal", "crossing", "new", "horizon")) %>%   
  filter(n>=50) %>% 
  top_n(50, avg_grade) %>% 
  ggplot(aes(n, avg_grade))+
  geom_point()+
  geom_text(aes(label = word), vjust=1, hjust=0, check_overlap = TRUE, size=3)+
  scale_x_log10()+
  labs(title = "what words were associated with high grade reviews?",
       subtitle = "top 50 words, only seen in 50+ reviews",
       y = "average grade",
       x = "count")+
  NULL
)
 

(review_words_scatter_bottom =     
    review_words %>% 
  group_by(word) %>% 
  summarize(n = n(),
            avg_grade = mean(grade)) %>% 
  filter(!word %in% c("nintendo", "switch", "animal", "crossing", "new", "horizon")) %>%   
  filter(n>=50) %>% 
  top_n(-50, avg_grade) %>% 
  ggplot(aes(n, avg_grade))+
  geom_point()+
  geom_text(aes(label = word), vjust=1, hjust=0, check_overlap = TRUE, size=3)+
  scale_x_log10()+
  labs(title = "what words were associated with low grade reviews?",
       subtitle = "lowest 50 words, only seen in 50+ reviews",
       y = "average grade",
       x="count",
       caption = "github: kaizadp | data: Animal Crossing")+
  NULL
)


## combining plots ----
library(patchwork)

(gg_grades_distrib + review_words_scatter_top)/(gg_reviews + review_words_scatter_bottom)+
  plot_layout(widths = c(1,1), heights = c(1,1))

(gg_grades_distrib /gg_reviews) 
ggsave("images/2020-05-05_animalcrossing/grades.png", height = 8, width = 6)


review_words_scatter_top / review_words_scatter_bottom
ggsave("images/2020-05-05_animalcrossing/grades_words.png", height = 8, width = 8.5)


