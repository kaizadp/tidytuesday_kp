devtools::install_github("thebioengineer/tidytuesdayR")

library(ggplot2)
library(dplyr)

# 1. load data ----
tuesdata <- tidytuesdayR::tt_load('2020-04-28')
#tuesdata <- tidytuesdayR::tt_load(2020, week = 18)

names(tuesdata)

grosses = tuesdata$grosses
synopses = tuesdata$synopses
cpi = tuesdata$cpi
pre_1985_starts = tuesdata$`pre-1985-starts`

str(grosses)

# 2. set ggplot theme ----
bgcol <- '#EADB5A'
textcol <- "grey95"
titlefont <- "Montserrat Medium"
subfont <- "Montserrat"
subfont2 <- "Montserrat Light"

theme_set(theme_classic())
theme_update(plot.background = element_rect(color=bgcol,fill=bgcol),
             panel.background = element_blank(),
             axis.text = element_text(size=12, color = "black"),
             axis.title = element_text(size = 12, color = "black", face = "bold")
#            axis.line = element_line(color=textcol),
#            text = element_text(color=textcol),
#            axis.ticks = element_line(color=textcol),
#            axis.text = element_text(color=textcol,family=subfont2),
#            axis.title = element_text(family=subfont),
#            plot.title = element_text(family=titlefont),
#            plot.subtitle = element_text(family=subfont),
#            plot.caption = element_text(family=subfont2)
)



# 3. questions ----
# a. what were the highest grossing shows?
# b. what were the longest running shows?
# c. how did ticket prices change over time?

#
## ------------- ----
## ------------- ----
# 4. higest grossing shows ----
highest_gross = 
  grosses %>% 
  dplyr::select(show, weekly_gross) %>% 
  group_by(show) %>% 
  dplyr::summarise(total_gross = sum(weekly_gross),
                   weeks = n()) %>% 
  arrange(desc(total_gross)) %>% 
  top_n(20, total_gross)


# plots
gg_gross1 =
  ggplot(highest_gross, aes(y = reorder(show, total_gross), x = total_gross/1e+6))+
  geom_path(group=1, color = "black")+
  geom_point(size=3)+

  xlim(0,2000)+
  
  annotate("text", label = "PLAYBILL", x = 700, y = 6.5, size = 17 , family = "mono", fontface = "bold", hjust = 0) +
  annotate("text", label = "highest grossing", x = 700, y = 5, size = 10 , family = "mono", fontface = "bold", hjust = 0) +
  annotate("text", label = "not adjusted for inflation",  x = 700, y = 4, size = 4, family = "mono", hjust = 0) +

  annotate("text", label = "million $",  x = 1800, y = 1, size = 4,  hjust = 0) +
    
  labs(x = "",
       y = "",
       caption = "github.com/kaizadp | data: Playbill")+
  
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
        )
  

gg_gross2 =
  ggplot(highest_gross, aes(y = reorder(show, total_gross), x = total_gross/1e+6))+
  geom_path(group=1, color = "black")+
  geom_point(size=3)+
  
    geom_text(data = highest_gross,aes(label=show,x=total_gross/1e+6 +50 ,y=show),
              hjust=0,family="American Typewriter", size=4, color = "black")+
  scale_color_viridis_c(option = "inferno")+
  
  xlim(0,2000)+
  
  annotate("text", label = "highest grossing", x = 0, y = 3, size = 10, angle = 90 , family = "mono", fontface = "bold", hjust = 0) +
  annotate("text", label = "not adjusted for inflation",  x = 100, y = 3, size = 4, angle = 90, family = "mono", hjust = 0) +
  
  annotate("text", label = "million $",  x = 1800, y = 1, size = 4,  hjust = 0) +
  
  labs(x = "",
       y = "")+
  
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
  )

#
# 5. durations of these shows ----
topgrossingshows = 
  highest_gross %>% 
  pull(show)

years = 
  grosses %>% 
  dplyr::mutate(year = format(week_ending, "%Y")) %>% 
  group_by(show) %>% 
  dplyr::summarise(start = as.numeric(min(year)),
                   stop = as.numeric(max(year)),
                   length = stop-start) %>% 
  filter(show %in% topgrossingshows) %>% 
  left_join(highest_gross, by = "show")

gg_years =
  ggplot(years, aes(y = reorder(show, total_gross)))+
  geom_point(aes(x = start), shape = 21, stroke = 1, size=2, fill = "yellow4") +
  geom_point(aes(x = stop), shape = 21,  stroke = 1, size=2,  fill = "yellow4") +
  geom_segment(aes(x = start, xend = stop, y = show, yend = show))+
  # geom_text(aes(x = 2035, label = show), size=3)+
  annotate("text", label = "years run", x = 1990, y = 1)+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()
  )+
  labs(x = "",
       y = "") +
  scale_x_continuous(limits = c(1985, 2020), breaks = c(1990, 2000, 2010, 2020))

gg_labels =
  ggplot(years, aes(y = reorder(show, total_gross)))+
  geom_text(aes(x = 2005, label = show), size=4, family="American Typewriter")+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()
  )+
  labs(x = "",
       y = "")+
  scale_x_continuous(limits = c(2004, 2006))


#
# 6. longest running shows ----
# which were the longest running shows?

duration = 
  grosses %>% 
  group_by(show) %>% 
  dplyr::summarise(n = n()) %>% 
  top_n(20, n)

gg_duration =
  ggplot(duration, aes(y = n, x = reorder(show, -n)))+
  geom_point(size=3)+
  geom_segment(aes(y = 0, yend = n, x = reorder(show, n), xend = reorder(show, n)))+
  #geom_path(group=1)
  geom_text(data = duration,aes(label=show,y=n+50,x=show),
            hjust=0,family="American Typewriter", size=4, angle = 90)+
  ylim(0,2500)+
  labs(y = "weeks",
       x = "",
       caption = paste0("source: Playbill\n",
                        "github.com/kaizadp"))+
  annotate("text", label = "longest running", fontface = "bold", 
           y = 2400, x = 9, size = 9, family = "mono",  hjust=0) +
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

gg_duration2  =
  ggplot(duration, aes(y = n, x = reorder(show, n)))+
  geom_point(size=3)+
  geom_segment(aes(y = 150, yend = n, x = reorder(show, n), xend = reorder(show, n)))+
  geom_text(data = duration,aes(label=show,y=n+50,x=show),
            hjust=0,family="American Typewriter", size=4, angle = 90)+
  scale_y_continuous(position = "right",
                     limits = c(0,2500))+
  labs(y = "weeks",
       x = "",
       caption = "github.com/kaizadp | data: Playbill")+
  annotate("text", label = "longest running", fontface = "bold", y = 0, x = 2, size = 9, family = "mono",  hjust=0) +
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()
  )

# how did the longest running show change price over time?

theatres_gross2 = 
  grosses %>% 
  dplyr::mutate(year = format(week_ending, "%Y"))

gg_phantom  =
  ggplot(theatres_gross2[theatres_gross2$show=="The Phantom of the Opera",],
         aes(x = as.numeric(year), y = avg_ticket_price))+
  geom_point(color = "grey50")+
  geom_smooth(color = "black")+
  
  labs(x = "",
       y = "$",
       caption = "source: Playbill")+
  annotate("text", label = "The Phantom of the Opera", fontface = "bold", x = 2000, y = 130, size = 7, family = "mono")+
  annotate("text", label = "ticket prices over time", x = 2000, y = 125, size = 5, family = "mono")


subset = 
  sales_time %>% 
  filter(show %in% c("Wicked", "Miss Saigon", "Hamilton", "The Phantom of the Opera"))


ggplot(subset, aes(x = as.numeric(year), y = avg_ticket_price, color = show))+
  geom_point()

#
# 7.ticket prices over time ----
sales_time = 
  grosses %>% 
  dplyr::mutate(year = format(week_ending, "%Y")) %>% 
  group_by(year) %>% 
  dplyr::mutate(highest_show = case_when(avg_ticket_price == max(avg_ticket_price)~show))


# find the most expensive 10+ year shows
sales_ten = 
  sales_time %>% 
  group_by(show) %>% 
  dplyr::mutate(year = as.numeric(year),
                duration = max(year)-min(year)) %>% 
  filter(duration>9) %>% 
  group_by(show) %>% 
  dplyr::summarise(show_price = mean(avg_ticket_price),
                   duration = max(duration)) %>% 
  top_n(5,show_price) %>% 
  pull(show)

sales_time2 = 
  sales_time %>% 
  filter(show %in% 
           c("Aladdin", "Beauty and the Beast", "Hamilton", "Hairspray", "Mamma Mia!")) %>% 
  group_by(show, year) %>% 
  dplyr::summarise(mean_price = mean(avg_ticket_price)) %>% 
  dplyr::mutate(year = as.numeric(year))

sales_time_summary = 
  sales_time %>% 
  group_by(year) %>% 
  dplyr::summarise(avg_ticket_price = mean(avg_ticket_price))

gg_prices = 
  ggplot(sales_time, aes(x = as.numeric(year), y = avg_ticket_price))+
  geom_point(color = "grey70")+
  geom_smooth(se=FALSE, data = sales_time2, aes(x = as.numeric(year), y = mean_price, color = show))+

  annotate("text", label = "Aladdin", x = 2018, y = 10, hjust=0)+  
  annotate("text", label = "Beauty and \n the Beast", x = 1992, y = 140)+  
  annotate("text", label = "Hairspray", x = 2000, y = 220)+  
  annotate("text", label = "Hamilton", x = 2013, y = 350)+  
  annotate("text", label = "Mamma Mia", x = 2008, y = 270)+  
  
  geom_curve(x = 2017, xend = 2016, y = 10, yend = 100, curvature = -0.4, color = "#D95F02")+ # aladdin
  geom_curve(x = 1992, xend = 1995, y = 100, yend = 50, curvature = 0.2, color = "#1B9E77")+ # bb
  geom_curve(x = 2000, xend = 2003, y = 200, yend = 80, curvature = 0.4, color = "#E7298A")+ #hs
  geom_curve(x = 2013, xend = 2017, y = 330, yend = 280, curvature = 0.4, color = "#7570B3")+ # hamilton
  geom_curve(x = 2008, xend = 2012, y = 250, yend = 100, curvature = 0.3, color = "#A6761D")+ # mm
    
  scale_color_manual(values = c("#D95F02", "#1B9E77", "#E7298A", "#7570B3", "#A6761D"))+
    labs(x = "",
       y = "$",
       caption = "github.com/kaizadp | data: Playbill")+

  annotate("text", label = "Ticket Prices ", x = 1986, y = 450, size = 10, hjust=0,
           family = "mono", fontface = "bold")+
    theme(legend.position = "none")

#
# 8. OUTPUTS ----
library(patchwork)

gg_gross2 + gg_duration2
ggsave("highest.tiff", height = 8, width = 15)

gg_years + gg_labels + gg_gross1 +
  plot_layout(widths = c(2,1.5,3))
ggsave("grossing.tiff", height = 8, width = 15)

gg_prices
ggsave("prices.tiff", height = 6, width = 10)



embed_fonts("combined.pdf", outfile="font_ggplot_embed.pdf")


#
## ------------- ----
## ------------- ----
## MISCELLANEOUS (not plotted) ----
# most successful theaters ----
theatres_gross1 = 
  grosses %>% 
  dplyr::mutate(year = format(week_ending, "%Y")) %>% 
  #  filter(year>1999) %>% 
  group_by(theatre, show) %>% 
  dplyr::summarise(total_gross = sum(weekly_gross)) %>% 
  ungroup


theatres_gross_temp = 
  theatres_gross1 %>% 
  group_by(theatre) %>% 
  dplyr::summarize(total = sum(total_gross)) %>% 
  top_n(10, total)

theatres_gross = 
  theatres_gross1 %>% 
  left_join(theatres_gross_temp, by = "theatre") %>% 
  filter(!is.na(total))

gg_theatres = ggplot(theatres_gross, 
                     aes(x = total_gross/1e+9, y = reorder(theatre, total), fill = show))+
  geom_bar(stat = "identity")+
  theme(legend.position = "none")



# how many plays per year? ----

year = 
  grosses %>% 
  dplyr::mutate(year = format(week_ending, "%Y")) %>% 
  group_by(year, show) %>% 
  dplyr::summarise(n = n()) %>% 
  group_by(year) %>% 
  dplyr::summarise(n = n()) %>% 
  
  filter(!year=="2020",
         !year=="1985")

gg_years = ggplot(year, aes(x = as.numeric(year), y = n))+
  geom_point(size=2)+
  geom_path(group=1)+
  ylab("number of shows")+
  xlab("year")

