#Import USdata set
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
USvideos <- read_csv("data/USvideos.csv")
#View(USvideos)

talk_show_list = c("Jimmy Kimmel Live",
                   "The Tonight Show Starring Jimmy Fallon",
                   "TheEllenShow",
                   "The Late Late Show with James Corden",
                   "Team Coco",
                   "The Late Show with Stephen Colbert",
                   "Late Night with Seth Meyers")

USvideos = USvideos %>% mutate(isTalkShow = channel_title %in% c("Jimmy Kimmel Live",
                                                      "The Tonight Show Starring Jimmy Fallon",
                                                      "TheEllenShow",
                                                      "The Late Late Show with James Corden",
                                                      "Team Coco",
                                                      "The Late Show with Stephen Colbert",
                                                      "Late Night with Seth Meyers"))


#check amount of average views each top featured channel gets per video
topChannelsAverageTrending= USvideos %>% group_by(channel_title) %>% summarise(TrendingAmount = n(), average = sum(views)/n()) %>%
    arrange(desc(TrendingAmount)) %>% top_n(150,TrendingAmount) %>% glimpse()
#top thirty channels based on trending amount not by average views

top = as.data.frame(topChannelsAverageTrending) %>% mutate(isTalkShow = channel_title %in% c("Jimmy Kimmel Live",
                                                                                       "The Tonight Show Starring Jimmy Fallon",
                                                                                       "TheEllenShow",
                                                                                       "The Late Late Show with James Corden",
                                                                                       "Team Coco",
                                                                                       "The Late Show with Stephen Colbert",
                                                                                       "Late Night with Seth Meyers"))

ggplot(top,aes(TrendingAmount, average, color = isTalkShow)) +
  geom_point() #+ 

source("exploration.R")

##Looks into what percent of trending videos are from talk shows
category_count = final %>% count(category) %>% arrange(desc(n)) %>% rename("overall_count" = "n")

final %>% 
  filter(channel_title %in% talk_show_list) %>% 
  count(category) %>%
  arrange(desc(n)) %>%
  left_join(category_count, by = "category") %>% 
  mutate(presence = n/overall_count)
#category           n overall_count presence
#<chr>          <int>         <int>    <dbl>
#1 Entertainment    612          9964   0.0614
#2 Comedy           551          3457   0.159 
#3 People & Blogs    52          3210   0.0162

USvideos %>%
  count(isTalkShow) %>% 
  mutate(presence = n/sum(n))
#isTalkShow     n presence
#<lgl>      <int>    <dbl>
#1 FALSE      39734   0.970 
#2 TRUE        1215   0.0297

USvideos %>%
  count(channel_title, isTalkShow) %>% 
  count(isTalkShow) %>% 
  mutate(presence = n/sum(n))
# A tibble: 2 x 3
#isTalkShow     n presence
#<lgl>      <int>    <dbl>
#1 FALSE       2200  0.997  
#2 TRUE           7  0.00317

  #geom_text(aes(label = ifelse(isTalkShow, channel_title, "")), size = 2)
#for talkshows: the graph shows that there is no correlation between average views and amount of times trending

