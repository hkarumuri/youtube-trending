#Import USdata set
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
USvideos <- read_csv("data/USvideos.csv")
View(USvideos)

#filter only the talk show channels
talkShow = USvideos %>% filter(channel_title %in% c("Jimmy Kimmel Live",
                                                    "The Tonight Show Starring Jimmy Fallon",
                                                    "TheEllenShow",
                                                    "The Late Late Show with James Corden",
                                                    "Team Coco",
                                                    "The Late Show with Stephen Colbert",
                                                    "Late Night with Seth Meyers"))
#groupby each channel and count how many total times
# that they have been featured on the trending page
timesTrending = talkShow %>% group_by(channel_title) %>% count(channel_title) %>%  arrange(desc(n))
timesTrending
#channel_title                              n
#<chr>                                  <int>
#1 The Tonight Show Starring Jimmy Fallon   197
#2 TheEllenShow                             193
#3 The Late Show with Stephen Colbert       187
#4 Jimmy Kimmel Live                        186
#5 Late Night with Seth Meyers              183
#6 The Late Late Show with James Corden     163
#7 Team Coco                                106

#Finding channels to be most featured on trending page
topChannels = USvideos %>% group_by(channel_title) %>% count(channel_title) %>% arrange(desc(n))
head(topChannels, 20)
#The 10 channels that have been featured on the Trending page the most
#channel_title                              n
#<chr>                                  <int>
#1 ESPN                                     203
#2 The Tonight Show Starring Jimmy Fallon   197
#3 Netflix                                  193
#4 TheEllenShow                             193
#5 Vox                                      193
#6 The Late Show with Stephen Colbert       187
#7 Jimmy Kimmel Live                        186
#8 Late Night with Seth Meyers              183
#9 Screen Junkies                           182
#10 NBA                                     181

#check amount of average views each talkshow gets per video
talkShowAverageTrending= talkShow %>% group_by(channel_title) %>% summarise(TrendingAmount = n(), average = sum(views)/n()) %>% arrange(desc(TrendingAmount))
ggplot(talkShowAverageTrending,aes(TrendingAmount, average, color = channel_title)) +
  geom_point()
#for talkshows: the graph shows that there is no correlation between average views and amount of times trending

#check amount of average views each top featured channel gets per video
(topChannelsAverageTrending= USvideos %>% group_by(channel_title) %>% summarise(TrendingAmount = n(), average = sum(views)/n()) %>%
    arrange(desc(TrendingAmount)) %>% top_n(30,TrendingAmount))
#top thirty channels based on trending amount not by average views

ggplot(topChannelsAverageTrending,aes(TrendingAmount, average, color = channel_title)) +
  geom_point()
#for talkshows: the graph shows that there is no correlation between average views and amount of times trending

