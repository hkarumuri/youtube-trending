#Import USdata set
library(readr)
library(tidyverse)
library(dplyr)
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
#channel_title                              n
#<chr>                                  <int>
#1 The Tonight Show Starring Jimmy Fallon   197
#2 TheEllenShow                             193
#3 The Late Show with Stephen Colbert       187
#4 Jimmy Kimmel Live                        186
#5 Late Night with Seth Meyers              183
#6 The Late Late Show with James Corden     163
#7 Team Coco                                106

#find the category of these talk show hosts
(talkShowCategory = talkShow %>% group_by(category_id)) %>%  select(category_id, channel_title) %>% distinct()

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

topChannels = USvideos %>% group_by(channel_title) %>% count(channel_title) %>% arrange(desc(n))
head(topChannels, 20)