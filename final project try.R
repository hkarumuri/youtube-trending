#Import USdata set
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
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


#lets see what category has the most views
totalNumberViewPerCategory =USvideos %>%
  group_by(category_id) %>%
  summarise(totalviews = sum(views))
totalNumberViewPerCategory %>% arrange(desc(totalviews)) %>% 
  ggplot(aes(category_id, totalviews,fill=totalviews))+
  geom_col()
#how can I make each category show up????
#this shows that music has the most views

#lets see what category has most likes
totalNumberLikesPerCategory =USvideos %>%
  group_by(category_id) %>%
  summarise(totalLikes = sum(likes))
totalNumberLikesPerCategory %>% arrange(desc(totalLikes)) %>% 
  ggplot(aes(category_id, totalLikes,fill=totalLikes))+
  geom_col()
#yes again music videos has most likes

totalNumberCommentsPerCategory =USvideos %>%
  group_by(category_id) %>%
  summarise(totalComments = sum(comment_count))
totalNumberCommentsPerCategory %>% arrange(desc(totalComments)) %>% 
  ggplot(aes(category_id, totalComments,fill=totalComments))+
  geom_col()

#lets compare everything at once
#
totalLikesDislikesViewsComments =USvideos %>%
  group_by(category_id) %>%
  summarise(totalLikes = sum(likes), 
            totalViews = sum(views),
            totalDislikes = sum(dislikes),
            totalComments = sum(comment_count))

#totalLikesDislikesViewsComments %>% arrange(desc(totalLikes)) %>% 
 # ggplot(aes(category_id))+
 # geom_col(aes(y=totalLikes), position = "dodge")
  #geom_col(aes(y=totalViews), position = "dodge")
  
  
  

  



#check amount of average views each talkshow gets per video
talkShowAverageTrending= talkShow %>% group_by(channel_title) %>% summarise(TrendingAmount = n(), average = sum(views)/n()) %>% arrange(desc(TrendingAmount))
ggplot(talkShowAverageTrending,aes(TrendingAmount, average, color = channel_title)) +
  geom_point()
#for talkshows: the graph shows that there is no correlation between average views and amount of times trending

talkShowAverageLikesTrending= talkShow %>% group_by(channel_title) %>% summarise(TrendingAmount = n(), average = sum(likes)/n()) %>% arrange(desc(TrendingAmount))
ggplot(talkShowAverageLikesTrending,aes(TrendingAmount, average, color = channel_title)) +
  geom_point()



#check amount of average views each top featured channel gets per video
categoryFix = USvideos %>% mutate(category_id = as.character(category_id))#because continous
(topChannelsAverageTrending= categoryFix %>% group_by(category_id) %>% summarise(TrendingAmount = n(), average = sum(views)/n()) %>%
    arrange(desc(TrendingAmount)) %>% top_n(100,TrendingAmount))
#top thirty channels based on trending amount not by average views

ggplot(topChannelsAverageTrending,aes(TrendingAmount, average, color = category_id)) +
  geom_point()+
  #theme(legend.position = 'none')+
  scale_y_log10()
#for talkshows: the graph shows that there is no correlation between average views and amount of times trending


#---------------------------------
#Lets explore how many views each category gets
#ViewsvsCategory = USvideos %>% group_by(category_id)
#ggplot(ViewsvsCategory, aes(category_id, views))+
#  geom_boxplot()

USvideos %>% 
  ggplot(aes(category_id,views, color=views))+
  geom_boxplot()+
  theme(axis.t)

#What time of publication has best chance to be trending
#change time to actual time
timePublished = USvideos %>% mutate(publish_time = ymd_hms(publish_time))
#want only the time of day
hourPublished = hour(timePublished$publish_time)
timePublished
timePublished%>%
  ggplot(aes(hourPublished))+
  geom_histogram(stat = "count")+
  xlab('Hour')+
  ylab('Amount of Videos')+
  ggtitle("Publication Time of Trending Videos")
  
#lets compare this to talkshows
timePublished = talkShow %>% mutate(publish_time = ymd_hms(publish_time))
#want only the time of day
hourPublished = hour(timePublished$publish_time)
timePublished
timePublished%>%
  ggplot(aes(hourPublished))+
  geom_histogram(stat = "count")+
  xlab('Hour')+
  ylab('Amount of Videos')+
  ggtitle("Publication Time of Talk Show Trending Videos")
#this shows the publication time of talk shows
#but all talkshows are in california so they should be 3 hours behind


#THIS HAS BEEN MODIFIED FOR THE BETTER
#lets take a look at what category usually gets the most views
categoryFix = USvideos %>% mutate(category_id = as.character(category_id))#because continous
categoryFix%>%group_by(category_id) %>% arrange(desc(category_id))%>% 
  ggplot(aes(category_id, views, fill=category_id)) + 
  geom_boxplot() +  
  scale_y_log10()+#adjust the format on the y-axis
  xlab('Category Id')+
  ylab('Number of Views')+
  ggtitle("Views per Category on Trending Page")
#theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
#shows that talk shows do not have more views than other categories


#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
##Connect Category with Number
library(rjson)
US_category_id = fromJSON(file = "data/US_category_id.json")
us_trend_vids <- read_delim("data/USvideos.csv", 
                            delim = ",",
                            col_types = cols(trending_date = col_date(format = "%y.%d.%m")))

US_cateogry_id1 = as.data.frame(US_category_id$items) %>% 
  gather("key", "val")
a = as.data.frame(str_subset(US_cateogry_id1$key, "^(id|snippet.title)")) %>% 
  rename(key = `str_subset(US_cateogry_id1$key, "^(id|snippet.title)")`)

b = left_join(a, US_cateogry_id1, by = "key")

id_to_category = b %>% mutate(key = str_extract(b$key, "^(id|snippet.title)")) %>% 
  mutate(index = rep(1:(nrow(b)/2), each = 2)) %>% 
  spread(key, val) %>% 
  select(id, snippet.title) %>% 
  rename(category = snippet.title) %>% 
  arrange(id) %>% 
  mutate(id = as.integer(id))

final = left_join(us_trend_vids, id_to_category, by = c("category_id" = "id"))
final
#------------------------------------------------------------------
#------------------------------------------------------------------
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
timesTrending %>% 
  ggplot(aes(channel_title,n,fill=channel_title))+
  geom_histogram(stat="identity")+
  xlab('TalkShow Title')+
  ylab('Trending Count')+
  ggtitle("Count of Total Times TalkShow has trended")


#Finding channels to be most featured on trending page
topChannels = USvideos %>% group_by(channel_title) %>% count(channel_title)  %>% arrange(desc(n))  %>% head(10)
topChannels
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
topChannels  %>% 
  ggplot(aes(channel_title,n,fill=channel_title))+
  geom_histogram(stat="identity")+
  xlab('Channel Title')+
  ylab('Trending Count')+
  ggtitle("Top 10 Trending Channels")



#lets take a look at what category usually gets the most views
categoryFix%>%group_by(category) %>% arrange(desc(category))%>% 
  ggplot(aes(category, views, fill=category)) + 
  geom_boxplot() +  
  scale_y_log10()+#adjust the format on the y-axis
  xlab('Category')+
  ylab('Number of Views')+
  ggtitle("Views per Category on Trending Page")+
  theme(legend.position = 'right', axis.text.x=element_text(angle=45, hjust=1, size = 8)) 


#theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8))+
#shows that talk shows do not have more views than other categories
#categoryFix = USvideos %>% mutate(category_id = as.character(category_id))#because continous
topChannelsAverageTrending= final %>% group_by(category) %>% summarise(TrendingAmount = n(), average = sum(views)/n()) %>%
    arrange(desc(TrendingAmount)) %>% top_n(100,TrendingAmount)
#top thirty channels based on trending amount not by average views

#First plot to show entertainment 
ggplot(topChannelsAverageTrending,aes(TrendingAmount, average, color = category)) +
  geom_point()+
  #theme(legend.position = 'none')+
  scale_y_log10()

#Show all channels in enterainment category
viewsofEntertainment = final %>% 
  filter(category == 'Entertainment') %>% 
  group_by(channel_title) %>%
  summarise(sumViews = sum(views), trendingAmount = n()) %>%
  top_n(50)
viewsofEntertainment %>% ggplot(aes(trendingAmount,sumViews,color = channel_title))+
  geom_point()+
  xlab('Total Views')+
  ylab('Trending Count')+
  ggtitle("Total Views versus Trending Count of Top 50 Channels in Entertainment Category")+
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8)) 
  

talk_show_list = c("Jimmy Kimmel Live",
                   
                   "The Tonight Show Starring Jimmy Fallon",
                   
                   "TheEllenShow",
                   
                   "The Late Late Show with James Corden",
                   
                   "Team Coco",
                   
                   "The Late Show with Stephen Colbert",
                   
                   "Late Night with Seth Meyers")

viewsofEntertainment = final %>% 
  filter(category == 'Entertainment') %>% 
  group_by(channel_title) %>%
  summarise(sumViews = sum(views), trendingAmount = n()) %>%
  mutate(isTalkShow = channel_title %in% talk_show_list) %>% 
  top_n(50)

viewsofEntertainment %>% ggplot(aes(trendingAmount,sumViews,color = isTalkShow))+
  geom_point()+
  xlab('Total Views')+
  ylab('Trending Count')+
  ggtitle("Talkshow Channels")
  theme(legend.position = 'none', axis.text.x=element_text(angle=45, hjust=1, size = 8)) +
  geom_text()


