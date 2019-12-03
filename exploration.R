##Youtube Trending Data
library(tidyverse)
us_trend_vids <- read_delim("data/USvideos.csv", 
                            delim = ",",
                            col_types = cols(trending_date = col_date(format = "%y.%d.%m")))
#view(problems(us_trend_vids)) Data seems to be read fine, problems coming from vertical bar (|) in tags

##Connect Category with Number
library(rjson)
US_category_id = fromJSON(file = "data/US_category_id.json")

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


