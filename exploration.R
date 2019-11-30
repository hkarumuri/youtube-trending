##Youtube Trending Data
library(tidyverse)
us_trend_vids <- read_delim("data/USvideos.csv", 
                            delim = ",",
                            col_types = cols(trending_date = col_date(format = "%y.%d.%m")))
#view(problems(us_trend_vids)) Data seems to be read fine, problems coming from vertical bar (|) in tags

##Connect Category with Number
library(rjson)
us_category_id = fromJSON("data/US_category_id.json")

