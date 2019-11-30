##Youtube Trending Data
library(tidyverse)
us_trend_vids <- read_delim("data/USvideos.csv", 
                            delim = ",",
                            col_types = cols(trending_date = col_date(format = "%y.%d.%m")))

view(problems(us_trend_vids))
