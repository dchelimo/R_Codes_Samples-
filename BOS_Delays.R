# David Chelimo 
# February 21, 2017

# Load Required Packages 
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)

# Load data from couse website
dat <- read.csv("BOS_Dec_2014.csv") 
df <- read.csv("http://mgimond.github.io/ES218/Data/BOS_2014.csv")

## Omit rows with NA as value for any of the column
## Filter to retain the month of December and for flights originating from Boston 
## Add date variables to the data:
#       DepDate (scheduled departure date/time), 
#       DepDateAct (actual departure date/time), 
#       ArrDate (scheduled arrival date/time), 
#       ArrDateAct (actual arrival date/time).
#       DelayDep (time Delay)
#       Weekday (the day of week variable)
#       Hour (the delay departure hour variable)
## Group and summarise by Weekday and Hour
## create a "wide" table that lists the median delay time (in minutes) by day-of-week and time-of-day
df1 <- df %>% mutate(
                CANCELLED = as.logical(CANCELLED),
                MonPad    = str_pad(MONTH, width=2, pad="0"),
                DayPad    = str_pad(DAY_OF_MONTH, width=2, pad="0"),
                TimePad   = str_pad(CRS_DEP_TIME, width=4, pad="0"),
                DepDt     = ymd_hm( paste(YEAR, MonPad, DayPad, TimePad)),
                Weekday   = wday(DepDt, label=TRUE),
                Hour      = hour(DepDt))%>% 
              filter( !CANCELLED,
                      ORIGIN == "BOS",
                      month(DepDt) == 12) %>% 
              group_by(Weekday, Hour) %>% 
              summarise(median_delay = median(DEP_DELAY, na.rm=TRUE)) %>% 
              spread(key = Hour, value = median_delay)

df1
