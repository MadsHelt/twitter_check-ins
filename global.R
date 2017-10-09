library(shiny)
library(shinythemes)
library(d3heatmap)
library(leaflet.extras)
library(leaflet)
library(plotly)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(quantmod)


# Tokyo data frame
tokyo = read.csv("Data/tokyo.csv", stringsAsFactors = FALSE, 
               colClasses = c("integer", "character", "character", "character", "numeric", 
                              "numeric", "integer", "integer", "numeric", "character",
                              "POSIXct", "Date", "character", "character", "integer", "character"))
# Removing column 4
tokyo = tokyo[-4]

# New variable returning year and month
tokyo$timeseries = format(as.Date(tokyo$date, format = "%Y-%m-%d"), "%Y-%m")

# Setting weekday variable to factor and specify order of the levels
#weekday$weekday <- factor(weekday$weekday,
#                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
#                                     "Friday", "Saturday", "Sunday"))


### Below data is used in the EDA tab
tokyo.venues <- tokyo %>% 
  group_by(venuecategory, timeofday) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))

tokyo.top50 <-tokyo %>% 
  group_by(venuecategory) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(50) 

top30 = tokyo.venues[tokyo.venues$venuecategory %in% tokyo.top50[1:30 ,]$venuecategory, ]

filtered = tokyo.venues[tokyo.venues$venuecategory %in% tokyo.top50[3:30 ,]$venuecategory, ]

tkdf = tokyo %>%
  group_by(., timeseries) %>%
  summarise(., check_in = n())

tkdf$change = Delt(tkdf$check_in) 

tkdf2 = tokyo %>%
  group_by(., timeseries, sex) %>%
  summarise(., check_in = n()) %>%
  group_by(., sex) 

tkdf3 = tokyo %>%
  group_by(., timeseries, popularity) %>%
  summarise(., check_in = n()) %>%
  group_by(., popularity)

popdf = tkdf3 %>%
  filter(., popularity == 'Popular')

popdf1 = tkdf3 %>%
  filter(., timeseries == '2012-12')

max <- tkdf3[which.max(tkdf3$check_in), ]
min <- popdf[which.min(popdf$check_in), ]
maxv <- popdf1[which.max(popdf1$check_in), ]

a <- list(
  x = max$timeseries,
  y = max$check_in,
  text = "Summer peak",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40)

b <- list(
  x = min$timeseries,
  y = min$check_in,
  text = "Summer hangover",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40)

c <- list(
  x = maxv$timeseries,
  y = maxv$check_in,
  text = "Winter peak",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40)

# Numbers to plot 
may <- list(
  x = tkdf$timeseries[2],
  y = 19500,
  xref = 'x',
  yref = 'y',
  text = "17%",
  showarrow = FALSE,
  font = list(size = 12))

june <- list(
  x = tkdf$timeseries[3],
  y = 14000,
  xref = 'x',
  yref = 'y',
  text = "-29%",
  showarrow = FALSE,
  font = list(size = 12))

july <- list(
  x = tkdf$timeseries[4],
  y = 14200,
  xref = 'x',
  yref = 'y',
  text = "1%",
  showarrow = FALSE,
  font = list(size = 12))

aug <- list(
  x = tkdf$timeseries[5],
  y = 6800,
  xref = 'x',
  yref = 'y',
  text = "-53%",
  showarrow = FALSE,
  font = list(size = 12))

sep <- list(
  x = tkdf$timeseries[6],
  y = 2500,
  xref = 'x',
  yref = 'y',
  text = "-67%",
  showarrow = FALSE,
  font = list(size = 12))

oct <- list(
  x = tkdf$timeseries[7],
  y = 8200,
  xref = 'x',
  yref = 'y',
  text = "273%",
  showarrow = FALSE,
  font = list(size = 12))

nov <- list(
  x = tkdf$timeseries[8],
  y = 12000,
  xref = 'x',
  yref = 'y',
  text = "47%",
  showarrow = FALSE,
  font = list(size = 12))

dec <- list(
  x = tkdf$timeseries[9],
  y = 13000,
  xref = 'x',
  yref = 'y',
  text = "8%",
  showarrow = FALSE,
  font = list(size = 12))

jan <- list(
  x = tkdf$timeseries[10],
  y = 12000,
  xref = 'x',
  yref = 'y',
  text = "-8%",
  showarrow = FALSE,
  font = list(size = 12))

feb <- list(
  x = tkdf$timeseries[11],
  y = 6500,
  xref = 'x',
  yref = 'y',
  text = "-48%",
  showarrow = FALSE,
  font = list(size = 12))

# Access to mapbox via token
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibWFkc2hlbHQiLCJhIjoiY2o4Y2FtM3NlMDg3cTJxbGJmeDU3b2oxNSJ9.kSX5pIZxdDpHsXl4TB4OXg')




