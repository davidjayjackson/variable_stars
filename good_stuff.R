library(flexdashboard)
library(rsconnect)
library(tidyverse)
library(insol)
library(plotly)
library(data.table)
library(lubridate)
library(DT)

mydata <- fread("./star-data.csv")
mydata <- janitor::clean_names(mydata)
mydata$star_name <- toupper(mydata$star_name)
mydata$Ymd <- JD(mydata$jd,inverse = TRUE)
mydata$Ymd <- as.Date(mydata$Ymd)
mydata$Year <- year(mydata$Ymd)
mydata$Month <- month(mydata$Ymd)
mydata <- mydata %>% select(star_name,Ymd,Year,Month,band,magnitude)
                       
plot_ly(data = data[which(data$supp == 'OJ'),], x = ~dose, y = ~length, type = 'bar', name = 'OJ',
               error_y = ~list(array = sd,
                               color = '#000000'))                      


##

just.stats <- mydata %>% group_by(Month=floor_date(Ymd, "month")) %>%
  summarize( Mean = round(mean(magnitude),digits=1),
             Brighter = round(Mean -1,digits = 1),
             Fainter = round(Mean +1,digits= 1))

just.stats %>% 
  if_else(Brighter < Mean -1 | Dimmer > Mean +1,"Yes","No")

just.stats$Verify <- ifelse(just.stats$Brighter < just.stats$Mean -1 | just.stats$Fainter > Mean +1,"Yes","No")

just.stats %>% filter(Month >="2019-01-01") %>% plot_ly %>% add_lines(x=~Month,y=~Mean,name="Mean") %>%
  add_lines(x=~Month,y=~Brighter,name="Brighter") %>%
  add_lines(x=~Month,y=~Fainter,name="Fainter")



just.stats <- mydata %>% 
  group_by(Month=floor_date(Ymd, "month")) %>%
  summarize(Mean     = round(mean(magnitude),digits=1),
            Brighter = round(Mean -1,digits = 1),
            Fainter  = round(Mean +1,digits= 1))

just.df <- just.stats %>%
  mutate(Verify = case_when(
        (Brighter <  Mean -1) | (Fainter > Mean + 1) ~ "Yes",TRUE ~ "No")) %>%
        filter(Verify =="Yes")
  
