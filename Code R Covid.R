

library(jsonlite)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(httr)
library(purrr)


###dataset

download.file("https://pkgstore.datahub.io/core/covid-19/countries-aggregated/archive/e887d0051c6718184bbd766c9cd86f57/countries-aggregated.csv", "datos_treemap.txt", "curl")
df<-read.csv("datos_treemap.txt", sep=",", header=T)
class(df) #[1] "data.frame"

coronaDataSimple <-df %>%
  mutate(ConfirmedLog = log(Confirmed,10)
  ) 
coronaDataSimple2<-subset(coronaDataSimple,
                          Country=='Argentina' |
                            Country== 'Chile' |
                            Country=='Colombia'|
                            Country=='Peru'|
                            Country=='Ecuador'
                          | Country=='Mexico'
                          |Country=='Brazil'
                          |Country=='US'
                          |Country=='Dominican Republic'
                          |Country=='Spain'
                          |Country=='China'
                          |Country=='Korea, South'
                          |Country=='Japan'
)

coronaDataSimple$Date<-as.Date(coronaDataSimple$Date, tryFormats = c("%Y-%m-%d"))

coronaDataSimple3 <- coronaDataSimple2 %>% 
  group_by(Country,Date) %>% 
  summarize(Confirmed = sum(Confirmed),ConfirmedLog=sum(ConfirmedLog))

coronaDataSimple3$Date<-as.Date(coronaDataSimple3$Date, tryFormats = c("%Y-%m-%d"))


coronaDataSimpleChiAr<-subset(coronaDataSimple3,Country=='Peru'|Country=='Colombia'|Country=='Argentina'|Country=='Mexico'|Country=='Dominican Republic'|Country=='Japan'|Country=='Korea, South') 
coronaDataSimpleBr<-subset(coronaDataSimple3,Country=='Peru'|Country=='Brazil'|Country=='Chile'|Country=='Ecuador'| Country=='Spain'| Country=='China'| Country=='US')

## Plots

myCaption <- " Perú vs Latinoamerica. Japon y Corea del Sur"
coronaPlot0 <- coronaDataSimpleChiAr %>% 
  ggplot(aes(x=Date)) +
  geom_line(aes(y=ConfirmedLog, color = Country)) +
  geom_line(mappling=aes(x = as.Date('2020-03-19',tryFormats = c("%Y-%m-%d"))),y=log(234,10))+ #Quarantine date in Peru
  #geom_line(aes(y=Confirmed, color = Country)) 
  labs(caption = myCaption)
coronaPlot0


myCaption <- " Perú vs America,España,China"
coronaPlot0 <- coronaDataSimpleBr %>% 
  ggplot(aes(x=Date)) +
  geom_line(aes(y=ConfirmedLog, color = Country)) +
  geom_line(mappling=aes(x = as.Date('2020-03-19',tryFormats = c("%Y-%m-%d"))),y=log(234,10))+ #Quarantine date in Peru
  labs(caption = myCaption)
coronaPlot0
