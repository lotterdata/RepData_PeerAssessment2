library(dplyr)
library(stringr)
library(ggplot2)

all <- read.csv("repdata_data_StormData.csv.bz2")

all$STATE <- as.character(all$STATE)
all$COUNTYNAME <- as.character(all$COUNTYNAME)
all$PROPDMGEXP <- as.character(all$PROPDMGEXP)
all$CROPDMGEXP <- as.character(all$CROPDMGEXP)
all$BGN_DATE <- as.character(all$BGN_DATE)
all$EVTYPE <- as.character(all$EVTYPE)

clean1 <- select(all,STATE__,BGN_DATE,COUNTY,COUNTYNAME,STATE,EVTYPE,
                 FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) %>%
          filter(STATE__ < 60) %>%
          filter(STATE != "ST") %>%
          filter(!(STATE__==26 & STATE == "MA")) %>%
          filter(!(STATE__==46 & STATE == "SC")) %>%
          filter(!(STATE__==39 & STATE == "ND")) %>%
          filter(!(STATE__==11 & STATE == "MD")) %>%
          filter(!(STATE__==24 & STATE == "OH")) %>%
          filter(!(STATE__==35 & STATE == "NJ")) 

econ.loss <- function(x,y){
    x*
    switch(as.character(y),
           "h" = 100,
           "H" = 100,
           "k" = 1000,
           "K" = 1000,
           "m" = 1000000,
           "M" = 1000000,
           "b" = 1000000000,
           "B" = 1000000000,
           1)
}
  
extract.year <- function(x){
    str_sub(x,
            start=str_locate_all(x,"/")[[1]][2,1]+1,
            end=str_locate_all(x,"/")[[1]][2,1]+4)
}

clean2 <- filter(clean1,FATALITIES>0 | INJURIES>0 | PROPDMG>0 | CROPDMG>0) %>%
          mutate(ECONLOSS = mapply(econ.loss,PROPDMG,PROPDMGEXP)+
                            mapply(econ.loss,CROPDMG,CROPDMGEXP)) %>%
          mutate(YEAR = as.numeric(sapply(BGN_DATE,extract.year))) %>%
          filter(YEAR > 2007)
            #1993 another possible cutoff


event.class <- function(x){
    switch(as.character(x),
           "ASTRONOMICAL LOW TIDE" = "Other",
           "AVALANCHE" = "Winter",
           "BLIZZARD" = "Winter",
           "COASTAL FLOOD" = "Flood",
           "COLD/WIND CHILL" = "Winter",
           "DENSE FOG" = "Other",
           "DROUGHT" = "Other",
           "DUST DEVIL" = "Other",
           "DUST STORM" = "Other",
           "EXCESSIVE HEAT" = "Heat",
           "EXTREME COLD/WIND CHILL" = "Winter",
           "FLASH FLOOD" = "Flood",
           "FLOOD" = "Flood",
           "FREEZING FOG" = "Winter",
           "FROST/FREEZE" = "Winter",
           "FUNNEL CLOUD" = "Tornado",
           "HAIL" = "Hail",
           "HEAT" = "Heat",
           "HEAVY RAIN" = "Other",
           "HEAVY SNOW" = "Winter",
           "HIGH SURF" = "Other",
           "HIGH WIND" = "Wind",
           "HURRICANE" = "Hurricane-Tropical",
           "ICE STORM" = "Winter",
           "LAKE-EFFECT SNOW" = "Winter",
           "LAKESHORE FLOOD" = "Flood",
           "LANDSLIDE" = "Other",
           "LIGHTNING" = "Thunderstorm",
           "RIP CURRENT" = "Other",
           "SEICHE" = "Other",
           "STORM SURGE/TIDE" = "Other",
           "STRONG WIND" = "Wind",
           "THUNDERSTORM WIND" = "Thunderstorm",
           "TORNADO" = "Tornado",
           "TROPICAL DEPRESSION" = "Hurricane-Tropical",
           "TROPICAL STORM" = "Hurricane-Tropical",
           "TSUNAMI" = "Other",
           "WILDFIRE" = "Other",
           "WINTER STORM" = "Winter",
           "WINTER WEATHER" = "Winter",
           "Other"
           )
}

clean2$EVCLASS <- sapply(clean2$EVTYPE,event.class)

totals <- group_by(clean2,STATE) %>%
          summarise(tot.fatal = sum(FATALITIES), tot.econ = sum(ECONLOSS))

subtotals <- group_by(clean2,STATE,EVCLASS) %>%
             summarise(tot.fatal = sum(FATALITIES), tot.econ = sum(ECONLOSS))

plot.data <- inner_join(totals,subtotals,by="STATE") %>%
             mutate(prop.fatal = tot.fatal.y/tot.fatal.x) %>%
             mutate(prop.econ = tot.econ.y/tot.econ.x)


g.fatal <- ggplot(plot.data ,aes(x=STATE,fill=EVCLASS,weight=prop.fatal))

plot.fatal <- g.fatal +
              geom_histogram() +
              coord_flip() +
              ggtitle("Percent of Fatalities in Nine Storm Categories, By State")

print(plot.fatal)


g.econ <- ggplot(plot.data ,aes(x=STATE,fill=EVCLASS,weight=prop.econ))

plot.econ <- g.econ +
             geom_histogram() +
             coord_flip() +
             ggtitle("Percent of Economic Losses in Nine Storm Categories, By State")

print(plot.econ)

