library(dplyr)
library(stringr)

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
           "DROUGHT" = "Drought",
           "DUST DEVIL" = "Dust",
           "DUST STORM" = "Dust",
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

