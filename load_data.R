library(dplyr)

all <- read.csv("repdata_data_StormData.csv.bz2")

all$STATE <- as.character(all$STATE)
all$COUNTYNAME <- as.character(all$COUNTYNAME)
all$PROPDMGEXP <- as.character(all$PROPDMGEXP)
all$CROPDMGEXP <- as.character(all$CROPDMGEXP)


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
          
clean2 <- filter(clean1,FATALITIES>0 | INJURIES>0 | PROPDMG>0 | CROPDMG>0) %>%
          mutate(ECONLOSS = mapply(econ.loss,PROPDMG,PROPDMGEXP)+
                            mapply(econ.loss,CROPDMG,CROPDMGEXP))


# 
# summer <- group_by(clean2,EVTYPE) %>%
#           summarise(critter =sum(ECONLOSS)) %>% 
#           arrange(desc(critter))