library(tidyverse)
library(knitr)
library(gt)

 poll <- read.csv("ps_4_elections-poll-nc09-3.csv")

 table <- poll %>% 
   select(race_eth, response, final_weight) %>% 
   filter(race_eth !=  "[DO NOT READ] Don't know/Refused") %>% 
   filter(response %in% c("Dem", "Rep", "Und")) %>% 
   group_by(race_eth, response) %>% 
   summarize(total = sum(final_weight)) %>% 
   spread(key = response, value = total) %>% 
   replace_na(list(Und = 0)) %>% 
   mutate(all = Dem + Rep + Und) %>% 
   mutate(Dem = Dem / all) %>% 
   mutate(Rep = Rep / all) %>% 
   mutate(Und = Und / all) %>% 
   select(-all) %>% 
   arrange(order(c("White","Black","Hispanic","Asian","Other")))
 
table %>% 
   gt() %>%
   tab_header(title = "Voting by Race in North Carolina’s 9th Congressional District") %>% 
   cols_label( Dem = "DEM.",
    Rep = "REP.",
    Und = "UND.") %>%
   fmt_percent(columns = c(DEM., REP., UND), decimals = 0)
 