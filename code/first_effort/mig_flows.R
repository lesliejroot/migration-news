library(tidyverse)
library(narcan)
library(geofacet)

d <- read_csv("~/Downloads/usa_00043.csv")
unique(d$YEAR)

mig_flow <- d %>% filter(MIGRATE1==4, 
                         BPL>150) %>% 
  group_by(STATEFIP, YEAR) %>% 
  summarise(n = sum(PERWT)) 

  
# read in codes to get the names
mig_flow$state_fip <- ifelse(mig_flow$STATEFIP<10, 
                                 paste0("0", mig_flow$STATEFIP), 
                                 as.character(mig_flow$STATEFIP))

rep_pattern <- narcan::st_fips_map$name
names(rep_pattern) <- narcan::st_fips_map$fips
mig_flow$state <- stringr::str_replace_all(mig_flow$state_fip, pattern = rep_pattern)

mig_flow %>% 
  ggplot(aes(YEAR, n)) + geom_line() + geom_point() + facet_geo(~state)


d %>% 
  group_by(STATEFIP, YEAR) %>% 
  summarise(pop = sum(PERWT)) %>% 
  left_join(mig_flow) %>% 
  mutate(prop = n/pop) %>% 
  ggplot(aes(YEAR, prop)) + geom_line() + geom_point() + facet_geo(~state)
ggsave("./plots/state_mig_flows.pdf", width = 10, height = 8)
