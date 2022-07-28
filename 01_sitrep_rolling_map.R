##################################################
###########SURVEILLANCE MAPPING CODE##################
############################################
##Created by: Goksel
##Created on: May 12, 2021
##Purpose: to develop LHA/HSDA based maps
##Modified by:
##Modified reasons:
#########################################################

library(tidyverse)
library(sf)
library(bcmaps)
library(tmap)




fn_lha_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/Health Surveillance - General/Population files/FNCF Population/2018 FNCF/FNCF_2018_population_by_LHA.csv")

fn_lha_pop <- fn_lha_pop %>% select(-X1) %>% 
  mutate(LOCAL_HLTH_AREA_CODE=as.character(lha_id_2018))

############

####################################################
###One yr rolling events 
oneyrroll <- seq(max(bcehs2$dateofevent_floor), by="-1 month", length=12)

fn_events <- bcehs2 %>% 
  filter(FN_FLAG == 1 & dateofevent_floor%in% oneyrroll) %>% 
  group_by(DERIVED_LHA) %>% 
  summarise(total_ods = n()) %>%  
  mutate(LOCAL_HLTH_AREA_CODE=as.character(DERIVED_LHA))



####################################################
### One year rolling map
fn_lha_pop_b <- fn_lha_pop %>% filter(year == 2019)


fn_lha_pop_b <- fn_lha_pop_b %>% select(-LOCAL_HLTH_AREA_CODE)
fn_rates_roll <-  fn_events %>% rename(lha_id_2018 = DERIVED_LHA) %>% 
  left_join(fn_lha_pop_b, by = "lha_id_2018") %>% 
  mutate(rate_per_1000_roll = round(1000*total_ods/population, digits=1))

ggplot(fn_rates_roll, aes(rate_per_1000_roll)) + 
  geom_histogram()

od_shape_roll_rates <- health_lha() %>% 
  left_join(fn_rates_roll, by="LOCAL_HLTH_AREA_CODE") %>% 
  mutate(supp_rate_roll_per_1000 = ifelse(is.na(total_ods), 0, 
                                          ifelse(total_ods <11, NA, 
                                                 rate_per_1000_roll)), 
         rank = ifelse(is.na(supp_rate_roll_per_1000), NA, 
                       rank(desc(supp_rate_roll_per_1000))), 
         map_label = ifelse(rank<7, LOCAL_HLTH_AREA_NAME, NA))

tm_shape(od_shape_roll_rates) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1, 200),
              labels = c("No events Reported", "<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title= "Rate per 1,000, Past 1 year rolling") +
  tm_layout(legend.text.size = 0.57, 
            legend.outside=T, 
            main.title =  "Drug Poisoning Event Rates per 1,000, Rolling 1 year", 
            title = "August 2020 - July 2021") #+ 
# tm_text("map_label",  remove.overlap = T, shadow = T, overwrite.lines = T, auto.placement = T, size=0.6)

#tmap_save(filename = here("July", "bc_aug20_july21.jpeg"), width=9, height=7)



vch_fraser <- od_shape_roll_rates %>% 
  filter(HLTH_SERVICE_DLVR_AREA_CODE %in% c(32, 31, 23) | lha_id_2018 %in% c(212, 213))

tm_shape(vch_fraser) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1, 200),
              labels = c("No events reported","<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title = "Rate per 1,000, 2020") + 
  #tm_text("LOCAL_HLTH_AREA_NAME") + 
  tm_layout(legend.text.size = 1, 
            #legend.position = c("right", "top"),
            legend.outside=T)

#tmap_save(filename = here(params$current_data_month, "vchfr_aug20_jul21.jpeg"), width=7, height=4)


fraser_map <- od_shape_roll_rates %>% filter(HLTH_AUTHORITY_CODE %in% c(2))

tm_shape(fraser_map) + 
  tm_polygons("supp_rate_roll_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1,  200),
              labels = c("No events Reported", "<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title = "Rate per 1,000, Past 1 year rolling") + 
  tm_text("LOCAL_HLTH_AREA_NAME") + 
  tm_layout(legend.text.size = 1, 
            #legend.position = c("right", "top"),
            legend.outside=T, 
            main.title = "Drug Poisoning Event Rates per 1,000, First Nations, Fraser, 
            July 1, 2020-June 30, 2021")

