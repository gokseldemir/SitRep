#############################
###
####Sitrep BC level map#########
##############################

fn_events <- bcehs2 %>% 
  filter(FN_FLAG == 1) %>% 
  group_by(year, DERIVED_LHA) %>% 
  summarise(total_ods = n()) %>%  
  pivot_wider(names_from = year, 
              values_from = total_ods, 
              #  values_fill = list(total_ods=0)
  ) %>% 
  mutate(LOCAL_HLTH_AREA_CODE=as.character(DERIVED_LHA))


fn_lha_pop <- read_csv(file = "L:/Health Surveillance/Health Surveillance - General/Population files/FNCF Population/2018 FNCF/FNCF_2018_population_by_LHA.csv")

fn_lha_pop <- fn_lha_pop %>% select(-X1) %>% 
  mutate(LOCAL_HLTH_AREA_CODE=as.character(lha_id_2018))


fn_lha_pop_b <- fn_lha_pop %>% filter(year == 2019)

fn_rates <- fn_events %>% rename(lha_id_2018 = DERIVED_LHA) %>% 
  left_join(fn_lha_pop_b, by = "lha_id_2018") %>% 
  mutate(rate = round(1000*`2020`/population, digits=1), 
         supp_rate_20_per_1000 = ifelse(is.na(`2020`), 0, 
                                        ifelse(`2020` <11, NA, 
                                               rate)), 
         supp_n = ifelse(is.na(`2020`), 0 , 
                         ifelse(`2020` <11, NA, `2020`))
         )

ggplot(fn_rates, aes(rate)) + 
  geom_histogram()

od_shape <- health_lha() %>% 
  left_join(fn_events, by="LOCAL_HLTH_AREA_CODE") %>% 
  left_join(fn_lha_pop, by="LOCAL_HLTH_AREA_CODE")


od_shape_rates <- od_shape %>% 
  mutate(od_rate_20_per_1000 = round(1000*`2020`/population, digits=1), 
         supp_rate_20_per_1000 = ifelse(is.na(`2020`), 0, 
                                        ifelse(`2020` <11, NA, 
                                               od_rate_20_per_1000)), 
         od_rate_21_per_1000 = round((1000*`2021`/population)*12/params$month_num, digits=1), 
         supp_rate_21_per_1000 = ifelse(is.na(`2021`), 0, 
                                        ifelse(`2021` <11, NA, 
                                               od_rate_21_per_1000)))

tm_shape(od_shape_rates) + 
  tm_polygons("supp_rate_21_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1, 200),
              labels = c("No events reported", "<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.0-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title= "Rate per 1,000, 2021") +
  tm_layout(legend.text.size = 0.57, 
            legend.outside=T) 

#tmap_save(filename = here("August", "lha_rate_maps_BC_2021_est.jpeg"), width=9, height=7)

vch_fraser <- od_shape_rates %>% 
  filter(HLTH_SERVICE_DLVR_AREA_CODE %in% c(32, 31, 23) | lha_id_2018 %in% c(212, 213))

tm_shape(vch_fraser) + 
  tm_polygons("supp_rate_21_per_1000", 
              style = "fixed", 
              breaks = c(0, 0.1, 10.1, 20.1, 30.1, 40.1, 50.1, 200),
              labels = c("No events reported","<10", "10.1-20.0", "20.1-30.0", "30.1-40.0", "40.1-50.0", ">50.0"),
              textNA = "Suppressed(n<11)", 
              title = "Rate per 1,000, 2021") + 
  # tm_text("LOCAL_HLTH_AREA_NAME", remove.overlap=T) + 
  tm_layout(legend.text.size = 1, 
            #legend.position = c("right", "top"),
            legend.outside=T)

#tmap_save(filename = here("August", "lha_rate_maps_vchfr_2021_est.jpeg"), width=6, height=4)
