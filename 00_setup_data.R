##################################################
###Set up BCEHS & BCCS Surveillance Data#########
###Created by: Goksel
###Created on: May 31, 2021 
###Project: Community Sitrep
###Modified by:
###Modified Reasons: 
###################################################

library(tidyverse)
library(knitr)
library(brew)
library(scales)
library(epitools)
library(odbc)
library(dbplyr)
library(lubridate)
library(broom)
library(ggrepel)
library(flextable)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(here)
library(viridis)
library(sf)
library(bcmaps)
library(tmap)

standards <- theme_classic() + 
  theme(axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        legend.text = element_text(size = 15), 
        panel.grid.major = element_blank(), 
        panel.grid.minor=element_blank(),
        axis.title = element_text(size=16),
        legend.title = element_blank())

format_dates <- function(x) {
  months <-  month(x, label = T, abbr=T)            
  years <- lubridate::year(x) 
  
  if_else(is.na(lag(years)) | lag(years) != years,  
          true = paste(years, months, sep = " - "), 
          false = paste(months))
}

format_dates_q <- function(x) {
  quarters <-  paste0("Q", quarter(x))            
  years <- lubridate::year(x) 
  
  if_else(is.na(lag(years)) | lag(years) != years,  
          true = paste(years, quarters, sep = " - "), 
          false = paste(quarters))
}



dbhandle <- dbConnect(odbc(), driver="ODBC Driver 17 for SQL Server", server="FNHADW", database="CHWSDM", Trusted_Connection = "yes")
tester <- tbl(dbhandle, in_schema("dbo", "BCEHS"))

bcehs1 <- as_tibble(tester) %>% 
  mutate(ha_name = ifelse(DERIVED_HA == 1, "Interior", 
                          ifelse(DERIVED_HA == 2, "Fraser", 
                                 ifelse(DERIVED_HA == 3, "Vancouver Coastal", 
                                        ifelse(DERIVED_HA ==4, "Vancouver Island", 
                                               ifelse(DERIVED_HA == 5, "Northern", "Unknown"))))), 
         ha_colour = ifelse(DERIVED_HA == 1, "#F04B52", 
                            ifelse(DERIVED_HA == 2, "#2C97A7", 
                                   ifelse(DERIVED_HA == 3, "#F57E20", 
                                          ifelse(DERIVED_HA ==4, "#00A14B", 
                                                 ifelse(DERIVED_HA == 5, "#8ACAE2", "grey"))))),
         FirstNations = ifelse(FN_FLAG == 1, "first_nations", "other_residents"),
         DATEOFSERV = ymd(DATE_SERVICE),
         month = month(DATE_SERVICE),
         month_lab = month(DATE_SERVICE, label = T),
         dateofevent_floor = floor_date(DATE_SERVICE, unit="month"),
         week=week(DATE_SERVICE),
         year=year(DATE_SERVICE),
         age_group = ifelse(PAT_AGE >=0 & PAT_AGE <20, "<20",
                            ifelse(PAT_AGE >=20  &  PAT_AGE <=29, "20-29",
                                   ifelse(PAT_AGE>=30 &  PAT_AGE<=39, "30-39",
                                          ifelse(PAT_AGE >=40 &  PAT_AGE<= 49, "40-49",
                                                 ifelse(PAT_AGE >=50 &  PAT_AGE<= 59, "50-59",
                                                        ifelse(PAT_AGE >=60 , "60+", "Unknown")))))),
         phn_flag_update = ifelse(PHN_FLAG == "N" & is.na(MATCHED_FLAG)==T, "N",  
                                  ifelse(PHN_FLAG == "N" & MATCHED_FLAG == "1", "Y", "Y")))



bcehs2 <- bcehs1 %>%
  filter(OD_GROUP=="Probable_OD_opioid") %>% 
  arrange(MOH_STUDYID, DATE_SERVICE) %>% 
  filter(PAT_EVENT_INDEX ==1)


bccs_sql <- tbl(dbhandle, in_schema("dbo", "F_BCCS_Illicit_Drug_ODs_FN_Deid"))
bccs <- as_tibble(bccs_sql)

bccs <- bccs %>% 
  #filter(DATEOFDEATH_YEAR > 2015) %>% 
  mutate(DATEOFDEATH = ymd(DATE_OF_DEATH),
         DATEOFDEATH_YEAR = year(DATE_OF_DEATH),
         ha_name = ifelse(FINAL_DERIVED_HA == 1, "Interior", 
                          ifelse(FINAL_DERIVED_HA == 2, "Fraser",
                                 ifelse(FINAL_DERIVED_HA == 3, "Vancouver Coastal",
                                        ifelse(FINAL_DERIVED_HA ==4, "Vancouver Island",
                                               ifelse(FINAL_DERIVED_HA == 5, "Northern", "Unknown"))))),
         FirstNations = ifelse(FNCF_MATCH == 1, "first_nations", "other_residents"),
         #DATEOFDEATH = ymd(DATEOFDEATH),
         month = month(DATEOFDEATH),
         month_lab = month(DATEOFDEATH, label = T, abbr = T),
         dateofdeath_floor = floor_date(DATEOFDEATH, unit="month"),
         quarter = ifelse(month < 4, "Q1",
                          ifelse(month >=4 & month <7, "Q2",
                                 ifelse(month >=7 & month <10, "Q3",
                                        ifelse(month >=10 & month <13, "Q4", NA)))),
         year_quarter = paste(DATEOFDEATH_YEAR, quarter, sep="-"), 
         age_group = ifelse(DECEASED_AGE >= 0 & DECEASED_AGE <20, "<20",
                            ifelse(DECEASED_AGE >=20  &  DECEASED_AGE <=29, "20-29",
                                   ifelse(DECEASED_AGE>=30 &  DECEASED_AGE<=39, "30-39",
                                          ifelse(DECEASED_AGE >=40 &  DECEASED_AGE<= 49, "40-49",
                                                 ifelse(DECEASED_AGE >=50 &  DECEASED_AGE<= 59, "50-59",
                                                        ifelse(DECEASED_AGE >=60 , ">=60", "Unknown")))))), 
#         barwidth = ifelse(DATEOFDEATH_YEAR == params$current_year, params$latest_data_month/14, 12/14)
  ) 

population_file <- read_csv(file="//fnha.local/groups/Health Surveillance/HS Staff - Confidential/Opioid Overdose Crisis/Opioid Quarterly Reports/R_readin_2020.08.05_Population_File.csv")

#SEtup LHA population File
fn_lha_pop <- read_csv(file = "//fnha.local/groups/Health Surveillance/Health Surveillance - General/Population files/FNCF Population/2018 FNCF/FNCF_2018_population_by_LHA.csv")

fn_lha_pop <- fn_lha_pop %>% select(-X1) %>% 
  mutate(LOCAL_HLTH_AREA_CODE=as.character(lha_id_2018))

############

####################################################
###One yr rolling events 

fn_lha_pop_b <- fn_lha_pop %>% filter(year == 2019)

fn_lha_pop_b <- fn_lha_pop_b %>% select(-LOCAL_HLTH_AREA_CODE)

