# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Pfizer County Hospitals Potential Project
# Purpose:      Model Result Validation
# programmer:   Xin Huang
# Date:         09-05-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##------------------------------------------------------------------------------
##--                 loading the required packages
##------------------------------------------------------------------------------
options(java.parameters = "-Xmx2048m")
suppressPackageStartupMessages({
  library(openxlsx)
  library(RODBC)
  library(dplyr)
  library(plm)
  library(dynlm)
  library(randomForest)
  library(data.table)
  library(stringi)
  library(stringr)
  library(purrr)
  library(caret)
  library(progress)
})


##-- read in the hospitals with over 100 beds count 
hosp_potential <-
  read.xlsx("02_Inputs/江苏城市医院/Norvasc_hospital_potential_updated.xlsx",
            sheet = "Out")

prj_pha_mapping_code <- read.xlsx("02_Inputs/江苏城市医院/prj_pha_mapping.xlsx")

hosp_potential_js <- hosp_potential %>%
  filter(Province == "江苏") 

total_potential <- sum(hosp_potential_js$`2017潜力`, na.rm = TRUE)

hosp_potential_js_m <- hosp_potential_js %>%
  left_join(prj_pha_mapping_code, by = "prj_code") 


moh_data <- read.xlsx("02_Inputs/20180824最新MOH新增修改和补充_补充江苏.xlsx",
                      sheet = "MOH")


moh_data_js <- moh_data %>%
  filter(Province == "江苏" & 床位数 >= 100) %>%
  select(PHA.ID, PHAHospname, 床位数) %>%
  distinct() %>%
  filter(!is.na(PHA.ID))


hosp_potential_js_m1 <- hosp_potential_js_m %>%
  filter(PHA_ID %in% unique(moh_data_js$PHA.ID)) %>%
  group_by(City) %>%
  summarise(`2017潜力` = sum(`2017潜力`, na.rm = TRUE)) %>%
  arrange(desc(`2017潜力`)) %>%
  mutate(hosp_rank = row_number())

##-- county hospitals summary

## city rank of hospitals potential and county hospital potential

final_data_chk <- final_data %>%
  group_by(city) %>%
  summarise(value_prj = sum(value_prj, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(value_prj)) %>%
  mutate(county_hosp_rank = row_number())

hosp_potential_js_m2 <- hosp_potential_js_m1 %>%
  inner_join(final_data_chk, by = c("City" = "city"))


## county rank and comparison 
final_data_chk1 <- final_data %>%
  group_by(district, gdp_value_100mil, permanent_residents_10thousand,
           urban_disposal_income, rural_disposal_income, social_consumption_100mil,
           retailotc_10thousand) %>%
  summarise(value_prj = sum(value_prj, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(value_prj)) %>%
  mutate(county_rank = row_number())

correlation <- cor(final_data_chk1[, -c(1, 9)])
corrplot(correlation)







  



