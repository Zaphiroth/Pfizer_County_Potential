# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Pfizer County Hospitals Potential Project
# Purpose:      Raw Data Process
# programmer:   Xin Huang
# Date:         09-05-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# loading the required packages
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
})


##-- readin the raw sample data
# county_sample_data_js <- read.xlsx("02_Inputs/pfizer_江苏_sample_hx.xlsx")
county_sample_data_js <- read.xlsx("02_Inputs/江苏新样本_add1.xlsx")
table(county_sample_data_js$city)
# 常州市   淮安市 连云港市   南通市   苏州市   泰州市   无锡市   宿迁市   徐州市   盐城市   扬州市 
# 2       12        8      129       21       20       13       58       17       24        9 
# 镇江市 
# 10 

county_sample_data_js <- county_sample_data_js
  # %>%
  # mutate(doc_cnt = as.numeric(`医生数`),
  #        bed_cnt = `床位数`) %>%
  # select(-c(`医生数`, `床位数`))

##-- read in biding data of 2016

biding_data_2016 <- fread("02_Inputs/biding_data_2016.csv", encoding="UTF-8")
biding_data_2016 <- setDF(biding_data_2016)

##-- according to the htn mkt definition as following to filter the data
# C02 ANTIHYPERTENSIVES
# C03 DIURETICS
# C07 BETA BLOCKING AGENTS
# C08 CALCIUM ANTAGONISTS
# C09 RENIN-ANGIOTEN SYST AGENT
# C11A LIPREG.CV.MULT-TH.COMBS


htn_biding_data_2016 <- biding_data_2016 %>%
  filter(ATCII %in% c("β-受体阻滞剂", "钙通道阻滞剂", 
                      "抗高血压药","利尿剂", "作用于肾素-血管紧张素系统的药物") 
         # |
         #   ATCIV == "HMG-CoA还原酶抑制剂和其他调节血脂药的复方"
         ) #葆至能

##-- sample continuty check
continuty_chk <- htn_biding_data_2016 %>%
  filter(hospital_name %in% unique(county_sample_data_js$hospital_name)) %>%
  group_by(hospital_name, month) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(hospital_name, month) %>%
  group_by(hospital_name) %>%
  summarise(month_cnt = n()) %>%
  filter(month_cnt >= 8)


htn_biding_data_2016_js <-
  htn_biding_data_2016 %>%
  group_by(province, city, district, Hospital_Code, hospital_name,
           Hospital_Categroy_I, Hospital_Categroy_II,
           Hospital_Categroy_III, Hospital_Categroy_IV) %>%
  summarise(value = sum(value, na.rm = TRUE),
            unit = sum(unit, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(province == "江苏省")

##-- do some QC
value_chk <- htn_biding_data_2016_js %>%
  filter(Hospital_Categroy_IV == "县级等级") 
sum(value_chk$value)

# htn_biding_data_2016_js_sample <- htn_biding_data_2016_js %>%
#   filter(hospital_name %in% unique(county_sample_data_js$新版名称_m))
# setdiff(unique(county_sample_data_js$新版名称_m),
#         unique(htn_biding_data_2016_js_sample$hospital_name))


htn_county_sample_data_js_m <- county_sample_data_js %>%
  select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
             county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) %>%
  filter(hospital_name %in% continuty_chk$hospital_name) %>%
  mutate(医生数 = as.numeric(医生数)) %>%
  select(-c(Province, City, Hospital_Code, Hospital_Categroy_I, province, city,
            Hospital_Categroy_II,Hospital_Categroy_III, Hospital_Categroy_IV,
            `Re-Speialty`))

htn_county_sample_data_js_m <- htn_county_sample_data_js_m %>%
  filter(!is.na(Est_DrugIncome_RMB))

##-- add more information for the counties
demograph_data <-
  read.xlsx("02_Inputs/data_for_clustering.xlsx", sheet = "county")
colnames(demograph_data) <-
  c("province", "city", "county", "county_code_2015", "gdp_value_100mil",
    "gdp_gr", "permanent_residents_10thousand", 
    "urban_permanent_residents_10thousand",
    "rural_permanent_residents_10thousand",
    "permanent_residents_birth_rate",
    "new_born", "urban_disposal_income", "urban_disposal_income_gr",
    "rural_disposal_income", "rural_disposal_income_gr",
    "social_consumption_100mil",
    "retailotc_10thousand", "sample_vaccine_data")

demograph_data_m <- demograph_data %>%
  select(-c(permanent_residents_birth_rate,
            new_born, sample_vaccine_data))

demograph_data_js <- demograph_data %>% 
  filter(province == "江苏省") %>%
  select(-c(province, city,
            permanent_residents_birth_rate,
            new_born, sample_vaccine_data))


##-- generate the model data based on the sample county hospitals in JS
htn_county_sample_data_js_m1 <- htn_county_sample_data_js_m %>%
  # group_by(新版名称_m,  Hosp_level, Province, City, Prefecture,
  #               Specialty_1, Specialty_2, Est_DrugIncome_RMB, sale,
  #               doc_cnt, bed_cnt, Hospital_Code, Hospital_Categroy_I, 
  #               Hospital_Categroy_II, Hospital_Categroy_III, 
  #               Hospital_Categroy_IV) %>%
  # summarise(value = sum(value, na.rm = TRUE),
  #           unit = sum(unit, na.rm = TRUE)) %>%
  ungroup() %>% 
  left_join(demograph_data_m, by = c("district" = "county")) %>%
  # select(-c(sale, district, Hospital_Code, Hospital_Categroy_I, 
  #               Hospital_Categroy_II, Hospital_Categroy_III, 
  #               Hospital_Categroy_IV)) %>%
  mutate(gdp_gr = as.numeric(gdp_gr),
         # 新版名称 = 新版名称_m,
         flag = 1)

table(htn_county_sample_data_js_m1$city)
# 常州   淮安 连云港   南通   苏州   泰州   无锡   宿迁   徐州   盐城   扬州   镇江 
# 2      4      4     19     14     13      6      6      9     14      6      9  


##-- clean the universe data
county_sample_data_js_tmp <- county_sample_data_js %>%
  mutate(PHA_tmp = ifelse(is.na(PHA), hospital_name, PHA))

county_hospitals_universe_js <-
  read.xlsx("02_Inputs/江苏-医院&县医院-HX.xlsx") %>%
  left_join(demograph_data_m, by = c("Prefecture" = "county")) %>%
  mutate(flag = ifelse(`新版名称` %in% unique(county_sample_data_js_tmp$PHA_tmp), 
         1, 0),
         医生数 = as.numeric(医生数),
         gdp_gr = as.numeric(gdp_gr)) %>%
  select(-c(Province, City, county.HP, City.Tier.2010, City.Tier.2018, Region)) %>%
  rename(hospital_name = 新版名称,
         district = Prefecture)

table(county_hospitals_universe_js$flag)

# do some check
chk <- county_hospitals_universe_js %>%
  filter(is.na(rural_disposal_income)) %>%
  select(district) %>%
  distinct()

chk1 <- county_sample_data_js_tmp %>%
  filter(!(PHA_tmp %in% county_hospitals_universe_js$hospital_name))

chk2 <- county_hospitals_universe_js %>%
  filter(flag == 0)

# wb <- createWorkbook()
# addWorksheet(wb, "matched_sample")
# addWorksheet(wb, "universe_except_ms")
# 
# writeData(wb, "matched_sample", chk1)
# writeData(wb, "universe_except_ms", chk2)
# 
# saveWorkbook(wb, "03_Outputs/qc_sample_universe.xlsx", overwrite = TRUE)

sample_data_not_in_universe <- chk1

##-- filter out the sample data from universe
setdiff(colnames(htn_county_sample_data_js_m1), 
        colnames(county_hospitals_universe_js))

testing_data <- county_hospitals_universe_js %>%
  filter(flag == 0)



universe_data_js <- 
  bind_rows(htn_county_sample_data_js_m1,
            testing_data)
  # %>%
  # select(-c(province, city))






  

  














