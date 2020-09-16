
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
  library(data.table)
  library(stringi)
  library(stringr)
  library(purrr)
  library(tidyr)
})

##------------------------------------------------------------------------------
##--              growth rate calculating and matching
##------------------------------------------------------------------------------

biding_data_2017_js <- read.xlsx("02_Inputs/js项目辉瑞41个品种-201809.xlsx")

sample_data_out <- county_sample_data_js %>%
  select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
            county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) %>%
  mutate(医生数 = as.numeric(医生数)) %>%
  select(-c(Province, City, Hospital_Code, Hospital_Categroy_I, province, city,
            Hospital_Categroy_II,Hospital_Categroy_III, Hospital_Categroy_IV,
            `Re-Speialty`)) %>%
  filter(!(hospital_name %in% continuty_chk$hospital_name) | is.na(Est_DrugIncome_RMB))

hospital_matching <- county_sample_data_js %>%
  filter(!(hospital_name == "徐州市睢宁县人民医院" & is.na(医生数))) %>%
  filter(hospital_name %in% biding_data_2017_js$医院名称)

hospital_not_matching <- county_sample_data_js %>%
  filter(!(hospital_name == "徐州市睢宁县人民医院" & is.na(医生数))) %>%
  filter(!(hospital_name %in% biding_data_2017_js$医院名称))

# hospital_matching_1 <- biding_data_2017_js %>%
#   filter(医院名称 %in% sample_data_out$hospital_name) %>%
#   select(医院名称) %>%
#   unique()
# 
# hospital_matching_2 <- biding_data_2017_js %>%
#   filter(医院名称 %in% testing_data$hospital_name) %>%
#   select(医院名称) %>%
#   unique()
# 
# hospital_matching_3 <- biding_data_2017_js %>%
#   filter(医院名称 %in% training_data$hospital_name) %>%
#   select(医院名称) %>%
#   unique()
# 
# hospital_matching_11 <- sample_data_out %>%
#   filter(hospital_name %in% biding_data_2017_js$医院名称)
# 
# hospital_matching_22 <- training_data %>%
#   filter(hospital_name %in% biding_data_2017_js$医院名称)
# 
# hospital_matching_33 <- testing_data %>%
#   filter(hospital_name %in% biding_data_2017_js$医院名称)

matching_data <- county_sample_data_js %>%
  select(-c(city, Hospital_Categroy_IV, 新版ID, Region, Prefecture ,
            county.HP, City.Tier.2010, City.Tier.2018)) %>%
  left_join(htn_biding_data_2016_js, 
            by = c("hospital_name")) %>%
  mutate(医生数 = as.numeric(医生数)) %>%
  select(hospital_name, district, value) %>%
  distinct()

biding_data_2017_js_m <- biding_data_2017_js %>%
  select(医院名称, 采购金额) %>%
  rename(hospital_name = 医院名称,
         value = 采购金额) %>%
  group_by(hospital_name) %>%
  summarise(value2017 = sum(value, na.rm = TRUE))

hospital_matching_hosp <- hospital_matching %>%
  select(hospital_name) %>%
  left_join(matching_data, by = c("hospital_name")) %>%
  left_join(biding_data_2017_js_m, by = c("hospital_name")) %>%
  mutate(growth_rate = value2017 / value,
         percentage = growth_rate - 1)

hospital_matching_dist <- hospital_matching_hosp %>%
  group_by(district) %>%
  summarise(value = sum(value),
            value2017 = sum(value2017)) %>%
  mutate(growth_rate = value2017 / value,
         percentage = growth_rate - 1)

testing_data_m1 <- cbind(dum_testing_data$hospital_name, data.frame(dum_testing_data$value))
colnames(testing_data_m1) <- c("hospital_name", "value")

testing_data_2017 <- testing_data_m %>%
  select(hospital_name, district) %>%
  left_join(hospital_matching_dist, by = c("district")) %>%
  select(hospital_name, district, growth_rate, percentage) %>%
  left_join(testing_data_m1, by = c("hospital_name")) %>%
  mutate(value2017 = value * growth_rate)

wb_rate <- createWorkbook()
addWorksheet(wb_rate, "growth_rate_hosp")
addWorksheet(wb_rate, "growth_rate_dist")
addWorksheet(wb_rate, "testing_data_2017")

writeData(wb_rate, "growth_rate_hosp", hospital_matching_hosp)
writeData(wb_rate, "growth_rate_dist", hospital_matching_dist)
writeData(wb_rate, "testing_data_2017", testing_data_2017)

saveWorkbook(wb_rate, "output_0920.xlsx",
             overwrite = TRUE)

##------------------------------------------------------------------------------
##--                        coordinates matching
##------------------------------------------------------------------------------

city_name <- unique(county_sample_data_js$city)

dist_coordinates <- read.xlsx("02_Inputs/江苏省各地级市县经纬度.xlsx") %>%
  mutate(地名 = paste(地名, "市", sep = "")) %>%
  filter(地名 %in% city_name,
         !(地名 == "南通市" & 经度 == 121.05)) %>%
  select(地名, 经度, 纬度) %>%
  rename(city = 地名,
         city_lon = 经度,
         city_lat = 纬度)

hosp_coordinates <- read.xlsx("02_Inputs/Updated_PHA_Location_Add.xlsx", sheet = 2) %>%
  select(新版名称, location) %>%
  separate(location, c("longitude", "latitude"), sep = ",")

hosp_coordinates_m <- read.xlsx("02_Inputs/Updated_PHA_Location_Add.xlsx", sheet = 2) %>%
  select(name, location) %>%
  mutate(name = sub("江苏常州市|江苏淮安市|江苏连云港市|江苏南京市|江苏南通市|江苏苏州市|
                    江苏泰州市|江苏无锡市|江苏宿迁市|江苏徐州市|江苏盐城市|江苏扬州市|
                    江苏镇江市|江苏", "", name)) %>%
  separate(location, c("longitude", "latitude"), sep = ",")

hosp_coordinates_m1 <- read.xlsx("02_Inputs/Updated_PHA_Location_Add.xlsx", sheet = 2) %>%
  select(gaode_name, location) %>%
  mutate(gaode_name = sub("江苏省|江苏", "", gaode_name)) %>%
  separate(location, c("longitude", "latitude"), sep = ",")

hosp_coordinates_m2 <- read.xlsx("02_Inputs/Updated_PHA_Location_Add.xlsx", sheet = 2) %>%
  select(PHAHospname, location) %>%
  separate(location, c("longitude", "latitude"), sep = ",")

hosp_coordinates_m3 <- read.xlsx("02_Inputs/Updated_PHA_Location_Add.xlsx", sheet = 2) %>%
  select(新版ID, location) %>%
  filter(新版ID != 0) %>%
  separate(location, c("longitude", "latitude"), sep = ",")


county_hospitals_universe_js_m1 <- read.xlsx("02_Inputs/江苏-医院&县医院-HX.xlsx", sheet = 3) %>%
  rename(city = City) %>%
  left_join(hosp_coordinates, by = "新版名称") %>%
  left_join(dist_coordinates, by = "city")

county_sample_data_js_m1 <- read.xlsx("02_Inputs/江苏新样本_add1.xlsx") %>%
  left_join(hosp_coordinates, by = c("PHA" = "新版名称")) %>%
  left_join(dist_coordinates, by = c("city")) %>%
  unique()

table(is.na(county_hospitals_universe_js_m1[, c("longitude", "city_lon")]))
table(is.na(county_sample_data_js_m1[, c("longitude", "city_lon")]))

write.xlsx(county_hospitals_universe_js_m1, "02_Inputs/county_hospitals_universe_js_add0930.xlsx")
write.xlsx(county_sample_data_js_m1, "02_Inputs/county_sample_data_js_add0930.xlsx")



chk <- county_sample_data_js_m1 %>%
  filter(is.na(longitude))
table(chk$city)
  
write.xlsx(table(chk$city), "02_Inputs/无法匹配医院的城市分布.xlsx")


















