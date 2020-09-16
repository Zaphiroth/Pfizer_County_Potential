
##------------------------------------------------------------------------------
##--                 Loading the required packages
##------------------------------------------------------------------------------

options(java.parameters = "-Xmx2048m")
suppressPackageStartupMessages({
  library(openxlsx)
  library(RODBC)
  library(dplyr)
  library(tidyr)
  library(plm)
  library(dynlm)
  library(randomForest)
  library(data.table)
  library(stringi)
  library(stringr)
  library(purrr)
  library(cluster)
  library(ClusterR)
  library(factoextra)
  library(ggplot2)
  library(NbClust)
  library(fpc)
  library(dbscan)
})


##------------------------------------------------------------------------------
##--                 Loading data
##------------------------------------------------------------------------------

## City indice

clustering_data_indice <- read.xlsx("02_Inputs/data_for_clustering.xlsx", sheet = 2, colNames = TRUE)

colnames(clustering_data_indice) <- c("省", "地级市", "区/县(县级市）", "2015年行政区划编码", "GDP总值(亿元)", 
                                      "GDP增长率（%）", "常住人口(万人)", "常住城镇人口(万人)", "常住乡村人口(万人)", 
                                      "常住人口出生率(‰)", "新生儿数", "城镇居民人均可支配收入（元）", 
                                      "城镇居民人均可支配收入增长率（%）", "农民人均可支配收入（元）", 
                                      "农民人均可支配收入增长率（%）", "SocialConsumption.(亿元)", "RetailOTC.(万元）", 
                                      "SampleVaccineData")

clustering_data_indice_m <- clustering_data_indice %>%
  filter(省 == "江苏省") %>%
  dplyr::select("省", "地级市", "区/县(县级市）", "2015年行政区划编码", "GDP总值(亿元)", "GDP增长率（%）", 
                "常住人口(万人)", "常住城镇人口(万人)", "常住乡村人口(万人)", "城镇居民人均可支配收入（元）", 
                "城镇居民人均可支配收入增长率（%）", "农民人均可支配收入（元）", "农民人均可支配收入增长率（%）", 
                "SocialConsumption.(亿元)", "RetailOTC.(万元）") %>%
  mutate(`GDP增长率（%）` = as.numeric(`GDP增长率（%）`))

clustering_data_indice_m1 <- clustering_data_indice_m %>%
  dplyr::select(-c("省", "2015年行政区划编码")) %>%
  rename(city = `地级市`,
         district = `区/县(县级市）`) %>%
  mutate(city = paste0(city, "市"), 
         district = ifelse(district == "海安县", "海安市", district))

## County hospital

# clustering_data_1 <- read.xlsx("02_Inputs/江苏-医院&县医院-HX0930.xlsx", sheet = 3, colNames = TRUE)
# 
# clustering_data_2 <- read.xlsx("02_Inputs/江苏新样本_add1.xlsx", colNames = TRUE)
# 
# clustering_data_22 <- clustering_data_2 %>%
#   dplyr::select(city, Prefecture, hospital_name, PHA, Est_DrugIncome_RMB, 医生数, 床位数) %>%
#   mutate(PHA = ifelse(is.na(PHA), hospital_name, PHA)) %>%
#   dplyr::select(-hospital_name) %>%
#   unique() %>%
#   rename(hosp_name = PHA,
#          prefecture = Prefecture) %>%
#   mutate(prefecture = ifelse(hosp_name == "宜兴市红塔医院(宜兴市金陵男科医院)", "宜兴市", 
#                              ifelse(hosp_name == "海门市第六人民医院", "海门市", 
#                                     ifelse(hosp_name == "如东县马塘医院", "如东县", 
#                                            ifelse(hosp_name == "无锡市江阴市人民医院(东区)", "江阴市", 
#                                                   prefecture))))) %>%
#   filter(!(hosp_name == "睢宁县人民医院" & prefecture == "泉山区"))
# 
# clustering_data_11 <- clustering_data_1 %>%
#   filter(!(新版名称 == "睢宁县人民医院" & is.na(医生数))) %>%
#   dplyr::select(City, Prefecture, 新版名称, Est_DrugIncome_RMB, 医生数, 床位数) %>%
#   rename(hosp_name = 新版名称,
#          city = City,
#          prefecture = Prefecture)
# 
# add <- clustering_data_22 %>%
#   filter(!(hosp_name %in% clustering_data_11$hosp_name))

clustering_data <- universe_data_js %>%
  mutate(city = ifelse(district == "海安市", "南通", city),
         city = paste0(city, "市"),
         Specialty_1 = ifelse(Specialty_1 == 0, "综合", Specialty_1))

description <- clustering_data %>%
  dplyr::select(city, district, hospital_name, 床位数) %>%
  left_join(clustering_data_indice_m1, by = c("city", "district")) %>%
  summary() %>%
  data.frame() %>%
  dplyr::select(Var2, Freq) %>%
  rename("Variables" = "Var2") %>%
  separate(Freq, c("description", "value"), sep = ":") %>%
  filter(!is.na(description)) %>%
  spread(description, value) %>%
  mutate(`Class ` = ifelse(is.na(`Class `), "numeric", `Class `),
         Length = ifelse(is.na(Length), 801, Length),
         `NA's   ` = ifelse(is.na(`NA's   `), 0, `NA's   `)) %>%
  dplyr::select(Variables, `Class `, Length, `NA's   `, `Min.   `, `1st Qu.`, `Median `, `Mean   `, `3rd Qu.`, `Max.   `)

write.xlsx(description, "Variable_description.xlsx")


## Check

chk_data <- read.xlsx("03_Outputs/county_hospitals_potential_projection_2016_1011.xlsx") %>%
  dplyr::select(hospital_name, value_2016)

chk <- clustering_data %>%
  dplyr::select(hospital_name, 床位数) %>%
  left_join(chk_data, by = "hospital_name")

sum(filter(chk, 床位数 == 0)$value_2016) / sum(chk$value_2016)

chk_1 <- universe_data_js %>%
  filter(is.na(Est_DrugIncome_RMB))

chk_2 <- universe_data_js %>%
  filter(is.na(医生数))

chk_3 <- universe_data_js %>%
  filter(is.na(床位数))

View(filter(chk_3, !(district %in% clustering_data$district)))
View(filter(chk_3, !(city %in% clustering_data$city)))


potential_2016 <- read.xlsx("02_inputs/county_hospitals_potential_projection_2016_1011.xlsx", sheet = 1)

potential_2017 <- read.xlsx("02_inputs/county_hospitals_potential_projection_2017_1011.xlsx", sheet = 1)

chk_potential_2016 <- potential_2016 %>%
  filter(hospital_name %in% chk_size$hospital_name)

sum(chk_potential_2016$value_2016) / sum(potential_2016$value_2016)    # = 0.03828127


chk <- clustering_data_22 %>%
  filter(!is.na(Est_DrugIncome_RMB)) %>%
  filter(!is.na(床位数)) %>%
  filter(!is.na(医生数))

View(table(ifelse(clustering_data_11_m$Est_DrugIncome_RMB == clustering_data_11$Est_DrugIncome_RMB, 1, 2)))
View(filter(clustering_data_11_m, is.na(clustering_data_11$Est_DrugIncome_RMB) & is.na(clustering_data_11_m$Est_DrugIncome_RMB)))

View(table(ifelse(clustering_data_11_m$医生数 == clustering_data_11$医生数, 1, 2)))
View(filter(clustering_data_11_m, is.na(clustering_data_11$医生数) & is.na(clustering_data_11_m$医生数)))

View(table(ifelse(clustering_data_11_m$床位数 == clustering_data_11$床位数, 1, 2)))
View(filter(clustering_data_11_m, is.na(clustering_data_11$床位数) & is.na(clustering_data_11_m$床位数)))


##------------------------------------------------------------------------------
##--                 Clustering districts
##------------------------------------------------------------------------------

## Standardized data

clustering_data_m <- clustering_data %>%
  dplyr::select(city, district, 床位数) %>%
  filter(!is.na(床位数) & 床位数 != 0)

clustering_data_district <- clustering_data_m %>%
  group_by(district) %>%
  summarise(床位数 = sum(床位数)) %>%
  left_join(clustering_data_indice_m1, by = "district")

clustering_data_district_m <- clustering_data_district %>%
  dplyr::select(-city, -district) %>%
  scale() %>%
  data.frame(row.names = as.character(clustering_data_district$district))

summary(clustering_data_district_m)

## k = 3

district_cluster <- function(x, y) {
  
  ss <- matrix(ncol = 6, nrow = x * y) %>%
    data.frame()
  colnames(ss) <- c("type", "gdp_factor", "pop_factor", "total_within_ss", "between_ss", "total_ss")
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      # i = 4
      # j = 4

      print(paste0("gdp * ", i, ", population * ", j))
      
      data <- clustering_data_district_m %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.) %>%
        data.frame(row.names = as.character(clustering_data_district$district))

      iciclename <- paste0("03_Outputs/district_cluster/district_icicle_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = iciclename, width = 800, height = 600, quality = 100)
      icicle_plot <- fviz_nbclust(data, kmeans, method = "wss", k.max = 10)
      print(icicle_plot)
      dev.off()
      
      set.seed(123)
      district_kmeans <- kmeans(data, centers = 3, nstart = 41)
      
      plotname <- paste0("03_Outputs/district_cluster/district_kmeans_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = plotname, width = 800, height = 600, quality = 100)
      cluster_plot <- fviz_cluster(district_kmeans, data, ellipse = TRUE, ellipse.type = "euclid", stand = FALSE, repel = TRUE) +
        geom_hline(aes(yintercept = 0), linetype = "dashed") +
        geom_vline(aes(xintercept = 0), linetype = "dashed")
      print(cluster_plot)
      dev.off()
      
      pca <- prcomp(data, scale. = FALSE, center = FALSE)
      
      clustered_data_district <- clustering_data_district %>%
        mutate(cluster = district_kmeans$cluster)
      
      # cluster_wb <- createWorkbook()
      # addWorksheet(cluster_wb, "cluster_results")
      # writeData(cluster_wb, "cluster_results", clustered_data_district)
      # excelname <- paste0("03_Outputs/district_cluster/district_cluster_gdp=", i, "_pop=", j, ".xlsx")
      # saveWorkbook(cluster_wb, file = excelname, overwrite = TRUE)
      
      k <- 10 * (i - 1) + j
      ss[k, ] <- c("district", i, j, district_kmeans$tot.withinss, district_kmeans$betweenss, district_kmeans$totss)
      
    }
  }
  
  write.xlsx(ss, "03_Outputs/district_cluster/distance_ss.xlsx")
  
}

district_cluster(10, 10)


##------------------------------------------------------------------------------
##--                 Clustering cities
##------------------------------------------------------------------------------

## Standardized data

clustering_data_city <- clustering_data_district %>%
  dplyr::select(-district) %>%
  group_by(city) %>%
  summarise(床位数 = sum(床位数),
            `GDP总值(亿元)` = sum(`GDP总值(亿元)`),
            `GDP增长率（%）` = mean(`GDP增长率（%）`),
            `常住人口(万人)` = sum(`常住人口(万人)`),
            `常住城镇人口(万人)` = sum(`常住城镇人口(万人)`),
            `常住乡村人口(万人)` = sum(`常住乡村人口(万人)`),
            `城镇居民人均可支配收入（元）` = mean(`城镇居民人均可支配收入（元）`),
            `城镇居民人均可支配收入增长率（%）` = mean(`城镇居民人均可支配收入增长率（%）`),
            `农民人均可支配收入（元）` = mean(`农民人均可支配收入（元）`),
            `农民人均可支配收入增长率（%）` = mean(`农民人均可支配收入增长率（%）`),
            `SocialConsumption.(亿元)` = sum(`SocialConsumption.(亿元)`),
            `RetailOTC.(万元）` = sum(`RetailOTC.(万元）`))

clustering_data_city_m <- clustering_data_city %>%
  dplyr::select(-city) %>%
  scale() %>%
  data.frame(row.names = as.character(clustering_data_city$city))

summary(clustering_data_city_m)

## k = 3

city_cluster <- function(x, y) {
  
  ss <- matrix(ncol = 6, nrow = x * y) %>%
    data.frame()
  colnames(ss) <- c("type", "gdp_factor", "pop_factor", "total_within_ss", "between_ss", "total_ss")
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      # i = 4
      # j = 4
      
      print(paste0("gdp * ", i, ", population * ", j))
      
      data <- clustering_data_city_m %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.) %>%
        data.frame(row.names = as.character(clustering_data_city$city))

      iciclename <- paste0("03_Outputs/city_cluster/city_icicle_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = iciclename, width = 800, height = 600, quality = 100)
      icicle_plot <- fviz_nbclust(data, kmeans, method = "wss", k.max = 10)
      print(icicle_plot)
      dev.off()
      
      set.seed(123)
      city_kmeans <- kmeans(data, centers = 3, nstart = 41)
      
      plotname <- paste0("03_Outputs/city_cluster/city_kmeans_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = plotname, width = 800, height = 600, quality = 100)
      cluster_plot <- fviz_cluster(city_kmeans, data, ellipse = TRUE, ellipse.type = "euclid", stand = FALSE, repel = TRUE) +
        geom_hline(aes(yintercept = 0), linetype = "dashed") +
        geom_vline(aes(xintercept = 0), linetype = "dashed")
      print(cluster_plot)
      dev.off()
      
      pca <- prcomp(data, scale. = FALSE, center = FALSE)
      
      clustered_data_city <- clustering_data_city %>%
        mutate(cluster = city_kmeans$cluster)
      
      # cluster_wb <- createWorkbook()
      # addWorksheet(cluster_wb, "cluster_results")
      # writeData(cluster_wb, "cluster_results", clustered_data_city)
      # excelname <- paste0("03_Outputs/city_cluster/city_cluster_gdp=", i, "_pop=", j, ".xlsx")
      # saveWorkbook(cluster_wb, file = excelname, overwrite = TRUE)
      
      k <- 10 * (i - 1) + j
      ss[k, ] <- c("city", i, j, city_kmeans$tot.withinss, city_kmeans$betweenss, city_kmeans$totss)
      
    }
  }
  
  write.xlsx(ss, "03_Outputs/city_cluster/distance_ss.xlsx")
  
}

city_cluster(10, 10)


##------------------------------------------------------------------------------
##--                 Hierarchical clustering
##------------------------------------------------------------------------------

## Of districts

# hcpc_data_district <- clustering_data_district %>%
#   dplyr::select(-city, -district)
# rownames(hcpc_data_district) <- as.character(clustering_data_district$district)
# 
# pca_district <- princomp(hcpc_data_district, cor = TRUE)
# summary(pca_district)
# 
# pc_district <- predict(pca_district) %>%
#   data.frame()
# dist_district <- pc_district %>%
#   dplyr::select(Comp.1, Comp.2, Comp.3, Comp.4, Comp.5) %>%
#   dist(method = "euclidean")
# 
# hc_district <- hclust(pc_district, method = "ward.D")
# 
# fviz_dend(hc_district, k = 3, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), color_labels_by_k = TRUE, rect = TRUE)

## Of cities

# hcpc_data_city <- clustering_data_city %>%
#   dplyr::select(-city)
# rownames(hcpc_data_city) <- as.character(clustering_data_city$city)
# 
# pca_city <- princomp(hcpc_data_city, cor = TRUE)
# summary(pca_city)
# 
# pc_city <- predict(pca_city) %>%
#   data.frame()
# dist_city <- pc_city %>%
#   dplyr::select(Comp.1, Comp.2, Comp.3, Comp.4) %>%
#   dist(method = "euclidean")
# 
# hc_city <- hclust(pc_city, method = "ward.D")
# 
# fviz_dend(hc_city, k = 3, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), color_labels_by_k = TRUE, rect = TRUE)


# hcpc_wb <- createWorkbook()
# addWorksheet(hcpc_wb, "pc_of_districts")
# # addWorksheet(hcpc_wb, "loadings_of_districts")
# addWorksheet(hcpc_wb, "dendrogram_of_districts")
# addWorksheet(hcpc_wb, "pc_of_cities")
# # addWorksheet(hcpc_wb, "loadings_of_cities")
# addWorksheet(hcpc_wb, "dendrogram_cities")
# 
# writeData(hcpc_wb, "pc_of_districts", pc_district, rowNames = TRUE)
# # writeData(hcpc_wb, "loadings_of_districts", pca_district)
# writeData(hcpc_wb, "pc_of_cities", pc_city, rowNames = TRUE)
# # writeData(hcpc_wb, "loadings_of_cities", pca_city$loadings)
# 
# saveWorkbook(hcpc_wb, "03_Outputs/hcpc/hcpc.xlsx", overwrite = TRUE)


## Of districts

district_hc <- function(x, y) {
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      print(paste0("gdp * ", i, ", population * ", j))
      
      hc_data <- clustering_data_district_m %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.) %>%
        data.frame(row.names = as.character(clustering_data_district$district)) %>%
        dist(method = "euclidean")
      
      set.seed(123)
      hc <- hclust(hc_data, method = "ward.D")
      
      plotname <- paste0("03_Outputs/district_hc/district_hc_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = plotname, width = 800, height = 600, quality = 100)
      dend <- fviz_dend(hc, k = 3, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), color_labels_by_k = TRUE, rect = TRUE)
      print(dend)
      dev.off()
      
    }
  }
  
}

district_hc(10, 10)

## Of cities

city_hc <- function(x, y) {
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      print(paste0("gdp * ", i, ", population * ", j))
      
      hc_data <- clustering_data_city_m %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.) %>%
        data.frame(row.names = as.character(clustering_data_city$city)) %>%
        dist(method = "euclidean")
      
      set.seed(123)
      hc <- hclust(hc_data, method = "ward.D")
      
      plotname <- paste0("03_Outputs/city_hc/city_hc_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = plotname, width = 800, height = 600, quality = 100)
      dend <- fviz_dend(hc, k = 3, k_colors = c("#2E9FDF", "#E7B800", "#FC4E07"), color_labels_by_k = TRUE, rect = TRUE)
      print(dend)
      dev.off()
      
    }
  }
  
}

city_hc(10, 10)


##------------------------------------------------------------------------------
##--                 Gaussian mixture model
##------------------------------------------------------------------------------

# data(dietary_survey_IBS)
# dat = as.matrix(dietary_survey_IBS[, -ncol(dietary_survey_IBS)])
# dat = center_scale(dat)
# gmm = GMM(dat, 2, "maha_dist", "random_subset", 10, 10)
# pca_dat = stats::princomp(dat)$scores[, 1:2]
# chk <- stats::princomp(dat)$loadings
# pr = predict_GMM(dat, gmm$centroids, gmm$covariance_matrices, gmm$weights)
# chk1 <- gmm$centroids %*% chk
# plot_2d(pca_dat, pr$cluster_labels, chk1[, c(1,2)])

## Of districts

district_gmm <- function(x, y) {
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      i = 4
      j = 4
      
      print(paste0("gdp * ", i, ", population * ", j))
      
      gmm_data <- clustering_data_district_m %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.) %>%
        data.frame(row.names = as.character(clustering_data_district$district))
      
      gmm <- GMM(gmm_data, gaussian_comps = 3, dist_mode = "maha_dist",
                 seed_mode = "random_subset", km_iter = 10, em_iter = 10)
      
      pr <- predict_GMM(gmm_data, gmm$centroids, gmm$covariance_matrices, gmm$weights)
      pca_data <- princomp(gmm_data)$scores[, 1: 2] %>%
        as.data.frame() %>%
        mutate(cluster = pr$cluster_labels) %>%
        data.frame(row.names = as.character(clustering_data_district$district))
      
      plotname <- paste0("03_Outputs/district_gmm/district_gmm_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = plotname, width = 800, height = 600, quality = 100)
      cluster_plot <- qplot(data = pca_data, x = Comp.1, y = Comp.2,
                            color = factor(pca_data$cluster),
                            shape = factor(pca_data$cluster),
                            show.legend = F) +
        scale_color_manual(values = c("#2E9FDF", "#E7B800", "#FC4E07")) +
        geom_text(data = pca_data, aes(Comp.1, Comp.2, label = row.names(pca_data)),
                  position = position_dodge(width = 1.5), vjust = -0.5) +
        xlab("Comp.1") +
        ylab("Comp.2") +
        theme(legend.position = "none")
      print(cluster_plot)
      dev.off()
      
    }
  }
  
}

district_gmm(10, 10)

## Of cities

city_gmm <- function(x, y) {
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      # i = 4
      # j = 4
      
      print(paste0("gdp * ", i, ", population * ", j))
      
      gmm_data <- clustering_data_city_m %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.) %>%
        data.frame(row.names = as.character(clustering_data_city$city))
      
      gmm <- GMM(gmm_data, gaussian_comps = 3, dist_mode = "eucl_dist",
                 seed_mode = "random_subset", km_iter = 10, em_iter = 10)
      
      pr <- predict_GMM(gmm_data, gmm$centroids, gmm$covariance_matrices, gmm$weights)
      pca_data <- princomp(gmm_data)$scores[, 1: 2] %>%
        as.data.frame() %>%
        mutate(cluster = pr$cluster_labels) %>%
        data.frame(row.names = as.character(clustering_data_city$city))
      
      plotname <- paste0("03_Outputs/city_gmm/city_gmm_gdp=", i, "_pop=", j, ".jpeg")
      jpeg(file = plotname, width = 800, height = 600, quality = 100)
      cluster_plot <- qplot(data = pca_data, x = Comp.1, y = -Comp.2,
                            color = factor(pca_data$cluster),
                            shape = factor(pca_data$cluster),
                            show.legend = F) +
        scale_color_manual(values = c("#2E9FDF", "#E7B800", "#FC4E07")) +
        geom_text(data = pca_data, aes(Comp.1, -Comp.2, label = row.names(pca_data)),
                  position = position_dodge(width = 1.5), vjust = -0.5) +
        xlab("Comp.1") +
        ylab("Comp.2") +
        theme(legend.position = "none")
      print(cluster_plot)
      dev.off()
      
    }
  }
  
}

city_gmm(10, 10)

##------------------------------------------------------------------------------
##--                 PCA
##------------------------------------------------------------------------------

district_m <- clustering_data_district %>%
  select(-district, -city) %>%
  data.frame(row.names = clustering_data_district$district)
district_m1 <- scale(district_m) %>%
  data.frame(row.names = clustering_data_district$district)
pca_district <- prcomp(district_m1, scale. = FALSE, center = FALSE)

city_m <- clustering_data_city %>%
  select(-city) %>%
  data.frame(row.names = clustering_data_city$city)
city_m1 <- scale(city_m) %>%
  data.frame(row.names = clustering_data_city$city)
pca_city <- prcomp(city_m1, scale. = FALSE, center = FALSE)

pc <- createWorkbook()
addWorksheet(pc, "district")
addWorksheet(pc, "district_scaled")
addWorksheet(pc, "district_loadings")
addWorksheet(pc, "district_scores")
addWorksheet(pc, "city")
addWorksheet(pc, "city_scaled")
addWorksheet(pc, "city_loadings")
addWorksheet(pc, "city_scores")
writeData(pc, "district", district_m, rowNames = TRUE)
writeData(pc, "district_scaled", district_m1, rowNames = TRUE)
writeData(pc, "district_loadings", pca_district$rotation, rowNames = TRUE)
writeData(pc, "district_scores", pca_district$x, rowNames = TRUE)
writeData(pc, "city", city_m, rowNames = TRUE)
writeData(pc, "city_scaled", city_m1, rowNames = TRUE)
writeData(pc, "city_loadings", pca_city$rotation, rowNames = TRUE)
writeData(pc, "city_scores", pca_city$x, rowNames = TRUE)
saveWorkbook(pc, "03_Outputs/pca_1120.xlsx", overwrite = TRUE)









