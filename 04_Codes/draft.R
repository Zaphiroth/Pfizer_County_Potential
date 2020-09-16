district_cluster1 <- function(x, y) {
  
  
  for(i in 1: x) {
    for(j in 1: y) {
      
      # i = 1
      # j = 1
      

      data <- clustering_data_district_m %>%
        data.frame() %>%
        mutate(GDP总值.亿元. = i * GDP总值.亿元.,
               常住人口.万人. = j * 常住人口.万人.)
      rownames(data) <- clustering_data_district$district
      
      iciclename <- paste0("03_Outputs/district_cluster/district_icicle_", i, "gdp_", j, "pop.jpeg")
      jpeg(file = iciclename)
      print(fviz_nbclust(data, kmeans, method = "wss", k.max = 10))
      dev.off()
      
    }
  }
  
}

district_cluster1(1, 1)
