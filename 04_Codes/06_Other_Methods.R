# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Pfizer County Hospitals Potential Project
# Purpose:      Try Neural Network Method
# programmer:   Zhe Liu
# Date:         12-03-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##------------------------------------------------------------------------------
##--                 Loading the required packages
##------------------------------------------------------------------------------
options(java.parameters = "-Xmx2048m")
suppressPackageStartupMessages({
  library(openxlsx)
  library(RODBC)
  library(plm)
  library(dynlm)
  library(randomForest)
  library(data.table)
  library(stringi)
  library(stringr)
  library(purrr)
  library(caret)
  library(progress)
  library(qrcode)
  library(qrencoder)
  library(png)
  library(base64)
  library(keras)
  library(tidyverse)
})

##------------------------------------------------------------------------------
##--                 CNN matrixes
##------------------------------------------------------------------------------

write.xlsx(universe_data_js_train, "02_Inputs/universe_train.xlsx")
write.xlsx(universe_data_js_test, "02_Inputs/universe_test.xlsx")

names.col <- universe_data_js_train %>%
  dplyr::select(-c(hospital_name, PHA, Specialty_2, Specialty.3, flag, province,
                   value_2016, unit_2016, county_code_2015, value_2017)) %>%
  names()

##-- train data
train_data <- universe_data_js_train %>%
  dplyr::select(-c(hospital_name, PHA, Specialty_2, Specialty.3, flag, province,
                   value_2016, unit_2016, county_code_2015, value_2017)) %>%
  unite("char", names.col, sep = ", ", remove = TRUE)

train.qr <- array(dim = c(105, 65, 65))
for (i in 1:105) {
  train.qr[i, , ] <- matrix(qrencode(train_data$char[i]), nrow = 65, ncol = 65)
}
train.qr <- array_reshape(train.qr, c(105, 65, 65, 1))
train.qr <- train.qr / 10

train_targets <- universe_data_js_train$value_2017 ^ (1/4)

##-- test data
test_data <- universe_data_js_test %>%
  dplyr::select(-c(hospital_name, PHA, Specialty_2, Specialty.3, flag, province,
                   value_2016, unit_2016, county_code_2015, value_2017)) %>%
  unite("char", names.col, sep = ", ", remove = TRUE)

test.qr <- array(dim = c(481, 65, 65))
for (i in 1:481) {
  test.qr[i, , ] <- matrix(qrencode(test_data$char[i]), nrow = 65, ncol = 65)
}
test.qr <- array_reshape(test.qr, c(481, 65, 65, 1))
test.qr <- test.qr / 10

##-- model
model1 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu", input_shape = c(65, 65, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

model1 %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history1 <- model1 %>%
  fit(
    train.qr,
    train_targets,
    batch_size = 1,
    epochs = 100,
    validation_split = 0.1
  )

plot(history1)

validation.mae1 <- data.frame(epoch = seq(1, 100),
                              validation.mae = history1$metrics$val_mean_absolute_error)
ggplot(validation.mae1, aes(x = epoch, y = validation.mae)) +
  geom_smooth()

##-- final model
final_model1 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu", input_shape = c(65, 65, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) %>%
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

final_model1 %>% fit(
  train.qr,
  train_targets,
  batch_size = 1,
  epochs = 40
)

##-- result
result1 <- final_model1 %>%
  predict(test.qr)
result1 <- result1 ^ 4

sum(result1) / (sum(result1) + sum(universe_data_js_train$value_2017, na.rm = TRUE))

##------------------------------------------------------------------------------
##--                 CNN images
##------------------------------------------------------------------------------

##-- generate png
city.num <- data.frame(city = unique(universe_data_js_train$city),
                       no.city = 1:12)

district.num <- data.frame(district = unique(universe_data_js_train$district),
                           no.district = 1:40)

train_data_m <- universe_data_js_train %>%
  dplyr::select(-c(hospital_name, PHA, Specialty_2, Specialty.3, flag, province,
                   value_2016, unit_2016, county_code_2015, value_2017)) %>%
  mutate(Hosp_level = ifelse(Hosp_level == "一级", 1,
                             ifelse(Hosp_level == "二级", 2, 0)),
         Specialty_1 = ifelse(Specialty_1 == "专科", 1, 0)) %>%
  left_join(city.num, by = "city") %>%
  left_join(district.num, by = "district") %>%
  dplyr::select(-city, -district) %>%
  rename("city" = "no.city",
         "district" = "no.district") %>%
  unite("char", names.col, sep = "/", remove = TRUE)

test_data_m <- universe_data_js_test %>%
  dplyr::select(-c(hospital_name, PHA, Specialty_2, Specialty.3, flag, province,
                   value_2016, unit_2016, county_code_2015, value_2017)) %>%
  mutate(Hosp_level = ifelse(Hosp_level == "一级", 1,
                             ifelse(Hosp_level == "—级甲等" | Hosp_level == "一级甲等", 1.1,
                                    ifelse(Hosp_level == "一级乙等", 1.2,
                                           ifelse(Hosp_level == "一级丙等", 1.3,
                                                  ifelse(Hosp_level == "二级", 2,
                                                         ifelse(Hosp_level == "二级甲等", 2.1,
                                                                ifelse(Hosp_level == "二级乙等", 2.2,
                                                                       ifelse(Hosp_level == "其他", 3,
                                                                              ifelse(Hosp_level == "未知等级", 4,
                                                                                     0))))))))),
         Specialty_1 = ifelse(Specialty_1 == "专科", 1, 0)) %>%
  left_join(city.num, by = "city") %>%
  left_join(district.num, by = "district") %>%
  dplyr::select(-city, -district) %>%
  rename("city" = "no.city",
         "district" = "no.district") %>%
  unite("char", names.col, sep = "/", remove = TRUE)

for (i in 1:105) {
  png_name <- paste0("02_Inputs/qrcode/train/", i, ".png")
  png(file = png_name, width = 150, height = 150)
  qr <- qrcode_gen(train_data_m$char[i])
  dev.off()
}

for (i in 1:481) {
  png_name <- paste0("02_Inputs/qrcode/test/", i, ".png")
  png(file = png_name, width = 150, height = 150)
  qr <- qrcode_gen(test_data_m$char[i])
  dev.off()
}

##-- load images
qrcode.train <- array(dim = c(105, 150, 150))
for (i in 1:105) {
  qrcode.train[i, , ] <- readPNG(paste("02_Inputs/qrcode/train/", i, ".png", sep = ""), native = TRUE)
}
qrcode.train <- array_reshape(qrcode.train, c(105, 150, 150, 1))
qrcode.train <- qrcode.train / 10


qrcode.test <- array(dim = c(481, 150, 150))
for (i in 1:481) {
  qrcode.test[i, , ] <- readPNG(paste("02_Inputs/qrcode/test/", i, ".png", sep = ""), native = TRUE)
}
qrcode.test <- array_reshape(qrcode.test, c(481, 150, 150, 1))
qrcode.test <- qrcode.test / 10

##-- model
model3 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu", input_shape = c(150, 150, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

model3 %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history3 <- model3 %>%
  fit(
    qrcode.train,
    train_targets,
    batch_size = 1,
    epochs = 100,
    validation_split = 0.1
  )

plot(history3)

validation.mae3 <- data.frame(epoch = seq(4, 100, by = 1),
                              validation.mae = history3$metrics$val_mean_absolute_error[4:100])
ggplot(validation.mae3, aes(x = epoch, y = validation.mae)) +
  geom_smooth()

##-- final model
final_model3 <- keras_model_sequential() %>%
  layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = "relu", input_shape = c(150, 150, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1) %>%
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

final_model3 %>% fit(
  qrcode.train,
  train_targets,
  batch_size = 1,
  epochs = 30
)

##-- result
result3 <- final_model3 %>%
  predict(qrcode.test)
result3 <- result3 ^ 4

sum(result3) / (sum(result3) + sum(universe_data_js_train$value_2017, na.rm = TRUE))

##------------------------------------------------------------------------------
##--                 Dense Model
##------------------------------------------------------------------------------

##-- data
train2 <- array(dim = c(105, 65, 65))
for (i in 1:105) {
  train2[i, , ] <- matrix(qrencode(train_data$char[i]), nrow = 65, ncol = 65)
}
train2 <- array_reshape(train2, c(105, 65 * 65))
train2 <- train2 / 10

test2 <- array(dim = c(481, 65, 65))
for (i in 1:481) {
  test2[i, , ] <- matrix(qrencode(test_data$char[i]), nrow = 65, ncol = 65)
}
test2 <- array_reshape(test2, c(481, 65 * 65))
test2 <- test2 / 10

##-- model
model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(65 * 65)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1)

model2 %>% compile(
  optimizer = "rmsprop",
  loss = "mse",
  metrics = c("mae")
)

history2 <- model2 %>%
  fit(
    train2,
    train_targets,
    batch_size = 4,
    epochs = 100,
    validation_split = 0.1
  )

plot(history2)

validation.mae2 <- data.frame(epoch = seq(1, 100),
                              validation.mae = history2$metrics$val_mean_absolute_error)
ggplot(validation.mae2, aes(x = epoch, y = validation.mae)) +
  geom_smooth()

##-- final model
final_model2 <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(65 * 65)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1) %>% compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )

final_model2 %>% fit(
  train2,
  train_targets,
  batch_size = 1,
  epochs = 30
)

##-- result
result2 <- final_model2 %>%
  predict(test2)
result2 <- result2 ^ 4

sum(result2) / (sum(result2) + sum(universe_data_js_train$value_2017, na.rm = TRUE))







