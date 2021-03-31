#tensorflow::install_tensorflow(extra_packages='pillow')
library(keras)
library(tensorflow)
library(tidyverse)

base_dir <- "C:/Users/emiwes/Downloads/dataset-resized/dataset-resized"
list.dirs(base_dir,full.names = F)
fnames <- list.files(base_dir, full.names = TRUE,recursive = T,include.dirs = F)
n <- fnames %>% length()
labs <- gsub("(.*)/([a-z0-9]+.jpg)", "\\2", fnames)
labs <- as_tibble(labs)
class_name <-  gsub("(.*)/([a-z]+)([0-9]+).jpg", "\\2", fnames) 
labs$class_name <- class_name
labs$class_name <- factor(labs$class_name)
binarized <- model.matrix( value ~ 0 + ., data = labs, contrasts.arg = list(contrasts=F) )
labs <- cbind(id=labs$value, as_tibble(binarized))
colnames(labs) <- gsub("class_name","", colnames(labs) )
labs$id <- list.files(base_dir, full.names = F, recursive = T)

head(labs,5)

# split 50% for train+validation and 50% for test
train_validation_proportion <- 0.7
train_proportion <- 0.8 

set.seed(40)
smp_size <- floor(train_validation_proportion * n ) # 1263
train.validation.ind <- sample(seq_len(n), size = smp_size)
train.validation.labels.full <- labs[train.validation.ind, ]
test.labels.full <- labs[-train.validation.ind, ]

# now we split train+validation into train/validation
set.seed(40)
# 80% train, 20% validation
train.ind <- sample(1:nrow(train.validation.labels.full) , floor(train_proportion*length(train.validation.ind)) )
training.labels.full <- train.validation.labels.full[train.ind, ]
validation.labels.full <- train.validation.labels.full[-train.ind, ]

nrow(train.validation.labels.full) + nrow(test.labels.full) == n
nrow(training.labels.full) +  nrow(validation.labels.full) + nrow(test.labels.full) == n

# ------------------------------------------------------------------------------

train_datagen <- image_data_generator(rescale = 1/255) 
test_datagen <- image_data_generator(rescale = 1/255) 
val_datagen <- image_data_generator(rescale = 1/255) 

train_generator <- flow_images_from_dataframe(
  directory = base_dir,
  dataframe = training.labels.full,
  generator = train_datagen,
  x_col = "id",
  y_col =  list("cardboard","glass","metal","paper","plastic","trash"),
  seed = 40,
  shuffle = TRUE,
  class_mode = "other", 
  target_size = c(150,150)
)

test_generator <- flow_images_from_dataframe(
  directory = base_dir,
  dataframe = test.labels.full,
  generator = test_datagen,
  x_col = "id",
  y_col =  list("cardboard","glass","metal","paper","plastic","trash"),
  seed = 40,
  shuffle = TRUE,
  class_mode = "other",
  target_size = c(150,150)
)

validation_generator <- flow_images_from_dataframe(
  directory = base_dir,
  dataframe = validation.labels.full,
  generator = val_datagen,
  x_col = "id",
  y_col =  list("cardboard","glass","metal","paper","plastic","trash"),
  seed = 40,
  shuffle = TRUE,
  class_mode = "other",
  target_size = c(150,150)
)

# ------------------------------------------------------------------------------
# custom neural network

# https://stackoverflow.com/questions/45645276/negative-dimension-size-caused-by-subtracting-3-from-1-for-conv2d-2-convolution/45647715#45647715
k <- c(3,3)
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = k, activation = "relu", input_shape = c(150,150,3) ) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = k, activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = k, activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 24, kernel_size = k, activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>% 
  layer_dropout(rate=0.2) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dense(units = 120, activation = "relu") %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dense(units = 15, activation = "relu") %>%
  layer_dense(units = 6, activation = "softmax")

model %>%
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = 1e-4),
    metrics = c('accuracy')
  )
history <- model %>%
  fit_generator(  train_generator,
                  steps_per_epoch = 10,
                  epochs = 15,
                  validation_data = validation_generator,
                  validation_steps = 10 )






conv_base <- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150, 150, 3)
)

freeze_weights(conv_base)

model2 <- keras_model_sequential() %>%
  conv_base %>%
  layer_flatten() %>%
  layer_dropout(rate=0.2) %>%
  layer_dense(units = 500, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = 6, activation = "softmax")

model2 %>%
  compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = 1e-4),
    metrics = c('accuracy')
  )

history2 <- model2 %>%
  fit_generator(  train_generator,
                  steps_per_epoch = 15,
                  epochs = 30,
                  validation_data = validation_generator,
                  validation_steps = 10 )
model2
plot(history2)


