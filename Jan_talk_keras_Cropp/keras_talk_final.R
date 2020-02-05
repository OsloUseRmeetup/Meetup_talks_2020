library(tidyverse)
library(tidyr)
library(naniar)
library(tidyimpute)
library(keras)
library(tidyverse)

#DATA PREP


hampshire<- read_csv(paste(getwd(), "/hampshire.csv", sep=""))

latitudes <- read_csv(paste(getwd(), "/latitude.csv", sep=""))

hampshire <- hampshire %>% 
  inner_join(latitudes, by=c("manor_name"="place"))

hampshire <- hampshire %>% 
  mutate(manor_name=as.numeric(as.factor(manor_name))) %>% 
  group_by(manor_name) %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  ungroup %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>% 
  select(manor_name, latitude, longitude, wheat_gross_yield_per_seed_ratio, barley_gross_yield_per_seed_ratio, oats_gross_yield_per_seed_ratio)



#hampshire <- hampshire %>% 
#  select(manor_name,
#         wheat_gross_yield_per_seed_ratio:longitude) %>% 
#  select(-source)

# train and test sets

require(caTools)
set.seed(101) 
sample = sample.split(hampshire$wheat_gross_yield_per_seed_ratio, SplitRatio = .75)
train = subset(hampshire, sample == TRUE)
test  = subset(hampshire, sample == FALSE)

train_x<- train %>% 
  select(-oats_gross_yield_per_seed_ratio)

train_x<- as.matrix(train_x)

train_y <- train %>% 
  select(oats_gross_yield_per_seed_ratio)

train_y <- as.matrix(train_y)

test_x<- test %>% 
  select(-oats_gross_yield_per_seed_ratio)

test_x<- as.matrix(test_x)

test_y <- test %>% 
  select(oats_gross_yield_per_seed_ratio)

test_y <- as.matrix(test_y)

#first model

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu",
              input_shape = dim(train_x)[2]) %>% #2
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

print(model)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mean_absolute_error"))

model %>% fit(train_x, train_y, epochs = 10, verbose=1)

scores = model %>% evaluate(test_x,test_y, verbose = 0)
scores

sd(hampshire$oats_gross_yield_per_seed_ratio)

y_pred = model %>% predict(test_x)








#normalizing

mean <- apply(train, 2, mean)
std <- apply(train, 2, sd)
train <- scale(train, center = mean, scale = std)
test <- scale(test, center = mean, scale = std)

train_x<- train[ , 1:5]

train_y <- train[, 6]

test_x<- test[, 1:5]

test_y <- test[, 6]

#overfitting model

model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu",
              input_shape = dim(train_x)[2]) %>% #2
  layer_dense(units = 64, activation = "relu") %>%
  #layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1)

print(model)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mean_absolute_error"))

model %>% fit(train_x, train_y, epochs = 10, verbose=1, validation_split=0.2)

model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu",
              input_shape = dim(train_x)[2]) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 128, kernel_regularizer = regularizer_l2(0.01),
              activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(),
  metrics = list("mean_absolute_error"))

model %>% fit(train_x, train_y, epochs = 10, verbose=1, validation_split=0.2)

#still overfitting slightly










#### RNN time

hampshire<- read_csv(paste(getwd(), "/hampshire.csv", sep=""))

hambledon <- hampshire %>%
  filter(manor_name =="Hambledon")

naniar::miss_var_summary(hambledon)

hambledon <- hambledon %>% 
  select(end_year:oats_gross_yield_per_seed_ratio) 


end_years <- as.data.frame(seq(1211,1347))

names(end_years) <- "end_year"

hambledon <- end_years %>% 
  left_join(hambledon, by="end_year")

hambledon <- hambledon %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))








# the <<- operator

x <- 0
x
f <- function() {
  x <<- x+1
}
f()
x

f()

x





#basic generaor

data <- hambledon
target_column <-4
max_index <- 80
lookback <- 5
i <<- lookback +1
i
batch_size <- 2
rows <- c(i:min(i, max_index))
rows
i <<- i + length(rows)
i

samples <- array(0, dim = c(length(rows), 
                            lookback,
                            dim(data)[[-1]]))
targets <- array(0, dim = c(length(rows)))

samples
targets

for (j in 1:length(rows)) {
  indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, 
                 length.out = dim(samples)[[2]])
  samples[j,,] <- data[indices,]
  targets[[j]] <- data[rows[[j]], target_column]
} 

samples
targets


### put in function





batch_size <- 2
train_end <- 100


generator <- function(data, lookback, max_index,
                      shuffle = FALSE, batch_size = 128) {
  i <- 1 + lookback
  function() {
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    
    
    samples <- array(0, dim = c(length(rows), 
                                lookback,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]], target_column]
    }            
    
    list(samples, targets)
  }
}

train_gen <- generator(
  hambledon,
  lookback = lookback,
  max_index = train_end,
  batch_size = batch_size
)
train_gen()

train_gen()

generated_object <- train_gen()
generated_object[[2]]
generated_object[[1]][1, , ]

data <- hambledon

train_end <- 80 
val_start <- train_end + 1
val_end <- 100 
test_start <- val_end + 1
test_end <- nrow(data)

train_data <- data[1:train_end,]

std <- c(apply(train_data, 2, sd))
mean <-c(apply(train_data, 2, mean))
data <- scale(data, center = mean, scale = std)

target_column <- 4


#NB! The data MUST be a matrix, not a dataframe!!
#right now this is done by scale()
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay, target_column]
    }            
    
    list(samples, targets)
  }
}
lookback <- 5 
step <- 1 
delay <- 1 
batch_size <- 2 
#

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = train_end,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

train_gen()



model <- keras_model_sequential() %>% 
  layer_simple_rnn(units = 32, input_shape = list(NULL, dim(hambledon)[[2]])) %>% 
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)
model %>% fit_generator(
  train_gen,
  steps_per_epoch = 2,
  epochs = 20
)
### add in validation set 

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index =train_end + 1,
  max_index = val_end,
  step = step,
  batch_size = batch_size
)
val_gen()

val_steps <- (val_end - (train_end+1) - lookback) / batch_size

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = val_end + 1,
  max_index = NULL,#test_end
  step = step,
  batch_size = batch_size
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 5,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)
