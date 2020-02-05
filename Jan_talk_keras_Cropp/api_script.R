

library(readr)
library(dplyr)
library(keras)

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
  select(manor_name, latitude, longitude, end_year, wheat_gross_yield_per_seed_ratio, barley_gross_yield_per_seed_ratio, oats_gross_yield_per_seed_ratio)



data <- as.matrix(hampshire)
target_column <- 7
shuffle_column <-1

generator <- function(data, lookback, delay, subset,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  manors <<-unique(data[ ,shuffle_column])
  if(subset=="train"){
    start_point <- 0
    end_point <- 0.7
  }else if(subset =="validation"){
    start_point <-0.7
    end_point <- 0.85
  }else if(subset=="test"){
    start_point <- 0.85
    end_point <- 1
  }
  function() {
    
    manor <- manors[runif(1, 1, 17)]
    data_tmp <- data[data[, shuffle_column] == manor,]
    data_filtered <- data_tmp[(start_point*length(data_tmp[,1])+1):(end_point*length(data_tmp[,1])) , ]
    #data_filtered <- data_tmp[1:5000 , ]
    max_index <- length(data_filtered[ , 1])
    min_index <-1
    
    if (is.null(max_index))
      max_index <- nrow(data) - delay - 1
    i <- min_index + lookback
    
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }

    samples_input1 <- array(0, dim = c(length(rows), 
                                lookback / step,
                                4))
    samples_input2 <- array(0, dim=c(1, 3))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]] - 1, 
                     length.out = dim(samples_input1)[[2]])
      samples_input1[j,,] <- data_filtered[indices, 4:7]
      samples_input2[] <- data_filtered[1, 1:3]
      targets[[j]] <- data_filtered[rows[[j]] + delay, target_column]
    }            
    
    list(list(samples_input1, samples_input2), list(targets))
    
  }
  
}


lookback <- 5
step <- 1 
delay <- 0 
batch_size <- 1

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  subset="train",
  #shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)
gen_object <- train_gen()


in_a <- layer_input(shape = list(NULL, 4), name = "input1")
in_b <- layer_input(shape = c(3), name = "input2")


library(keras)


lstm_out <- in_a %>% 
  layer_lstm(units = 32) %>% 
  layer_dense(units = 32)

dense_out <- in_b %>% 
  layer_dense(units = 32)

out <- layer_concatenate(c(lstm_out, dense_out), axis=-1, name="concat") %>% 
  layer_dense(units = 1, activation = 'linear', name="output")

model <- keras_model(inputs = list(in_a, in_b), outputs = list(out))

model %>% compile(loss = "mse", optimizer = "adam")

model %>% fit_generator(
  train_gen,
  steps_per_epoch = 2,
  epochs = 20
)

print(model)
