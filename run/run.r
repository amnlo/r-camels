# this is the file that runs the general analysis
library(dplyr)
library(tidyr)
library(tensorflow)
library(keras)
library(ggplot2)
library(data.table)
library(abind)

source("../functions/read.catchment.attributes.r")
source("../functions/read.forcing.r")
source("../functions/read.strmflw.r")
source("../functions/preprocess.data.r")
source("../functions/scale.log.r")

# pth <- "../camels_attributes_v2_0/camels_attributes_v2_0/data/"
# attrib <- read.catchment.attributes(pth)
# pth <- "../basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/usgs_streamflow/"
# strmflw <- read.strmflw(pth)
# pth <- "../basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/basin_mean_forcing/daymet"
# forcing <- read.forcing(pth)
# all.data <- list(attrib=attrib, strmflw=strmflw, forcing=forcing)
# save(all.data, file="../all_data.RData")
load("../all_data.RData")

lags <- 60
data <- preprocess.data(all.data, lags=lags, n.catchments = 200, prop.separate = FALSE)
x <- data$x
y <- data$y[,1]

## ==================================================================
## split into training and test data
batch.size <- 32
validation_split <- 0.2
even.train <- ((nrow(x)*(1-validation_split))%/%batch.size)*batch.size
even.test  <- ((nrow(x)*validation_split)%/%batch.size)*batch.size
x.train <- x[1:even.train,,]
x.test <-  x[(even.train+1):(even.train+even.test),,]
y.train <- y[1:even.train]
y.test  <- y[(even.train+1):(even.train+even.test)]

## ==================================================================
## construct LSTM
model <- keras_model_sequential()
model %>% layer_lstm(units = 64,
                     input_shape = c(lags+1, dim(x.train)[3]),
                     batch_size = batch.size,
                     stateful = FALSE) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)
model %>% compile(loss = 'mse', optimizer = 'adam', metrics='mse')

## ==================================================================
## make sure to save checkpoints while training
checkpoint_dir <- "checkpoints"
dir.create(checkpoint_dir)
filepath <- file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
# Create checkpoint callback
cp_callback <- callback_model_checkpoint(
  filepath = filepath,
  save_weights_only = TRUE,
  verbose = 1
)
## ==================================================================
## train model
model %>% fit(x = x.train,
                         y = y.train,
                         batch_size = batch.size,
                         epochs = 10,
                         callbacks = list(cp_callback),
                         view_metrics = TRUE,
                         validation_data = list(x.test, y.test),
                         shuffle = TRUE)
## ==================================================================
## save model
model %>% save_model_hdf5(filepath=paste0(checkpoint_dir,"/lstm01.h5"))
## load (optimal) model weights
model %>% load_model_weights_hdf5(filepath=paste0(checkpoint_dir,"/weights.12-0.13.hdf5"))

## ==================================================================
## do prediction
model %>% evaluate(x.test, y.test, batch_size=batch.size)
pred.test  <- model %>% predict(x.test, y.test, batch_size=batch.size)
pred.train <- model %>% predict(x.train, y.train, batch_size=batch.size)
dat.pred <- data.frame(obs=c(y.test,y.train), pred=c(pred.test,pred.train), testtrain=c(rep("test",length(y.test)),rep("train",length(y.train))),
                       rw=1:(length(y.test)+length(y.train))) %>% gather(key="key",value = "value",-rw,-testtrain)
dat.pred <- dat.pred %>%mutate(value=unscale.log(list(y=value,scl=data$scl)))

## ==================================================================
## plot prediction
gg <- ggplot() + theme_bw() + labs(y="Discharge (mm/d)", x="Time", color="") + theme(text = element_text(size=16))
gg.train <- gg + geom_line(aes(x=rw, y=value, color=key), data=dat.pred%>%filter(testtrain=="train")) + labs(caption="calibration")
plot(gg.train)
gg.test <- gg + geom_line(aes(x=rw, y=value, color=key), data=dat.pred%>%filter(testtrain=="test")) + labs(caption="validation")
plot(gg.test)

## construct LSTM plus catchment properties NN
batch.size <- 10
inp.ts <- layer_input(batch_shape = c(batch.size, lags+1, ncol(forc1)))
mylstm <- inp.ts %>% layer_lstm(units = 100,
                                batch_size = batch.size,
                                return_sequences = TRUE,
                                stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5)
catch.prop <- layer_input(batch_shape=c(batch.size, ncol(prop)), name="catch.prop")
output <- layer_concatenate(list(mylstm, catch.prop)) %>% layer_dense(units = 1)
model <- keras_model(inputs = c(inp.ts, catch.prop), outputs = output) %>% compile(loss = 'mse', optimizer = 'adam', metrics='accuracy')


