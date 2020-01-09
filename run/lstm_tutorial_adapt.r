## https://www.r-bloggers.com/lstm-with-keras-tensorflow/

library(BatchGetSymbols)
library(plotly)
library(tensorflow)
library(keras)

tickers <- c('%5EIBEX')
first.date <- Sys.Date() - 360*15
last.date <- Sys.Date()

myts <- BatchGetSymbols(tickers = tickers,
                        first.date = first.date,
                        last.date = last.date,
                        cache.folder = file.path(tempdir(),
                                                 'BGS_Cache') ) # cache in tempdir()
class(myts)
length(myts)
str(myts)

y = myts$df.tickers$price.close
myts = data.frame(index = myts$df.tickers$ref.date, price = y, vol = myts$df.tickers$volume)
myts = myts[complete.cases(myts), ]
myts = myts[-seq(nrow(myts) - 3000), ]
myts$index = seq(nrow(myts))

plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol)
acf(myts$price, lag.max = 3000)

msd.price = c(mean(myts$price), sd(myts$price))
msd.vol = c(mean(myts$vol), sd(myts$vol))
myts$price = (myts$price - msd.price[1])/msd.price[2]
myts$vol = (myts$vol - msd.vol[1])/msd.vol[2]
summary(myts)

datalags = 10
train = myts[seq(2000 + datalags), ]
test = myts[2000 + datalags + seq(1000 + datalags), ]
batch.size = 10

x.train = array(data = lag(cbind(train$price, train$vol), datalags)[-(1:datalags), ], dim = c(nrow(train) - datalags, datalags, 2)) # the 3rd dimention of x.train are the features
y.train = array(data = train$price[-(1:datalags)], dim = c(nrow(train)-datalags, 1))

x.test = array(data = lag(cbind(test$vol, test$price), datalags)[-(1:datalags), ], dim = c(nrow(test) - datalags, datalags, 2))
y.test = array(data = test$price[-(1:datalags)], dim = c(nrow(test) - datalags, 1))

inp.ts <- layer_input(batch_shape = c(batch.size, datalags, 2))
mylstm <- inp.ts %>% layer_lstm(units = 100,
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5)

## add another layer that accepts different input and concatenate the two models
catch.prop <- layer_input(batch_shape=c(batch.size, 10), name="catch.prop")
output <- layer_concatenate(list(mylstm, catch.prop)) %>% layer_dense(units = 1)
## add also the input needed for the new, concatenated model
x.train <- list(x.train, array(data = rnorm(nrow(x.train)*10), dim = c(nrow(x.train),10)))
x.test <- list(x.test, array(data = rnorm(nrow(x.test)*10), dim = c(nrow(x.test),10)))

model <- keras_model(inputs = c(inp.ts, catch.prop), outputs = output) %>% compile(loss = 'mae', optimizer = 'adam')

history <- model %>% fit(x = x.train,
              y = y.train,
              batch_size = batch.size,
              epochs = 100,
              shuffle = FALSE)

pred_out <- model %>% predict(x.test, batch_size = batch.size) %>% .[,1]

plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol) %>%
  add_trace(y = c(rep(NA, 2000), pred_out), x = myts$index, name = "LSTM prediction", mode = "lines")
