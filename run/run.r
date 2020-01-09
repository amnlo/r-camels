# this is the file that runs the general analysis
library(dplyr)
library(tidyr)
library(tensorflow)
library(keras)
library(ggplot2)



source("../functions/read.catchment.attributes.r")
source("../functions/read.forcing.r")
source("../functions/read.strmflw.r")

scale_this <- function(x){
  mn <- mean(x, na.rm=TRUE)
  sd <- sd(x, na.rm=TRUE)
  x.new <- (x - mn) / sd
  return(list(x=x.new, center=mn, scale=sd))
}

# pth <- "../camels_attributes_v2_0/camels_attributes_v2_0/data/"
# attrib <- read.catchment.attributes(pth)
# pth <- "../basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/usgs_streamflow/"
# strmflw <- read.strmflw(pth)
# pth <- "../basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/basin_mean_forcing/daymet"
# forcing <- read.forcing(pth)
# all.data <- list(attrib=attrib, strmflw=strmflw, forcing=forcing)
# save(all.data, file="../all_data.RData")
load("../all_data.RData")

## reshape forcing data into nice format
dims <- list()
n.row <- 0
n.col <- 0
for(forc in names(all.data$forcing)[1:2]){
  dims[[forc]] <- dim(all.data$forcing[[forc]])
  n.row <- n.row + dims[[forc]][1]
  n.col <- max(n.col, dims[[forc]][2])
}
forc.dat <- matrix(0.0, nrow=n.row, ncol=n.col)
i <- 1
for(curr in dims[1:2]){
  if(i==1){
    print(curr)
    forc.dat[1:(curr[1]),1:n.col] <- as.numeric(as.matrix(all.data$forcing[[i]]))
    row.curr <- curr[1]+1
  }
  else{
    print(curr)
    forc.dat[row.curr:(row.curr+curr[1]-1),1:n.col] <- as.numeric(as.matrix(all.data$forcing[[i]]))
    row.curr <- row.curr + curr[1]
  }
  i <- i+1
}
colnames(forc.dat) <- colnames(all.data$forcing[[1]])

## put catchment properties data into nice format
prop <- all.data$attrib
notneed <- c("huc_02", "gauge_name", "gauge_lat", "gauge_lon", "area_geospa_fabric") # unwanted column names
prop <- prop %>% select(-notneed)

## prepare data for 1 catchment (as example of data format needed)
id <- "01013500"
forc1.orig <- all.data$forcing[[id]] %>% select(-Year, -Mnth, -Day, -Hr, -dayl.s., -swe.mm.)
forc1 <- forc1.orig
forc1 <- forc1 %>% mutate(prcp.mm.day. = ((prcp.mm.day.+ 0.1)^0.2 - 1) / 0.2) # Box-Cox transformation
forc1 <- forc1 %>% mutate(avg.temp = (tmax.C. + tmin.C.) /2) %>% mutate(avg.temp = (avg.temp-min(avg.temp))/(max(avg.temp)-min(avg.temp)))
forc1 <- forc1 %>% mutate(srad.W.m2. = scale_this(srad.W.m2.)$x)
forc1 <- forc1 %>% mutate(vp.Pa. = scale_this(vp.Pa.)$x)
forc1 <- forc1 %>% select(-tmin.C., -tmax.C.)
lags <- 60
tmp <- lapply(X = 1:(nrow(forc1)-lags), FUN = function(x,data) data[x:(x+lags-1),], data=forc1)
tmp <- array(unlist(tmp), dim=c(lags,ncol(forc1),length(tmp)))
dimnames(tmp)[[2]] <- colnames(forc1)
dat <- aperm(tmp, c(3,1,2))
y <- all.data$strmflw[[id]] %>% select(V5) %>% slice((lags+1):nrow(.))
y <- as.numeric(y$V5)
rm <- which(y != -999)
y <- y[rm]
scld <- scale_this(y)
y <- scld$x
dat <- dat[rm,,]

batch.size <- 20
validation_split <- 0.2
even.train <- ((nrow(dat)*(1-validation_split))%/%batch.size)*batch.size
even.test  <- ((nrow(dat)*validation_split)%/%batch.size)*batch.size
x.train <- dat[1:even.train,,]
x.test <-  dat[(even.train+1):(even.train+even.test),,]
y.train <- y[1:even.train]
y.test  <- y[(even.train+1):(even.train+even.test)]

## construct LSTM for one catchment
model <- keras_model_sequential()
model %>% layer_lstm(units = lags,
             input_shape = c(lags, ncol(forc1)),
             batch_size = batch.size,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 50,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)
model %>% compile(loss = 'mse', optimizer = 'adam', metrics='accuracy')

checkpoint_dir <- "checkpoints"
filepath <- file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
# Create checkpoint callback
cp_callback <- callback_model_checkpoint(
  filepath = filepath,
  save_weights_only = TRUE,
  verbose = 1
)
model %>% fit(x = x.train,
                         y = y.train,
                         batch_size = batch.size,
                         epochs = 5,
                         callbacks = list(cp_callback),
                         view_metrics = TRUE,
                         validation_data = list(x.test, y.test),
                         shuffle = FALSE)
model %>% evaluate(x.test, y.test, batch_size=batch.size)
pred.test  <- model %>% predict(x.test, y.test, batch_size=batch.size)
pred.train <- model %>% predict(x.train, y.train, batch_size=batch.size)
dat.pred <- data.frame(obs=c(y.test,y.train), pred=c(pred.test,pred.train), testtrain=c(rep("test",length(y.test)),rep("train",length(y.train))),
                       rw=1:(length(y.test)+length(y.train))) %>% gather(key="key",value = "value",-rw,-testtrain)
feet.meter <- 0.3048
gg.train <- ggplot(dat.pred%>%filter(testtrain=="train")%>%mutate(value=(value*scld$scale+scld$center)*feet.meter^3), aes(x=rw, y=value, color=key)) + geom_point()
plot(gg.train)
gg.test <- ggplot(dat.pred%>%filter(testtrain=="test")%>%mutate(value=(value*scld$scale+scld$center)*feet.meter^3), aes(x=rw, y=value, color=key)) + geom_line() + theme_bw() + labs(y=expression("Discharge ("*m^3*"/s)"), x="Time", color="") + theme(text = element_text(size=16))
ggsave("pred_streamflow_validation.png", plot = gg.test, width=8)
plot(gg.test)

## construct LSTM plus catchment properties NN
batch.size <- 10
inp.ts <- layer_input(batch_shape = c(batch.size, lags, ncol(forc1)))
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


