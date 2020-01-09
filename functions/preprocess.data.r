preprocess.data <- function(all.data){
  ## preprocess forcing data
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
  
  
  
  return(data)
}