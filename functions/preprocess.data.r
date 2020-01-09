preprocess.data <- function(all.data, lags=60, n.catchments=10){
  set.seed(9)
  catch.ids <- all.data$attrib$gauge_id[sample(1:nrow(all.data$attrib), size=n.catchments, replace=F)]
  all.data.small <- list(strmflw = all.data$strmflw[which(as.numeric(names(all.data$strmflw)) %in% catch.ids)],
                         forcing = all.data$forcing[which(as.numeric(names(all.data$forcing)) %in% catch.ids)])
  ## preprocess forcing data
  forc <- rbindlist(all.data.small$forcing, idcol = "gauge_id") # spread the list into a data frame
  forc <- forc %>% select(-dayl.s., -swe.mm.)
  forc <- forc %>% mutate(gauge_id = as.numeric(gauge_id))
  forc <- forc %>% mutate(prcp.mm.day. = ((prcp.mm.day.+ 0.1)^0.2 - 1) / 0.2) # Box-Cox transformation
  forc <- forc %>% mutate(avg.temp = (tmax.C. + tmin.C.) /2) %>% mutate(avg.temp = (avg.temp-min(avg.temp))/(max(avg.temp)-min(avg.temp)))
  forc <- forc %>% mutate(srad.W.m2. = scale_this(srad.W.m2.)$x)
  forc <- forc %>% mutate(vp.Pa. = scale_this(vp.Pa.)$x)
  forc <- forc %>% select(-tmin.C., -tmax.C.)
  tmp <- lapply(X = 1:(nrow(forc)-lags), FUN = function(x,data) data[x:(x+lags-1),], data=forc)
  tmp <- array(unlist(tmp), dim=c(lags,ncol(forc),length(tmp)))
  dimnames(tmp)[[2]] <- colnames(forc)
  dat <- aperm(tmp, c(3,1,2))
  
  ## preprocess streamflow data
  strm <- rbindlist(all.data.small$strmflw, idcol="gauge_id")
  strm <- strm %>% mutate(gauge_id = as.numeric(gauge_id))
  strm <- strm %>% select(-V6)
  colnames(strm)[2:5] <- c("Year","Mnth","Day","Q")
  
  ## preprocess catchment property data
  notneed <- c("huc_02", "gauge_name", "gauge_lat", "gauge_lon", "area_geospa_fabric") # unwanted column names
  prop <- all.data.small$attrib %>% select(-notneed)
  
  
  return(data)
}