preprocess.data <- function(all.data, lags=60, n.catchments=10, prop.separate=TRUE){
  set.seed(9)
  catch.ids <- all.data$attrib$gauge_id[1:n.catchments]
  all.data.small <- list(strmflw = all.data$strmflw[which(as.numeric(names(all.data$strmflw)) %in% catch.ids)],
                         forcing = all.data$forcing[which(as.numeric(names(all.data$forcing)) %in% catch.ids)],
                         attrib  = all.data$attrib %>% filter(gauge_id %in% catch.ids))
  
  ## preprocess catchment property data
  notneed <- c("huc_02", "gauge_name", "gauge_lat", "gauge_lon", "area_geospa_fabric") # unwanted column names
  need <- c("gauge_id","p_mean","pet_mean","p_seasonality","frac_snow","aridity","area_gages2")
  prop <- all.data.small$attrib %>% select(need)
  if(n.catchments>1){
    prop <- prop %>% mutate(area_scld=(area_gages2-min(area_gages2))/(diff(range(area_gages2))))
  }else{
    prop <- prop %>% mutate(area_scld=0)
  }
  
  
  
  ## preprocess forcing data (and include catchment property data)
  forc <- rbindlist(all.data.small$forcing, idcol = "gauge_id") # spread the list into a data frame
  forc <- forc %>% filter(Year>1990 & Year < 2000)
  forc <- forc %>% select(-dayl.s., -swe.mm.)
  # Scale forcing data
  forc <- forc %>% mutate(prcp.mm.day. = ((prcp.mm.day.+ 0.1)^0.2 - 1) / 0.2) # Box-Cox transformation
  forc <- forc %>% mutate(avg.temp = (tmax.C. + tmin.C.) /2) %>% mutate(avg.temp = (avg.temp-min(avg.temp))/(max(avg.temp)-min(avg.temp)))
  forc <- forc %>% mutate(srad.W.m2. = scale_this(srad.W.m2.)$x)
  forc <- forc %>% mutate(vp.Pa. = scale_this(vp.Pa.)$x)
  forc <- forc %>% mutate(gauge_id = as.numeric(gauge_id))
  forc <- forc %>% select(-tmin.C., -tmax.C.) %>% arrange(gauge_id, Year, Mnth, Day)
  forc.l <- list()
  for(c.curr in unique(forc$gauge_id)){
    ind <- which(forc$gauge_id == c.curr)
    if(prop.separate){
      forc.prop <- forc[ind,]      
    }else{
      ## add property data (values are repeated for each time step)
      forc.prop <- cbind(forc[ind,], prop %>% filter(gauge_id==c.curr) %>% select(-area_gages2)) ## remove unscaled area (scaled is still in there)
    }
    tmp <- lapply(X = 1:(length(ind)-lags), FUN = function(x,data) data[x:(x+lags),], data = forc.prop)
    tmp <- array(unlist(tmp), dim=c(lags+1,ncol(forc.prop),length(tmp)))
    dimnames(tmp)[[2]] <- colnames(forc.prop)
    dat <- aperm(tmp, c(3,1,2))
    forc.l[[as.character(c.curr)]] <- dat
  }
  forc.new <- do.call(abind, list(forc.l, along=1))
  forc.new <- forc.new[,,!(dimnames(forc.new)[[3]] %in% c("gauge_id","Year","Mnth","Day","Hr"))]
  
  ## preprocess streamflow data
  strm <- rbindlist(all.data.small$strmflw, idcol="gauge_id")
  strm <- strm %>% mutate(gauge_id = as.numeric(gauge_id))
  strm <- strm %>% select(-V6)
  colnames(strm)[2:5] <- c("Year","Mnth","Day","Q")
  strm <- strm  %>% filter(Year > 1990 & Year < 2000)
  if(any(strm[,"Q"]==-999)) stop("negative streamflow (= NA) encountered")
  if(nrow(strm) != nrow(forc)) warning("streamflow dimension does not agree with forcing data")
  ## make sure streamflow rows correspond to forcing rows
  joint <- merge(forc, strm, by=c("gauge_id","Year","Mnth","Day")) %>% arrange(gauge_id, Year, Mnth, Day)
  joint <- merge(joint, prop %>% select(gauge_id, area_gages2), by="gauge_id") %>% mutate(Q=Q/area_gages2) %>% select(-area_gages2)
  strm <- joint %>% select(gauge_id, Q)
  for(c.curr in unique(strm$gauge_id)){
    ind <- which(strm$gauge_id==c.curr)
    strm <- strm %>% slice(-ind[1:lags])
  }
  strm <- strm %>% select(Q)
  ## scale streamflow data
  tmp <- scale.log(strm[,1])
  strm[,1] <- tmp$y
  
  data <- list(x=forc.new, y=strm, scl=tmp$scl)
  return(data)
}