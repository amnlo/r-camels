read.catchment.attributes <- function(pth){
  wd.curr <- getwd()
  setwd(pth)
  fils <- list.files()
  setwd(wd.curr)
  attr <- list()
  i <- 1
  tbl <- data.frame()
  for(fil.curr in fils){
    attr[[i]] <- read.csv(paste0(pth,fil.curr), header=TRUE, sep=";")
    i <- i + 1
  }
  tbl <- attr[[1]]
  for(i in 2:length(attr)){
    tbl <- full_join(tbl,attr[[i]],by="gauge_id")
  }
  return(tbl)
}