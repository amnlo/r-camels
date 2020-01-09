read.strmflw <- function(pth){
  wd.curr <- getwd()
  setwd(pth)
  dirs <- list.dirs(recursive=FALSE)
  setwd(wd.curr)
  strmflw <- list()
  i <- 1
  for(dir.curr in dirs){
    fils <- list.files(paste0(pth,dir.curr))
    for(fil.curr in fils){
      strm.curr <- read.table(paste0(pth,dir.curr,"/",fil.curr), header=FALSE)
      strmflw[[substr(fil.curr, 1, 8)]] <- strm.curr[,-1]
      i <- i + 1
    }
  }
  return(strmflw)
}