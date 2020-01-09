read.forcing <- function(pth){
  wd.curr <- getwd()
  setwd(pth)
  dirs <- list.dirs(recursive=FALSE)
  setwd(wd.curr)
  forcing <- list()
  i <- 1
  for(dir.curr in dirs){
    print(dir.curr)
    fils <- list.files(paste0(pth,"/",dir.curr))
    for(fil.curr in fils){
      print(fil.curr)
      forcing.curr <- read.table(paste0(pth,dir.curr,"/",fil.curr), header=TRUE, skip=3)
      forcing[[substr(fil.curr, 1, 8)]] <- forcing.curr
    }
    i <- i + 1
  }
  return(forcing)
}