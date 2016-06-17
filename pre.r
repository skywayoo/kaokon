getwd()
setwd("~/kaokon/M05A/")
library(stringr)
options(stringsAsFactors = FALSE)

#2015/06/08 to 2016/06/08
folders <- list.files(path = "20150608-20160608/", pattern="*",full.names = T)
hr <- c("00","01","02","03","04","05","06","07","08","09","10",11:23)
filenames <- list.files(path = paste(folders,"/",hr,sep=""), pattern="*",full.names = T)
ldf <- lapply(filenames,function(i){
  read.csv(i, header=FALSE)
})
#precessing M05A
process_fun <- function(ldf){
data <- data.frame()
value <- list()
new_value <- list()
for(i in 1:length(ldf)){
cat("processing",filenames[i],"--------------------->", round((100-(100-(i/length(ldf)*100))),2),"%","\n")
time <- str_extract(ldf[[i]][,1],"[0-9]+:[0-9]+")
ldf[[i]][,1] = time
idx <- c(1:length(ldf[[i]][,1]))
start <- seq(1,length(ldf[[i]][,1]),by=5)
end <- seq(5,length(ldf[[i]][,1]),by=5)
  for(n in 1:length(start)){
    value[[n]] <- ldf[[i]][start[n]:end[n],]
  } #end start end index
  for(k in 1:length(value)){
    dat <- as.matrix(value[[k]])
    new_value[[k]] <- c(as.vector(t(dat)[,1][1:6]),as.vector(t(dat)[,2][4:6]),as.vector(t(dat)[,3][4:6]),
                        as.vector(t(dat)[,4][4:6]),as.vector(t(dat)[,5][4:6]))
  } #end new_value
  for(m in 1:length(new_value)){
    data <- rbind(data,new_value[[m]])
  }
  names(data) <- c("time","from","to","car_31","speed_31","vol_31","car_32","speed_32","vol_32","car_41","speed_41","vol_41",
                   "car_42","speed_42","vol_42","car_5","speed_5","vol_5")
} #end ldf

return(data)
}

M05A <- process_fun(ldf)
