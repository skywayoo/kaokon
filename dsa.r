setwd("../Desktop/高公局ETC檔案/路徑1台北到高雄/")
names(dat_1) = c("time",sprintf("num_%s",5:0),sprintf("week_num_%s",1:48))
ldf = lapply(sprintf("f_dat%s.csv",1:8), function(x){
        read.csv(x)
})

mape_fun <- function(label,pred){
  res = vector()
  for(i in 1:length(label)){
    res[i] = (label[i]-pred[i])/label[i]
  }
        return(abs(res))
}


train_dat = ldf
train_dat[[1]] = train_dat[[1]][-c(c(length(train_dat[[1]][,1])-288*2+1):length(train_dat[[1]][,1])),]
train_dat[[2]] = train_dat[[2]][-c(c(length(train_dat[[2]][,1])-288*2+1):length(train_dat[[2]][,1])),]
train_dat[[3]] = train_dat[[3]][-c(c(length(train_dat[[3]][,1])-288*1+1):length(train_dat[[3]][,1])),]
train_dat[[4]] = train_dat[[4]][-c(c(length(train_dat[[4]][,1])-288*1+1):length(train_dat[[4]][,1])),]
train_dat[[5]] = train_dat[[5]][-c(c(length(train_dat[[5]][,1])-288*1+1):length(train_dat[[5]][,1])),]
train_dat[[6]] = train_dat[[6]][-c(c(length(train_dat[[6]][,1])-288*1+1):length(train_dat[[6]][,1])),]
train_dat[[7]] = train_dat[[7]][-c(c(length(train_dat[[7]][,1])-288*1+1):length(train_dat[[7]][,1])),]

mean_model <- function(ldf,now_day,pred_day){
day_val = list()
new_dat = list()
  for(d in now_day:pred_day%%7){
    cat(d,"\n")
    if(d==1){
      history =ldf[[d]][284:571,]
      dat = ldf[[d]][,-1]
      pred=list()
      for(i in 1:288){
        idx = (length(dat[,1])+i)%%288
        if(idx==0){idx=1}
        prev_val = dat[length(dat[,1]),1:5]
        hist_val = history[idx,c(8:length(history))]
        label_val = dat[length(dat[,1]),6]
        train = data.frame(prev_val,label_val,hist_val)
        pred[[i]] = (rowSums(hist_val)/49+rowSums(prev_val)/5)/2
        dat[length(dat[,1])+1,] = unlist(c(prev_val,pred[[i]],hist_val))
      }
      new_dat[[d]] = dat
      day_val[[d]] = unlist(pred)
    }
    if(d>1){
      history =ldf[[d]]
      dat = ldf[[d]][,-1]
      pred=list()
        for(i in 1:288){
          idx = (length(dat[,1])+i)%%288
          if(idx==0){idx=1}
          prev_val = dat[length(dat[,1]),1:5]
          hist_val = history[idx,c(8:length(history))]
          label_val = dat[length(dat[,1]),6]
          train = data.frame(prev_val,label_val,hist_val)
          pred[[i]] = (rowSums(hist_val)/49+rowSums(prev_val)/5)/2
          dat[length(dat[,1])+1,] = unlist(c(prev_val,pred[[i]],hist_val))
        }
      new_dat[[d]] = dat
      day_val[[d]] = unlist(pred)
      
    }
  }
return(list(day_val,new_dat))
}

test = mean_model(train_dat,1,5)
pred = as.vector(unlist(test[[1]]))
label=list()
label[[1]] = ldf[[1]][c(c(length(ldf[[1]][,1])-288*2+1):c(length(ldf[[1]][,1])-288)),1:10]
label[[2]] = ldf[[2]][c(c(length(ldf[[2]][,1])-288*2+1):c(length(ldf[[2]][,1])-288)),7]
label[[3]] = ldf[[3]][c(c(length(ldf[[3]][,1])-288*1+1):c(length(ldf[[3]][,1]))),7]
label[[4]] = ldf[[4]][c(c(length(ldf[[4]][,1])-288*1+1):c(length(ldf[[4]][,1]))),7]
label[[5]] = ldf[[5]][c(c(length(ldf[[5]][,1])-288*1+1):c(length(ldf[[5]][,1]))),7]
mape_val = mape_fun(as.vector(unlist(label)),pred)
plot(mape_val,t="l")






