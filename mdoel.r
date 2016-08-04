rm(list = ls())
setwd("~/0801/高公局ETC檔案/路徑1台北到高雄/")
require(mxnet)
library(cvTools)
#read data
ldf = lapply(sprintf("f_dat%s.csv",1:8), function(x){
  read.csv(x)
})
cv_best = c(9,6,9,2,9,10,5)

#[1]max 24 mape,[2]cv_pred,[3]cv_label,[4]model 
cv_best_model <- function(ldf,day,cv_best){
  cv_best = cv_best[day]
  cat("Day=",day,"------cv_best=",cv_best,"\n")
  dat = ldf[[day]]
  for(i in 2:length(dat)){
    dat[,i]=as.numeric(dat[,i]) 
  }
  dat =dat [-1]
  #create folds
  set.seed(5487)
  cvfold <- cvFolds(length(dat[,1]),K = 10,type = "random")
  #each fold
  index_1 <- cvfold$subsets[cvfold$which==1,]
  index_2 <- cvfold$subsets[cvfold$which==2,]
  index_3 <- cvfold$subsets[cvfold$which==3,]
  index_4 <- cvfold$subsets[cvfold$which==4,]
  index_5 <- cvfold$subsets[cvfold$which==5,]
  index_6 <- cvfold$subsets[cvfold$which==6,]
  index_7 <- cvfold$subsets[cvfold$which==7,]
  index_8 <- cvfold$subsets[cvfold$which==8,]
  index_9 <- cvfold$subsets[cvfold$which==9,]
  index_10 <- cvfold$subsets[cvfold$which==10,]
  
  cv1 <- dat[index_1,] 
  cv2 <- dat[index_2,] 
  cv3 <- dat[index_3,] 
  cv4 <- dat[index_4,] 
  cv5 <- dat[index_5,] 
  cv6 <- dat[index_6,] 
  cv7 <- dat[index_7,] 
  cv8 <- dat[index_8,] 
  cv9 <- dat[index_9,] 
  cv10 <- dat[index_10,] 
  cv_all = list(cv1,cv2,cv3,cv4,cv5,
                cv6,cv7,cv8,cv9,cv10)
  
  cv_model = function(dat){
    mape_fun <- function(label,pred){
      res = vector()
      for(i in 1:length(label)){
        res[i] = (label[i]-pred[i])/label[i]
      }
      return(abs(res))
    }
    maxs <- apply(dat,2,max) 
    mins <- apply(dat,2,min)
    train.x <- data.matrix(scale(dat,center=mins,scale=maxs-mins))[,-6]
    train.y <- data.matrix(scale(dat,center=mins,scale=maxs-mins))[,6]
    data <- mx.symbol.Variable("data")
    fc1 <- mx.symbol.FullyConnected(data,num_hidden=200)
    act1 <- mx.symbol.Activation(fc1,name="relu1",act_type="relu")
    fc2 <- mx.symbol.FullyConnected(act1,num_hidden=200)
    act2 <- mx.symbol.Activation(fc2,name="relu2",act_type="relu")
    fc3 <- mx.symbol.FullyConnected(act2,num_hidden=1)
    lro <- mx.symbol.LinearRegressionOutput(fc3)
    mx.set.seed(0)
    #288 0.02
    model <- mx.model.FeedForward.create(lro,X=train.x,y=train.y,ctx=mx.gpu(),num.round=5000,array.batch.size = 100, 
                                         learning.rate=0.05,momentum=0.9,eval.metric=mx.metric.mae)
    pred = predict(model,train.x,ctx = mx.gpu())
    pred = round(as.vector(pred)*(max(dat$X0)-min(dat$X0))+min(dat$X0),2)
    label <- as.vector(train.y*(max(dat$X0)-min(dat$X0))+min(dat$X0))
    return(list(sort(mape_fun(label,pred),decreasing = T)[1:24],pred,label,model))
  }
  cv_result = cv_model(cv_all[[cv_best]])
  return(cv_result)
}
mon = cv_best_model(ldf,1,cv_best)
tues = cv_best_model(ldf,2,cv_best)
wend = cv_best_model(ldf,3,cv_best)
thur = cv_best_model(ldf,4,cv_best)
fri = cv_best_model(ldf,5,cv_best)
sat = cv_best_model(ldf,6,cv_best)
sun = cv_best_model(ldf,7,cv_best)

thur[1]
################################## predict model###############################
day_val = list()
new_dat = list()
week_idx = rep(1:7,1000)
#input first=預測第一天禮拜幾 ,end=預測幾天7+X-1? test=當下的前五筆+目前的值
first=7
end=7+30
length(week_idx[first:end])
test= unlist(c(ldf[[6]][11232,2:7]))
v=0
for(d in week_idx[first:end]){
  v=v+1
  cat(d,"\n")
  if(d==1){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history <- history_all[284:571,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(mon[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[d]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
  if(d==2){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history =history_all[1:288,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(tues[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[v]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
  if(d==3){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history =history_all[1:288,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(wend[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[v]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
  if(d==4){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history =history_all[1:288,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(thur[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[v]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
  if(d==5){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history =history_all[1:288,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(fri[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[v]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
  if(d==6){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history =history_all[1:288,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(sat[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[v]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
  if(d==7){
    all = ldf[[d]][,-1]
    prev_6 = rbind(all[,1:6],test)
    maxs <- apply(prev_6,2,max) 
    mins <- apply(prev_6,2,min)
    prev_6_dat <- data.matrix(scale(prev_6,center=mins,scale=maxs-mins))
    hist_dat = all
    maxs_his <- apply(hist_dat,2,max) 
    mins_his <- apply(hist_dat,2,min)
    history_all <- data.matrix(scale(hist_dat,center=mins_his,scale=maxs_his-mins_his))
    history =history_all[1:288,]
    pred=list()
    for(i in 1:288){
      hist_val = history[i,c(7:length(history[1,]))]
      test.x = data.matrix(t(as.matrix(c(prev_6_dat[length(ldf[[d]][,1])+i,][-1],hist_val))))
      pred[[i]] = predict(sun[[4]],test.x,ctx = mx.gpu())
      if(pred[[i]]>1)(pred[[i]]=1)
      prev_6_dat = rbind(prev_6_dat,c(prev_6_dat[length(ldf[[d]][,1])+i,c(-1)],pred[[i]]))
    }
    pred_normal = round(unlist(pred)*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2)
    new_dat[[v]] = prev_6_dat[length(ldf[[d]][,1]):length(prev_6_dat[,1]),1:6]
    day_val[[v]] = pred_normal
    test = c(round(prev_6_dat[length(prev_6_dat[,1]),1]*(max(prev_6$X5)-min(prev_6$X5))+min(prev_6$X5),2),
             round(prev_6_dat[length(prev_6_dat[,1]),2]*(max(prev_6$X4)-min(prev_6$X4))+min(prev_6$X4),2),
             round(prev_6_dat[length(prev_6_dat[,1]),3]*(max(prev_6$X3)-min(prev_6$X3))+min(prev_6$X3),2),
             round(prev_6_dat[length(prev_6_dat[,1]),4]*(max(prev_6$X2)-min(prev_6$X2))+min(prev_6$X2),2),
             round(prev_6_dat[length(prev_6_dat[,1]),5]*(max(prev_6$X1)-min(prev_6$X1))+min(prev_6$X1),2),
             round(prev_6_dat[length(prev_6_dat[,1]),6]*(max(prev_6$X0)-min(prev_6$X0))+min(prev_6$X0),2))
  }
}
plot(unlist(day_val),t='l')
lines(x=1:length(label),
      y=label,col="red")
day = c(sprintf("2016-05-0%s",1:9),sprintf("2016-05-%s",10:31))
get_label = function(first,end,day){
  val=list()
  v=0
  for(i in week_idx[first:end]){
    v=v+1
    val[[v]] = c(ldf[[i]][which(ldf[[i]][,1] == day[v]),7])
    cat(val[[v]],"\n")
  }
  return(unlist(val))
}
label =get_label(7,37,day)
mape_fun <- function(label,pred){
  res = vector()
  for(i in 1:length(label)){
    res[i] = (label[i]-pred[i])/label[i]
  }
  return(abs(res))
}

m = mape_fun(label,unlist(day_val))
sort(m,decreasing = T)[1:24]
