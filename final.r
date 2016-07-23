library(stringr)
#read m04a
m04a = read.csv("../lab/m04a.csv") 
head(m04a) 
#def way5

way_5_number <- c("01F0182S","01F0248S","01F0264S","01F0293S","01F0339S","01F0376S","01F0413S","01F0467S","01F0509S","01F0532S","01F0557S","01F0578S",
  "01F0633S","01F0664S","01F0681S","01F0750S","01F0880S","01F0928S","01F0950S","01F0980S","01F1045S")
#get traveltime
#note that you need put next station 
traveltime <- function(number,day){ 
  sub_m04a = subset(m04a,m04a$from==number[1]) 
  sub_m04a = subset(sub_m04a,sub_m04a$end==number[2])
  for(i in 1:length(sub_m04a[,1])){ 
    sub_m04a[i,14]=(sub_m04a[i,4]*sub_m04a[i,5]+sub_m04a[i,6]*sub_m04a[i,7]+sub_m04a[i,8]*sub_m04a[i,9]+sub_m04a[i,10]*sub_m04a[i,11]+ 
                      sub_m04a[i,12]*sub_m04a[i,13])/(sub_m04a[i,5]+sub_m04a[i,7]+sub_m04a[i,9]+sub_m04a[i,11]+sub_m04a[i,13]) 
  } 
  date <- list() 
  for(i in 1:288){ 
    date[[i]] <- sub_m04a[seq(i,105696,by=288)[seq(day,367,7)],c(1,2,3,4,14)] 
    names(date[[i]]) <- c("time","from","end","car31_time","traveltime") 
  }  
  return(date) 
} 
way_value <- function(way_num){ 
  way_value <- list() 
  day_value <- list() 
  result <- list() 
  for(i in 1:c(length(way_num)-1)){ 
    cat(i,way_num[i],"\n") 
    for(d in 1:7){ 
      idx <- c(way_num[i],way_num[i+1])
      way_value[[i]] <- traveltime(idx,d) 
      day_value[[d]] <- way_value[[i]] 
    } 
    result[[i]] <- day_value 
  } 
  return(result) 
} 
way_5_traveltime <- way_value(way_5_number) 
way_5_traveltime[[10]][[4]][[287]]

#get travel time by day1~7
get_travel = function(way_num,traveltime,week,day){
  dat <- data.frame(1:288)
  for(w in 1:c(length(way_num)-1)){
    value=list()
    for(t in 1:288){
      value[[t]] = floor(traveltime[[w]][[week]][[t]][day,]$traveltime)
    }
    dat[,w] = unlist(value)
  }
  names(dat) <- way_num[-c(length(way_num))]
  return(dat)
}
m04a$car31_time
subset(test,test$end=="01F0557S")
get_travel(way_5_number,way_5_traveltime,4,17)
get_traveltime_byday <- function(way_num,way_traveltime,week){
  days_way  = list()
  for(day in 1:length(way_traveltime[[1]][[week]][[1]][,1])) {
    cat(day,"\n")
    days_way[[day]] = get_travel(way_num,way_traveltime,week,day)
  }
  return(days_way)
}
way5_1_n = get_traveltime_byday(way_5_number,way_5_traveltime,1)
way5_2_n= get_traveltime_byday(way_5_number,way_5_traveltime,2)
way5_3_n= get_traveltime_byday(way_5_number,way_5_traveltime,3)
way5_4_n= get_traveltime_byday(way_5_number,way_5_traveltime,4)
way5_5_n= get_traveltime_byday(way_5_number,way_5_traveltime,5)
way5_6_n= get_traveltime_byday(way_5_number,way_5_traveltime,6)
way5_7_n= get_traveltime_byday(way_5_number,way_5_traveltime,7)
way5_3_n[[17]][,10]
#d2d
billy <- function(way_num,way_head,way_end){
  result<-list()
  head_f <- as.numeric(str_extract(str_extract(way_num[1],"F[0-9]+"),"[0-9]+"))/10
  head_e <- as.numeric(str_extract(str_extract(way_num[2],"F[0-9]+"),"[0-9]+"))/10
  head <- way_head
  head_result <- c((head - head_f)/((head - head_f)+(head_e - head))*100,
                   (head_e - head)/((head - head_f)+(head_e - head))*100)
  result[[1]] <- head_result
  end <- way_end
  end_f <- as.numeric(str_extract(str_extract(way_num[3],"F[0-9]+"),"[0-9]+"))/10
  end_e <- as.numeric(str_extract(str_extract(way_num[4],"F[0-9]+"),"[0-9]+"))/10
  end_result <- c((end - end_f)/((end - end_f)+(end_e - end))*100,(end_e - end)
                  /((end - end_f)+(end_e - end))*100)
  result[[2]] <- end_result
  return(result)
}
d2d <- function(way_4_number,dat){
  travel <- data.frame(1:288)
  travel_list = list()
  for(d in 1:length(dat)){
    cat(d,"\n")
    way_4_number=way_5_number
    for(i in 1:288){
      b1 = billy(c(way_4_number[1:2],way_4_number[2:3]),23.044,25.125)[[1]]/100
      b2 = billy(c(way_4_number[1:2],way_4_number[2:3]),23.044,25.125)[[2]]/100
      b3 = billy(c(way_4_number[3:4],way_4_number[4:5]),27.122,33.039)[[1]]/100
      b4 = billy(c(way_4_number[3:4],way_4_number[4:5]),27.122,33.039)[[2]]/100
      b5 = billy(c(way_4_number[5:6],way_4_number[6:7]),34.4,40.854)[[1]]/100
      b6 = billy(c(way_4_number[5:6],way_4_number[6:7]),34.4,40.854)[[2]]/100
      b7 = billy(c(way_4_number[7:8],way_4_number[8:9]),42.213,49.033)[[1]]/100
      b8 = billy(c(way_4_number[7:8],way_4_number[8:9]),42.213,49.033)[[2]]/100
      b9 = billy(c(way_4_number[9:10],way_4_number[10:11]),52.237,55.115)[[1]]/100
      b10 = billy(c(way_4_number[9:10],way_4_number[10:11]),52.237,55.115)[[2]]/100
      b11 = billy(c(way_4_number[11:12],way_4_number[12:13]),57.321,62.424)[[1]]/100
      b12 = billy(c(way_4_number[11:12],way_4_number[12:13]),57.321,62.424)[[2]]/100
      b13 = billy(c(way_4_number[13:14],way_4_number[14:15]),64.988,67.281)[[1]]/100
      b14 = billy(c(way_4_number[13:14],way_4_number[14:15]),64.988,67.281)[[2]]/100
      b15 = billy(c(way_4_number[15:16],way_4_number[16:17]),69.136,83.758)[[1]]/100
      b16 = billy(c(way_4_number[15:16],way_4_number[16:17]),69.136,83.758)[[2]]/100
      b17 = billy(c(way_4_number[17:18],way_4_number[18:19]),91.023,94.056)[[1]]/100
      b18 = billy(c(way_4_number[17:18],way_4_number[18:19]),91.023,94.056)[[2]]/100
      b19 = billy(c(way_4_number[19:20],way_4_number[20:21]),96.572,99.38)[[1]]/100
      b20 = billy(c(way_4_number[19:20],way_4_number[20:21]),96.572,99.38)[[2]]/100
      #圓山到台北
      travel[i,1] <- round(dat[[d]][i,1]*b1[2]+dat[[d]][i,2]*b2[1])
      #台北到三重
      travel[i,2] <- round(dat[[d]][i,2]*b2[2]+dat[[d]][i,3]*b3[1])
      #三重到五股
      travel[i,3] <- round(dat[[d]][i,3]*b3[2]+dat[[d]][i,4]*b4[1])
      #五股到高工局
      travel[i,4] <- round(dat[[d]][i,4]*b4[2]+dat[[d]][i,5]*b5[1])
      #高工局到林口(文化一路)
      travel[i,5] <- round(dat[[d]][i,5]*b5[2]+dat[[d]][i,6]*b6[1])
      #林口(文化一路)~文化北路
      travel[i,6] <- round(dat[[d]][i,6]*b6[2]+dat[[d]][i,7]*b7[1])
      #文化北路~桃園
      travel[i,7] <- round(dat[[d]][i,7]*b7[2]+dat[[d]][i,8]*b8[1])
      #桃園到機場系統
      travel[i,8] <- round(dat[[d]][i,8]*b8[2]+dat[[d]][i,9]*b9[1])
      #機場系統到中壢服務
      travel[i,9] <- round(dat[[d]][i,9]*b9[2]+dat[[d]][i,10]*b10[1])
      #中壢服務~內壢
      travel[i,10] <- round(dat[[d]][i,10]*b10[2]+dat[[d]][i,11]*b11[1])
      #內壢到中壢
      travel[i,11] <- round(dat[[d]][i,11]*b11[2]+dat[[d]][i,12]*b12[1])
      #中壢到平鎮
      travel[i,12] <- round(dat[[d]][i,12]*b12[2]+dat[[d]][i,13]*b13[1])
      #評鎮到幼獅
      travel[i,13] <- round(dat[[d]][i,13]*b13[2]+dat[[d]][i,14]*b14[1])
      #幼獅到楊沒
      travel[i,14] <- round(dat[[d]][i,14]*b14[2]+dat[[d]][i,15]*b15[1])
      #楊沒到湖口
      travel[i,15] <- round(dat[[d]][i,15]*b15[2]+dat[[d]][i,16]*b16[1])
      #湖口到竹北
      travel[i,16] <- round(dat[[d]][i,16]*b16[2]+dat[[d]][i,17]*b17[1])
      #竹北到新竹(宮到五)
      travel[i,17] <- round(dat[[d]][i,17]*b17[2]+dat[[d]][i,18]*b18[1])
      #宮到五到園區
      travel[i,18] <- round(dat[[d]][i,18]*b18[2]+dat[[d]][i,19]*b19[1])
      #元區到新竹系統
      travel[i,19] <- round(dat[[d]][i,19]*b19[2]+dat[[d]][i,20]*b20[1])
    }
    names(travel) <- c("圓山-台北","台北-三重","三重到五股","五股到高工局","高工局到林口(文化一路)","林口(文化一路)到林口(文化北路)","文化北路到桃園",
                       "桃園到機場系統","機場系統到中壢服務","中壢服務-內壢","內壢到中壢","中壢到平鎮","平鎮-幼獅","幼獅到楊梅","楊梅到湖口","湖口到竹北",
                       "竹北到新竹(公道五)","新竹(公道五)到園區","園區到新竹系統")
    travel_list[[d]] = travel
  }
  return(travel_list)
}
d2d5_1_n = d2d(way_5_number,way5_1_n)
d2d5_1_n[[48]]
d2d5_2_n = d2d(way_5_number,way5_2_n)
d2d5_3_n = d2d(way_5_number,way5_3_n)
d2d5_4_n = d2d(way_5_number,way5_4_n)
d2d5_5_n = d2d(way_5_number,way5_5_n)
d2d5_6_n = d2d(way_5_number,way5_6_n)
d2d5_7_n = d2d(way_5_number,way5_7_n)
d2d5_3_n[[17]][,11]

# na
na_process <- function(dat){
  for(i in 1:length(dat)){
    cat(i)
    for(d in 1:length(dat[[i]])){
      dat[[i]][,d][which(is.nan(dat[[i]][,d]))] = round(sum(dat[[i]][,d][-which(is.nan(dat[[i]][,d]))])/length(dat[[i]][,d][-which(is.nan(dat[[i]][,d]))]))
    }
  }
  return(dat)
}
d2d5_1_n = na_process(d2d5_1_n)
d2d5_2_n = na_process(d2d5_2_n)
d2d5_3_n = na_process(d2d5_3_n)
d2d5_4_n = na_process(d2d5_4_n)
d2d5_5_n = na_process(d2d5_5_n)
d2d5_6_n = na_process(d2d5_6_n)
d2d5_7_n = na_process(d2d5_7_n)

is.nan(d2d5_4_n[[17]][,10])
d2d5_4_n[[17]][,9]=round((round(sum(d2d5_4_n[[16]][,9]))/288+round(sum(d2d5_4_n[[18]][,9])/288))/2)
d2d5_4_n[[17]][,10]=round((round(sum(d2d5_4_n[[16]][,10]))/288+round(sum(d2d5_4_n[[18]][,10])/288))/2)

#list all
way_all = list()
way_all[[1]] = d2d5_1_n
way_all[[2]] = d2d5_2_n
way_all[[3]] = d2d5_3_n
way_all[[4]] = d2d5_4_n
way_all[[5]] = d2d5_5_n
way_all[[6]] = d2d5_6_n
way_all[[7]] = d2d5_7_n

names(test)=c(1:19)
#get final travel
get_d2d_final <- function(way_all,week){
  final_value =list()
  for(day in 1:c(length(way_all[[week]]))){
    v0=vector()
    cat(day,"\n")
    for(i in 1:288){
      travel = way_all[[week]][[day]]
      valid = seq(300,50000,300)
      count=list()
      v=i
      if(week!=7){
        for(t in 1:length(travel)){
          if(v>288){
          travel = way_all[[week+1]][[day]]
          count[[t]]=travel[v-288,t]
          v_num <- sum(unlist(count))
          if(v_num>=valid[1]){
            if(v_num>=valid[which(v_num>=valid)[length(which(v_num>=valid))]]){
              v=v+which(v_num>=valid)[length(which(v_num>=valid))]
              v_idx <- c(1:which(v_num>=valid)[length(which(v_num>=valid))])
              valid=valid[-v_idx]
            }  
          }
        }
          if(v<=288){
          count[[t]]=travel[v,t]
          v_num <- sum(unlist(count))
          if(v_num>=valid[1]){
            if(v_num>=valid[which(v_num>=valid)[length(which(v_num>=valid))]]){
              v=v+which(v_num>=valid)[length(which(v_num>=valid))]
              v_idx <- c(1:which(v_num>=valid)[length(which(v_num>=valid))])
              valid=valid[-v_idx]
            }  
          }
        }
        }
        v0 = c(v0,v_num) 
      }
      if(week==7){
        for(t in 1:length(travel)){
          if(v>288){
          travel = way_all[[1]][[day]]
          count[[t]]=travel[v-288,t]
          v_num <- sum(unlist(count))
          if(v_num>=valid[1]){
            if(v_num>=valid[which(v_num>=valid)[length(which(v_num>=valid))]]){
              v=v+which(v_num>=valid)[length(which(v_num>=valid))]
              v_idx <- c(1:which(v_num>=valid)[length(which(v_num>=valid))])
              valid=valid[-v_idx]
            }  
          }
        }
          if(v<=288){
          count[[t]]=travel[v,t]
          v_num <- sum(unlist(count))
          if(v_num>=valid[1]){
            if(v_num>=valid[which(v_num>=valid)[length(which(v_num>=valid))]]){
              v=v+which(v_num>=valid)[length(which(v_num>=valid))]
              v_idx <- c(1:which(v_num>=valid)[length(which(v_num>=valid))])
              valid=valid[-v_idx]
            }  
          }
        }
      }
        v0 = c(v0,v_num) 
      }
    }
    final_value[[day]] = v0
  }
  return(final_value)
}
#note that hear need 1 2 4 5 6 7 then do #123
f_way_1 = get_d2d_final(way_all,1)
f_way_1[[48]]
f_way_2 = get_d2d_final(way_all,2)
f_way_4 = get_d2d_final(way_all,4)
f_way_5 = get_d2d_final(way_all,5)
f_way_6 = get_d2d_final(way_all,6)
f_way_7 = get_d2d_final(way_all,7)
#way_all[[3]] = way_all[[3]][1:52]
f_way_3 = get_d2d_final(way_all,3)

f_way_7[[1]]




setwd("5圓山到新竹南/")
final_way = t(data.frame(f_way_1))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[1]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"1.csv")

final_way = t(data.frame(f_way_2))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[2]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"2.csv")


final_way = t(data.frame(f_way_3))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[3]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"3.csv")

final_way = t(data.frame(f_way_4))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[4]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"4.csv")

final_way = t(data.frame(f_way_5))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[5]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"5.csv")

final_way = t(data.frame(f_way_6))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[6]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"6.csv")


final_way = t(data.frame(f_way_7))
row.names(final_way)=str_extract(way_5_traveltime[[1]][[7]][[1]]$time,"[0-9]+/[0-9]+/[0-9]+")
head(final_way)
write.csv(final_way,"7.csv")





