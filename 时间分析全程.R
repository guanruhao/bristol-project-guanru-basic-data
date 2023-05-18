
source("C:/Users/10274/Desktop/考虑拥挤程度的分析与作图。/1 乘客信息读取.R")

vector=unlist(data$leave_time)-unlist(data$enter_time)
max_value=max(vector)
max(unlist(data$leave_time))
#该处为自定义参数区
delta_t=60
#第一个为起始站第二个是终点站
#两者必须为邻接站点，以表示有向线路
station1=3
station2=4

time_series=seq(600,2980,by=delta_t)
predict=numeric(length(time_series))
real=numeric(length(time_series))

#link指定器, 两者需要是临接的,并会返回有向的线路编号
link=function(sta1,sta2){
  ind=which(line_frame[,1]==sta1&line_frame[,2]==sta2)
  return(ind)}
target_link=link(station1,station2)
target_link
for(kkkkk in 1:length(time_series)){
  t=time_series[kkkkk]
  
  if(t-max_value<600){start=600}else{start=t-max_value}
  end=t+delta_t
  
  library(progress)
  pb=progress_bar$new(
    format = "[:bar] :percent",
    total =71^2
  )
  #找到对应预测时间前两个多小时到预测时间的乘客情况
  data1=data[which(data$enter_time>=start & data$enter_time<end&data$leave_time>t ),]
  node1=node_path[which(data$enter_time>=start & data$enter_time<end&data$leave_time>t)]
  
  #遍历所有站点并收集路线选择情况
  ori=rep(1:71,each=71)
  des=rep(c(1:71),71)
  #各OD对占总人数的占比
  share=numeric(71^2)
  #各OD对所有路线的边使用情况
  edge_set=rep(list(c(1,1,1)),71^2)
  #各OD对各路线的进入站点时间
  time_set=rep(list(c(1,1,1)),71^2)
  #各OD对各路线各边使用的线路编号
  line_set=rep(list(c(1,1,1)),71^2)
  #各OD对的各路线各边使用的线路编号使用次数
  num_set=rep(list(c(1,1,1)),71^2)
  
  library(progress)
  pb=progress_bar$new(
    format = "[:bar] :percent",
    total =71^2
  )
  
  for(i in 1:71^2){
    pb$tick()
    sequence=which(data1$origin==ori[i] & data1$destination==des[i])
    share[i]=length(sequence)/600000
    edge_set[[i]]=data1[sequence,]$edge_path
    time_set[[i]]=data1[sequence,]$enter_time
    set1=rep(list(c(1,1,1)),length(data1[sequence,]$edge_path))
    set2=rep(list(c(1,1,1)),length(data1[sequence,]$edge_path))
    if(length(data1[sequence,]$edge_path)>0){
      for(j in 1:length(data1[sequence,]$edge_path)){
        ser=line_frame[data1[sequence,]$edge_path[[j]],3]
        set1[[j]]=as.vector(rle(ser)$values)
        set2[[j]]=as.vector(rle(ser)$lengths)}
      line_set[[i]]=set1
      num_set[[i]]=set2}else{
        line_set[[i]]=list(0)
        num_set[[i]]==list(0)}
  }
  
  usage=timed_usage[,c((t-600):(end-600-1))][target_link,]
  if(usage[1]!=0&mean(usage[1:6])<usage[1]){usage=usage[-c(1:6)]}
  aaa=rle(as.vector(usage))$value
  actual=sum(aaa[aaa!=0])
  #将数据精简，留下只通过该边的乘客
  
  data2=data1[which(sapply(data1$edge_path, function(x) target_link %in% x)),]
  node2=node1[which(sapply(data1$edge_path, function(x) target_link %in% x))]
  
  #遍历所有站点并收集路线选择情况
  ori=rep(1:71,each=71)
  des=rep(c(1:71),71)
  #各OD对占总人数的占比
  share=numeric(71^2)
  #各OD对所有路线的边使用情况
  edge_set=rep(list(c(1,1,1)),71^2)
  #各OD对各路线的进入站点时间
  time_set=rep(list(c(1,1,1)),71^2)
  #各OD对各路线各边使用的线路编号
  line_set=rep(list(c(1,1,1)),71^2)
  #各OD对的各路线各边使用的线路编号使用次数
  num_set=rep(list(c(1,1,1)),71^2)
  
  library(progress)
  pb=progress_bar$new(
    format = "[:bar] :percent",
    total =71^2
  )
  
  for(i in 1:71^2){
    pb$tick()
    sequence=which(data2$origin==ori[i] & data2$destination==des[i])
    share[i]=length(sequence)/600000
    edge_set[[i]]=data2[sequence,]$edge_path
    time_set[[i]]=data2[sequence,]$enter_time
    set1=rep(list(c(1,1,1)),length(data2[sequence,]$edge_path))
    set2=rep(list(c(1,1,1)),length(data2[sequence,]$edge_path))
    if(length(data2[sequence,]$edge_path)>0){
      for(j in 1:length(data2[sequence,]$edge_path)){
        ser=line_frame[data2[sequence,]$edge_path[[j]],3]
        set1[[j]]=as.vector(rle(ser)$values)
        set2[[j]]=as.vector(rle(ser)$lengths)}
      line_set[[i]]=set1
      num_set[[i]]=set2}else{
        line_set[[i]]=list(0)
        num_set[[i]]==list(0)}
  }
  #该函数会返回指定乘客经过该link时的时间区间
  calculate=function(i){
    sample=data2[i,]
    index=which(sample$edge_path[[1]]==target_link)
    line_detail=line_frame[sample$edge_path[[1]],3][c(1:index)]
    if(length(rle(line_detail)$values)==1){
      time1=sample$enter_time[[1]]+9+3*(index-1)
      time2=sample$enter_time[[1]]+9+3*index
    }else{time1=sample$enter_time[[1]]+9+9*(length(rle(line_detail)$values)-1)+6*(index-1)
    time2=sample$enter_time[[1]]+9+9*(length(rle(line_detail)$values)-1)+6*index}
    return(c(time1,time2))}
  
  total_number=length(unlist(data2$enter_time))
  
  intervals=matrix(NA,nrow=total_number,ncol=2)
  
  for (i in 1:total_number){
    intervals[i,]=calculate(i)
  }
  
  
  
  interval_intersect <- function(vec1, vec2) {
    start=vec1[1]
    time1=vec2[1]
    time2=vec2[2]
    if(time1<=start& start<time2){x=1}else{x=0}
    return(x)}
  
  logit=numeric(total_number)
  for(i in 1:total_number){
    logit[i]=interval_intersect(as.vector(intervals[i,]),c(t,t+delta_t))}
  predict[kkkkk]=sum(logit)
  real[kkkkk]=actual
}
predict
real

predict-real
mean(abs(predict-real)[c(1:35)])

thelength=35
mean(abs((predict[1:thelength]-real[1:thelength])/real[1:thelength])*100)
predict=predict[1:thelength]
real=real[1:thelength]



